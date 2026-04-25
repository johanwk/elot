// src/cliSpawn.ts
//
// Shared helper for spawning the CLI from the extension host.
//
// By default we spawn the *bundled* `dist/cli.js` against the same
// Node binary VS Code's extension host runs on (process.execPath).
// This means the user does NOT need a separately-installed
// `elot-cli` -- the .vsix already ships everything.
//
// If `elot.cliPath` is set to a non-empty string, that takes
// precedence and is spawned as a plain executable.  This is the
// escape hatch for power users who want to point at a development
// checkout or a globally-installed binary.

import * as vscode from "vscode";
import { spawn } from "child_process";
import { existsSync } from "fs";
import * as path from "path";

let extensionPath: string | null = null;

/** Called once from `activate(context)` so we can locate dist/cli.js. */
export function initCliSpawn(ctx: vscode.ExtensionContext): void {
  extensionPath = ctx.extensionPath;
}

/**
 * Resolve which command + leading args to spawn for `elot-cli ...`.
 *
 * Returns { command, leadingArgs, display } where:
 *   - `command` is what to pass as argv[0] to spawn().
 *   - `leadingArgs` are prepended to the user's CLI args.
 *   - `display` is a human-readable rendering ("elot db register ...").
 */
export function resolveCliSpawn(): {
  command: string;
  leadingArgs: string[];
  display: string;
} {
  const cfg = vscode.workspace.getConfiguration("elot");
  const userPath = (cfg.get<string>("cliPath") ?? "").trim();
  if (userPath.length > 0) {
    return { command: userPath, leadingArgs: [], display: userPath };
  }
  // Bundled CLI: <extensionPath>/dist/cli.js, run via the host's node.
  if (extensionPath) {
    const bundled = path.join(extensionPath, "dist", "cli.js");
    if (existsSync(bundled)) {
      return {
        command: process.execPath,
        leadingArgs: [bundled],
        display: "elot",
      };
    }
  }
  // Last-resort fallback: hope something named `elot` is on PATH.
  return { command: "elot", leadingArgs: [], display: "elot" };
}

/**
 * Spawn the CLI with the given user args, streaming stdout/stderr
 * to the provided output channel.  Resolves with {code, signal}.
 */
export function runElotCli(
  userArgs: string[],
  channel: vscode.OutputChannel,
): Promise<{ code: number | null; signal: NodeJS.Signals | null }> {
  return new Promise((resolve) => {
    const { command, leadingArgs, display } = resolveCliSpawn();
    const args = [...leadingArgs, ...userArgs];
    channel.appendLine(`> ${display} ${userArgs.join(" ")}`);
    let proc;
    try {
      proc = spawn(command, args, { shell: false });
    } catch (err) {
      channel.appendLine(`spawn failed: ${(err as Error).message}`);
      resolve({ code: -1, signal: null });
      return;
    }
    proc.stdout.on("data", (b) => channel.append(b.toString()));
    proc.stderr.on("data", (b) => channel.append(b.toString()));
    proc.on("error", (err) => channel.appendLine(`spawn error: ${err.message}`));
    proc.on("close", (code, signal) => {
      channel.appendLine(
        `(exit ${code ?? "null"}${signal ? `, signal ${signal}` : ""})`,
      );
      resolve({ code, signal });
    });
  });
}

/** Diagnostic string for "could not launch CLI" error toasts. */
export function describeCliResolution(): string {
  const { command, leadingArgs } = resolveCliSpawn();
  if (leadingArgs.length > 0) {
    return `bundled CLI: ${command} ${leadingArgs[0]}`;
  }
  return `external: ${command}`;
}
