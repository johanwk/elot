// src/registerSourceCommand.ts
//
// Step 2.3.6: `elot.registerSource` and `elot.refreshSource` commands.
//
// These are thin wrappers over `elot-cli db register|refresh` -- the
// CLI remains the sole writer (per Stage 2 framing), the extension
// just drives it from a UI flow and reloads the bridge on success.
//
// Flow (registerSource):
//   1. QuickPick a file via showOpenDialog (or paste a remote .rq).
//   2. Detect / pick the source type.
//   3. InputBox source name (default = file stem).
//   4. For type=rq, optionally InputBox a --data-source endpoint/file.
//   5. Spawn `elot-cli db register ...`, stream to "Elot CLI" channel.
//   6. On success: bridge.reload(); offer "Activate now?".
//
// Flow (refreshSource):
//   1. QuickPick from existing sources (live from the bridge).
//   2. Spawn `elot-cli db refresh ...`.
//   3. On success: bridge.reload(); brief toast.

import * as vscode from "vscode";
import { spawn } from "child_process";
import * as path from "path";
import type { ElotDbBridge } from "./db/bridge.js";
import {
  buildRegisterInvocation,
  buildRefreshInvocation,
  defaultSourceNameFromFile,
} from "./cliRunner.js";

const SUPPORTED_TYPES: Array<{ id: string; description: string }> = [
  { id: "csv", description: "Comma-separated; id + label + lang/attrs" },
  { id: "tsv", description: "Tab-separated; same shape as CSV" },
  { id: "json", description: "Flat or nested JSON labels" },
  { id: "ttl", description: "Turtle file via ROBOT (extracts rdfs:label)" },
  { id: "rq", description: "SPARQL query via ROBOT against an RDF data source" },
  { id: "org", description: "ELOT Org ontology" },
];

let outputChannel: vscode.OutputChannel | null = null;
function chan(): vscode.OutputChannel {
  if (!outputChannel) outputChannel = vscode.window.createOutputChannel("Elot CLI");
  return outputChannel;
}

function getCliPath(): string {
  const cfg = vscode.workspace.getConfiguration("elot");
  const v = (cfg.get<string>("cliPath") ?? "").trim();
  return v.length > 0 ? v : "elot-cli";
}

/** Spawn the CLI; resolve to {code, signal}.  Streams output to the channel. */
function runCli(args: string[]): Promise<{ code: number | null; signal: NodeJS.Signals | null }> {
  return new Promise((resolve) => {
    const cli = getCliPath();
    const ch = chan();
    ch.appendLine(`> ${[cli, ...args].join(" ")}`);
    let proc;
    try {
      proc = spawn(cli, args, { shell: false });
    } catch (err) {
      ch.appendLine(`spawn failed: ${(err as Error).message}`);
      resolve({ code: -1, signal: null });
      return;
    }
    proc.stdout.on("data", (b) => ch.append(b.toString()));
    proc.stderr.on("data", (b) => ch.append(b.toString()));
    proc.on("error", (err) => {
      ch.appendLine(`spawn error: ${err.message}`);
    });
    proc.on("close", (code, signal) => {
      ch.appendLine(`(exit ${code ?? "null"}${signal ? `, signal ${signal}` : ""})`);
      resolve({ code, signal });
    });
  });
}

async function pickType(file: string): Promise<string | undefined> {
  const ext = path.extname(file).toLowerCase().replace(/^\./, "");
  const def = SUPPORTED_TYPES.find((t) => t.id === ext)?.id;
  const items = SUPPORTED_TYPES.map((t) => ({
    label: t.id + (t.id === def ? "  (auto)" : ""),
    description: t.description,
    id: t.id,
  }));
  const picked = await vscode.window.showQuickPick(items, {
    title: "ELOT: Choose source type",
    placeHolder: def
      ? `Detected '${def}' from extension; press Enter to confirm or pick another.`
      : "Select the source type",
  });
  return picked?.id;
}

async function activateAfterRegister(source: string): Promise<void> {
  const c = await vscode.window.showInformationMessage(
    `ELOT: registered '${source}'. Activate it now?`,
    "Activate",
    "Not now",
  );
  if (c !== "Activate") return;
  // Append to elot.activeLabelSources at the same scope as existing.
  const cfg = vscode.workspace.getConfiguration("elot");
  const ins = cfg.inspect<unknown>("activeLabelSources");
  const target =
    ins?.workspaceFolderValue !== undefined
      ? vscode.ConfigurationTarget.WorkspaceFolder
      : ins?.workspaceValue !== undefined
        ? vscode.ConfigurationTarget.Workspace
        : ins?.globalValue !== undefined
          ? vscode.ConfigurationTarget.Global
          : vscode.workspace.workspaceFolders &&
              vscode.workspace.workspaceFolders.length > 0
            ? vscode.ConfigurationTarget.Workspace
            : vscode.ConfigurationTarget.Global;
  const current = (cfg.get<unknown[]>("activeLabelSources") ?? []) as unknown[];
  if (current.some((x) =>
    typeof x === "string" ? x === source : (x as any)?.source === source,
  )) {
    return; // already active
  }
  const next = [...current, { source, dataSource: "" }];
  await cfg.update("activeLabelSources", next, target);
}

// ---- registerSource ---------------------------------------------

async function registerSource(bridge: ElotDbBridge): Promise<void> {
  // 1. File selection.
  const ws =
    vscode.workspace.workspaceFolders && vscode.workspace.workspaceFolders[0]
      ? vscode.workspace.workspaceFolders[0].uri
      : undefined;
  const picked = await vscode.window.showOpenDialog({
    canSelectFiles: true,
    canSelectFolders: false,
    canSelectMany: false,
    defaultUri: ws,
    openLabel: "Register",
    title: "ELOT: Pick a source file (CSV / TSV / JSON / TTL / RQ / Org)",
    filters: {
      "Label sources": ["csv", "tsv", "json", "ttl", "rq", "org"],
      "All files": ["*"],
    },
  });
  if (!picked || picked.length === 0) return;
  const file = picked[0].fsPath;

  // 2. Type.
  const type = await pickType(file);
  if (!type) return;

  // 3. Source name.
  const defaultName = defaultSourceNameFromFile(file);
  const sourceName = await vscode.window.showInputBox({
    title: "ELOT: Source name",
    prompt: "Used as the key under which labels are registered.",
    value: defaultName,
    validateInput: (v) =>
      !v || !v.trim() ? "Source name is required" : undefined,
  });
  if (!sourceName) return;

  // 4. Optional --data-source for RQ.
  let dataSource: string | undefined;
  if (type === "rq") {
    dataSource = await vscode.window.showInputBox({
      title: "ELOT: SPARQL data source (rq only)",
      prompt:
        "Local RDF file path or http(s) SPARQL endpoint. Leave empty to omit.",
      value: "",
    });
    if (dataSource === undefined) return; // cancelled
    dataSource = dataSource.trim() || undefined;
  }

  // 5. Run CLI.
  const inv = buildRegisterInvocation(getCliPath(), {
    file,
    source: sourceName.trim(),
    type,
    dataSource: dataSource ?? null,
    dbPath: bridge.path ?? null,
  });
  chan().show(true);
  const res = await vscode.window.withProgress(
    {
      location: vscode.ProgressLocation.Notification,
      title: `ELOT: Registering '${sourceName}'...`,
      cancellable: false,
    },
    () => runCli(inv.args),
  );

  if (res.code === 0) {
    await bridge.reload();
    await activateAfterRegister(sourceName.trim());
  } else if (res.code === -1) {
    vscode.window.showErrorMessage(
      `ELOT: could not launch '${getCliPath()}'.  Set 'elot.cliPath' to the elot-cli binary.`,
    );
  } else {
    vscode.window.showErrorMessage(
      `ELOT: register failed (exit ${res.code}). See the 'Elot CLI' output channel.`,
    );
  }
}

// ---- refreshSource ----------------------------------------------

async function refreshSource(bridge: ElotDbBridge): Promise<void> {
  const db = await bridge.get();
  if (!db) {
    vscode.window.showWarningMessage(
      "ELOT: label DB is not loaded.  Use `Elot: Register Label Source` first.",
    );
    return;
  }
  const sources = db.listSources();
  if (sources.length === 0) {
    vscode.window.showInformationMessage(
      "ELOT: the database is empty.  Use `Elot: Register Label Source` first.",
    );
    return;
  }
  const items = sources.map((s) => ({
    label: s.source,
    description: s.dataSource ? s.dataSource : undefined,
    detail: s.type ? `type: ${s.type}` : undefined,
    src: s,
  }));
  const picked = await vscode.window.showQuickPick(items, {
    title: "ELOT: Refresh source",
    placeHolder: "Re-parse the source from its original file",
  });
  if (!picked) return;

  const inv = buildRefreshInvocation(getCliPath(), {
    source: picked.src.source,
    dataSource: picked.src.dataSource ?? null,
    dbPath: bridge.path ?? null,
  });
  chan().show(true);
  const res = await vscode.window.withProgress(
    {
      location: vscode.ProgressLocation.Notification,
      title: `ELOT: Refreshing '${picked.src.source}'...`,
      cancellable: false,
    },
    () => runCli(inv.args),
  );

  if (res.code === 0) {
    await bridge.reload();
    vscode.window.showInformationMessage(
      `ELOT: refreshed '${picked.src.source}'.`,
    );
  } else if (res.code === -1) {
    vscode.window.showErrorMessage(
      `ELOT: could not launch '${getCliPath()}'.  Set 'elot.cliPath' to the elot-cli binary.`,
    );
  } else {
    vscode.window.showErrorMessage(
      `ELOT: refresh failed (exit ${res.code}). See the 'Elot CLI' output channel.`,
    );
  }
}

// ---- registration -----------------------------------------------

export function registerRegisterSourceCommands(
  bridge: ElotDbBridge,
): vscode.Disposable {
  const reg = vscode.commands.registerCommand("elot.registerSource", () =>
    registerSource(bridge),
  );
  const ref = vscode.commands.registerCommand("elot.refreshSource", () =>
    refreshSource(bridge),
  );
  return vscode.Disposable.from(reg, ref);
}
