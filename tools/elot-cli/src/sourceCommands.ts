// src/sourceCommands.ts
//
// Step 2.3.4: VS Code commands for managing the user's
// `elot.activeLabelSources` list via QuickPicks.
//
//   - elot.activateSource    : multi-select inactive sources, append
//   - elot.deactivateSource  : multi-select active sources, remove
//   - elot.reorderActiveSources : pick item, pick action, persist
//
// Settings round-trip:
//   We update at workspace scope if a workspace folder is open; else
//   user (global) scope.  The original scope of the key (if any) is
//   preserved -- we don't silently move a workspace setting up to
//   user.

import * as vscode from "vscode";
import type { ElotDbBridge } from "./db/bridge.js";
import { normalizeActiveSources } from "./activeSources.js";
import {
  SourceKey,
  appendUnique,
  removeByKeys,
  inactiveOf,
} from "./sourceCommands/reorder.js";
import { reorderWithButtons } from "./sourceCommands/reorderUx.js";

const SETTING_KEY = "activeLabelSources";

/** Decide which scope to write to, preserving any existing scope. */
function targetForUpdate(): vscode.ConfigurationTarget {
  const cfg = vscode.workspace.getConfiguration("elot");
  const ins = cfg.inspect<unknown>(SETTING_KEY);
  if (ins?.workspaceFolderValue !== undefined)
    return vscode.ConfigurationTarget.WorkspaceFolder;
  if (ins?.workspaceValue !== undefined)
    return vscode.ConfigurationTarget.Workspace;
  if (ins?.globalValue !== undefined) return vscode.ConfigurationTarget.Global;
  // Default: workspace if a folder is open, else global.
  return vscode.workspace.workspaceFolders &&
    vscode.workspace.workspaceFolders.length > 0
    ? vscode.ConfigurationTarget.Workspace
    : vscode.ConfigurationTarget.Global;
}

function scopeLabel(t: vscode.ConfigurationTarget): string {
  switch (t) {
    case vscode.ConfigurationTarget.WorkspaceFolder:
      return "workspace folder";
    case vscode.ConfigurationTarget.Workspace:
      return "workspace";
    default:
      return "user";
  }
}

/**
 * Persist the new active list as plain objects.  We always write the
 * normalised `{source, dataSource}` shape (not the bare-string
 * shorthand) so settings.json stays unambiguous after an edit.
 */
async function persistActive(next: SourceKey[]): Promise<vscode.ConfigurationTarget> {
  const cfg = vscode.workspace.getConfiguration("elot");
  const target = targetForUpdate();
  const payload = next.map((s) => ({
    source: s.source,
    dataSource: s.dataSource ?? "",
  }));
  await cfg.update(SETTING_KEY, payload, target);
  return target;
}

function readActive(): SourceKey[] {
  const cfg = vscode.workspace.getConfiguration("elot");
  return normalizeActiveSources(cfg.get<unknown>(SETTING_KEY)).map((s) => ({
    source: s.source,
    dataSource: s.dataSource ?? "",
  }));
}

function describe(s: SourceKey): vscode.QuickPickItem & { key: SourceKey } {
  return {
    label: s.source,
    description: s.dataSource ? s.dataSource : undefined,
    key: s,
  };
}

// ---- elot.activateSource ----------------------------------------

async function activateSource(bridge: ElotDbBridge): Promise<void> {
  const db = await bridge.get();
  if (!db) {
    vscode.window.showWarningMessage(
      "ELOT: label DB is not loaded. Use `elot-cli db register ...` first.",
    );
    return;
  }
  const allRows = db.listSources();
  const all: SourceKey[] = allRows.map((r) => ({
    source: r.source,
    dataSource: r.dataSource ?? "",
  }));
  if (all.length === 0) {
    vscode.window.showInformationMessage(
      "ELOT: the database is empty. Register a source with `elot-cli db register ...`.",
    );
    return;
  }
  const active = readActive();
  const inactive = inactiveOf(all, active);
  if (inactive.length === 0) {
    vscode.window.showInformationMessage(
      "ELOT: all registered sources are already active.",
    );
    return;
  }
  const picked = await vscode.window.showQuickPick(
    inactive.map(describe),
    {
      title: "ELOT: Activate label source(s)",
      placeHolder: "Select one or more sources to activate",
      canPickMany: true,
      matchOnDescription: true,
    },
  );
  if (!picked || picked.length === 0) return;
  const next = appendUnique(active, picked.map((p) => p.key));
  const target = await persistActive(next);
  vscode.window.showInformationMessage(
    `ELOT: activated ${picked.length} source${picked.length === 1 ? "" : "s"} (${scopeLabel(target)} settings).`,
  );
}

// ---- elot.deactivateSource --------------------------------------

async function deactivateSource(): Promise<void> {
  const active = readActive();
  if (active.length === 0) {
    vscode.window.showInformationMessage(
      "ELOT: no active sources. Use `Elot: Activate label source` to add some.",
    );
    return;
  }
  const picked = await vscode.window.showQuickPick(
    active.map(describe),
    {
      title: "ELOT: Deactivate label source(s)",
      placeHolder: "Select one or more sources to deactivate",
      canPickMany: true,
      matchOnDescription: true,
    },
  );
  if (!picked || picked.length === 0) return;
  const next = removeByKeys(active, picked.map((p) => p.key));
  const target = await persistActive(next);
  vscode.window.showInformationMessage(
    `ELOT: deactivated ${picked.length} source${picked.length === 1 ? "" : "s"} (${scopeLabel(target)} settings).`,
  );
}

// ---- elot.reorderActiveSources ----------------------------------
//
// 2.3.6: per-item-button picker.  All moves happen in a single
// QuickPick; persist once when the user accepts.

async function reorderActiveSources(): Promise<void> {
  const active = readActive();
  if (active.length < 2) {
    vscode.window.showInformationMessage(
      "ELOT: need at least 2 active sources to reorder.",
    );
    return;
  }
  const next = await reorderWithButtons(active);
  if (!next) return;
  const target = await persistActive(next);
  vscode.window.showInformationMessage(
    `ELOT: saved new source order (${scopeLabel(target)} settings).`,
  );
}

// ---- registration -----------------------------------------------

export function registerSourceCommands(
  bridge: ElotDbBridge,
): vscode.Disposable {
  const a = vscode.commands.registerCommand("elot.activateSource", () =>
    activateSource(bridge),
  );
  const d = vscode.commands.registerCommand(
    "elot.deactivateSource",
    deactivateSource,
  );
  const r = vscode.commands.registerCommand(
    "elot.reorderActiveSources",
    reorderActiveSources,
  );
  return vscode.Disposable.from(a, d, r);
}
