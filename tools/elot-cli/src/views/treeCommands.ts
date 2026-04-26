// src/views/treeCommands.ts
//
// Step 2.3.7a: per-row and per-view commands wired from the
// TreeView UI.  Most of these are thin adapters that extract the
// SourceKey from the clicked TreeItem and call into existing
// command logic (sourceCommands.ts / registerSourceCommand.ts).
//
// Per-item TreeItem arguments arrive from VS Code with our stashed
// __elotKey property (set in the providers); we read it back here.

import * as vscode from "vscode";
import type { ElotDbBridge } from "../db/bridge.js";
import { normalizeActiveSources } from "../activeSources.js";
import {
  type SourceKey,
  appendUnique,
  removeByKeys,
  moveUp,
  moveDown,
  indexOfKey,
} from "../sourceCommands/reorder.js";
import { buildRefreshInvocation } from "../cliRunner.js";
import { runElotCli, describeCliResolution } from "../cliSpawn.js";
import { ensureRobotJar } from "../robotDownload.js";
import { getEffectiveLanguagePrefs } from "../activeSources.js";
import { insertTokenForId } from "../labelLookup/items.js";
import type { DbLookupView } from "../labelLookup/items.js";

const SETTING_KEY = "activeLabelSources";

function getKey(arg: unknown): SourceKey | null {
  if (!arg || typeof arg !== "object") return null;
  // VS Code passes the element returned from getChildren() (a
  // DbSourceItem / ActiveSourceItem with a `.key` property) when an
  // inline button is clicked from a TreeView - NOT the TreeItem.  We
  // also fall back to __elotKey for callers that pass a TreeItem
  // directly via vscode.commands.executeCommand.
  const direct = (arg as any).key;
  if (direct && typeof direct.source === "string") {
    return { source: direct.source, dataSource: direct.dataSource ?? "" };
  }
  const k = (arg as any).__elotKey;
  if (k && typeof k.source === "string") {
    return { source: k.source, dataSource: k.dataSource ?? "" };
  }
  return null;
}

function readActive(): SourceKey[] {
  const cfg = vscode.workspace.getConfiguration("elot");
  return normalizeActiveSources(cfg.get<unknown>(SETTING_KEY)).map((s) => ({
    source: s.source,
    dataSource: s.dataSource ?? "",
  }));
}

function targetForUpdate(): vscode.ConfigurationTarget {
  const cfg = vscode.workspace.getConfiguration("elot");
  const ins = cfg.inspect<unknown>(SETTING_KEY);
  if (ins?.workspaceFolderValue !== undefined)
    return vscode.ConfigurationTarget.WorkspaceFolder;
  if (ins?.workspaceValue !== undefined)
    return vscode.ConfigurationTarget.Workspace;
  if (ins?.globalValue !== undefined) return vscode.ConfigurationTarget.Global;
  return vscode.workspace.workspaceFolders &&
    vscode.workspace.workspaceFolders.length > 0
    ? vscode.ConfigurationTarget.Workspace
    : vscode.ConfigurationTarget.Global;
}

async function persist(next: SourceKey[]): Promise<void> {
  const cfg = vscode.workspace.getConfiguration("elot");
  const target = targetForUpdate();
  const payload = next.map((s) => ({
    source: s.source,
    dataSource: s.dataSource ?? "",
  }));
  await cfg.update(SETTING_KEY, payload, target);
}

function getCliPathForArgs(): string {
  // Used only as the leading element of `buildRefreshInvocation`'s
  // display string -- the actual spawn goes through runElotCli.
  const cfg = vscode.workspace.getConfiguration("elot");
  const v = (cfg.get<string>("cliPath") ?? "").trim();
  return v.length > 0 ? v : "elot-cli";
}

let cliChannel: vscode.OutputChannel | null = null;
function chan(): vscode.OutputChannel {
  if (!cliChannel) cliChannel = vscode.window.createOutputChannel("Elot CLI");
  return cliChannel;
}

// ---- DB-view actions --------------------------------------------

async function treeActivate(arg: unknown): Promise<void> {
  const key = getKey(arg);
  if (!key) return;
  const next = appendUnique(readActive(), [key]);
  await persist(next);
}

async function treeDeactivate(arg: unknown): Promise<void> {
  const key = getKey(arg);
  if (!key) return;
  const next = removeByKeys(readActive(), [key]);
  await persist(next);
}

async function treeRefresh(
  bridge: ElotDbBridge,
  context: vscode.ExtensionContext,
  arg: unknown,
): Promise<void> {
  const key = getKey(arg);
  if (!key) return;
  // ROBOT auto-download for ttl/rq sources.
  try {
    const db = await bridge.get();
    const src = db?.listSources().find(
      (s) =>
        s.source === key.source &&
        (s.dataSource ?? "") === (key.dataSource ?? ""),
    );
    if (src && (src.type === "ttl" || src.type === "rq")) {
      const robot = await ensureRobotJar(context);
      if (!robot) return;
    }
  } catch {
    /* ignore — let CLI surface the error */
  }
  const inv = buildRefreshInvocation(getCliPathForArgs(), {
    source: key.source,
    dataSource: key.dataSource || null,
    dbPath: bridge.path ?? null,
  });
  chan().show(true);
  const res = await vscode.window.withProgress(
    {
      location: vscode.ProgressLocation.Notification,
      title: `ELOT: Refreshing '${key.source}'...`,
      cancellable: false,
    },
    () => runElotCli(inv.args, chan()),
  );
  if (res.code === 0) {
    await bridge.reload();
    vscode.window.showInformationMessage(
      `ELOT: refreshed '${key.source}'.`,
    );
  } else if (res.code === -1) {
    vscode.window.showErrorMessage(
      `ELOT: could not launch CLI (${describeCliResolution()}).  Set 'elot.cliPath' to override.`,
    );
  } else {
    vscode.window.showErrorMessage(
      `ELOT: refresh failed (exit ${res.code}). See the 'Elot CLI' output channel.`,
    );
  }
}

async function treeDelete(
  bridge: ElotDbBridge,
  arg: unknown,
): Promise<void> {
  const key = getKey(arg);
  if (!key) return;
  const choice = await vscode.window.showWarningMessage(
    `Delete source '${key.source}' from the database?  This removes all its labels and prefixes.`,
    { modal: true },
    "Delete",
  );
  if (choice !== "Delete") return;
  const args = ["db", "remove", key.source];
  if (bridge.path) args.push("--db", bridge.path);
  chan().show(true);
  const res = await vscode.window.withProgress(
    {
      location: vscode.ProgressLocation.Notification,
      title: `ELOT: Removing '${key.source}'...`,
      cancellable: false,
    },
    () => runElotCli(args, chan()),
  );
  if (res.code === 0) {
    // Also drop from active list if present.
    const active = readActive();
    if (indexOfKey(active, key) !== -1) {
      await persist(removeByKeys(active, [key]));
    }
    await bridge.reload();
    vscode.window.showInformationMessage(
      `ELOT: removed '${key.source}'.`,
    );
  } else if (res.code === -1) {
    vscode.window.showErrorMessage(
      `ELOT: could not launch CLI (${describeCliResolution()}).  Set 'elot.cliPath' to override.`,
    );
  } else {
    vscode.window.showErrorMessage(
      `ELOT: remove failed (exit ${res.code}). See the 'Elot CLI' output channel.`,
    );
  }
}

// ---- Active-view actions ----------------------------------------

async function treeBrowseSource(
  bridge: ElotDbBridge,
  arg: unknown,
): Promise<void> {
  const key = getKey(arg);
  if (!key) return;
  const db = await bridge.get();
  if (!db) {
    vscode.window.showWarningMessage("ELOT: DB not loaded.");
    return;
  }
  const cfg = vscode.workspace.getConfiguration("elot");
  const prefs = getEffectiveLanguagePrefs(cfg.get<unknown>("preferredLanguages"));
  const scope = [{ source: key.source, dataSource: key.dataSource ?? "" }];

  const idsByLabel = db.allActiveLabels(scope, prefs);
  if (idsByLabel.size === 0) {
    vscode.window.showInformationMessage(
      `ELOT: source '${key.source}' has no labelled identifiers.`,
    );
    return;
  }

  // Flatten to one item per id.  When a label maps to multiple ids,
  // each gets the id as a disambiguating detail.
  interface Row {
    label: string;
    id: string;
    rdfType: string;
    detail: string;
  }
  const rows: Row[] = [];
  for (const [label, ids] of idsByLabel) {
    const colliding = ids.length > 1;
    for (const id of ids) {
      const rt = db.getAttr(id, "rdf:type", scope, prefs) ?? "";
      let detail = "";
      if (colliding) {
        // Show CURIE if available; else the id itself.
        const c = db.contractUri(id, scope);
        detail = c.length > 0 ? c[0] : id;
      }
      rows.push({ label, id, rdfType: rt, detail });
    }
  }
  rows.sort((a, b) =>
    a.label.localeCompare(b.label, undefined, { sensitivity: "base" }),
  );

  interface QPItem extends vscode.QuickPickItem {
    _id: string;
  }
  const items: QPItem[] = rows.map((r) => ({
    label: r.label,
    description: r.rdfType || undefined,
    detail: r.detail || undefined,
    _id: r.id,
  }));

  const dsHint = key.dataSource ? ` (${key.dataSource})` : "";
  const picked = await vscode.window.showQuickPick(items, {
    title: `ELOT: '${key.source}'${dsHint} - ${rows.length} entities`,
    placeHolder:
      "Type to filter; Enter to insert CURIE/IRI at cursor, Esc to dismiss",
    matchOnDescription: true,
    matchOnDetail: true,
  });
  if (!picked) return;

  const view: DbLookupView = {
    idsByLabel,
    contractUri: (uri) => {
      const c = db.contractUri(uri, scope);
      return c.length > 0 ? c[0] : null;
    },
    labelVariants: (id) => db.labelVariants(id, scope),
    rdfTypeForId: (id) => db.getAttr(id, "rdf:type", scope, prefs),
  };
  const token = insertTokenForId(picked._id, view, null);
  const ed = vscode.window.activeTextEditor;
  if (!ed) {
    // No editor to insert into - copy to clipboard so the action
    // is still useful (e.g. when the user is just browsing).
    await vscode.env.clipboard.writeText(token);
    vscode.window.showInformationMessage(
      `ELOT: copied '${token}' to clipboard (no active editor).`,
    );
    return;
  }
  await ed.edit((b) => {
    for (const sel of ed.selections) {
      if (sel.isEmpty) b.insert(sel.active, token);
      else b.replace(sel, token);
    }
  });
}

// ---- Active-view actions (reorder) ------------------------------

async function treeMoveUp(arg: unknown): Promise<void> {
  const key = getKey(arg);
  if (!key) return;
  const active = readActive();
  const i = indexOfKey(active, key);
  if (i <= 0) return;
  await persist(moveUp(active, i));
}

async function treeMoveDown(arg: unknown): Promise<void> {
  const key = getKey(arg);
  if (!key) return;
  const active = readActive();
  const i = indexOfKey(active, key);
  if (i < 0 || i >= active.length - 1) return;
  await persist(moveDown(active, i));
}

// ---- View-title actions -----------------------------------------

async function treeRefreshAll(bridge: ElotDbBridge): Promise<void> {
  await bridge.reload();
}

// ---- registration -----------------------------------------------

export function registerTreeCommands(
  bridge: ElotDbBridge,
  context: vscode.ExtensionContext,
): vscode.Disposable {
  const subs: vscode.Disposable[] = [];
  subs.push(
    vscode.commands.registerCommand("elot.tree.activateSource", treeActivate),
  );
  subs.push(
    vscode.commands.registerCommand(
      "elot.tree.deactivateSource",
      treeDeactivate,
    ),
  );
  subs.push(
    vscode.commands.registerCommand("elot.tree.refreshSource", (a) =>
      treeRefresh(bridge, context, a),
    ),
  );
  subs.push(
    vscode.commands.registerCommand("elot.tree.deleteSource", (a) =>
      treeDelete(bridge, a),
    ),
  );
  subs.push(
    vscode.commands.registerCommand("elot.tree.browseSource", (a) =>
      treeBrowseSource(bridge, a),
    ),
  );
  subs.push(vscode.commands.registerCommand("elot.tree.moveUp", treeMoveUp));
  subs.push(
    vscode.commands.registerCommand("elot.tree.moveDown", treeMoveDown),
  );
  subs.push(
    vscode.commands.registerCommand("elot.tree.refreshAll", () =>
      treeRefreshAll(bridge),
    ),
  );
  return vscode.Disposable.from(...subs);
}
