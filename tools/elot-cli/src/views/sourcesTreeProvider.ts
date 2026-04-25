// src/views/sourcesTreeProvider.ts
//
// Step 2.3.7a: TreeDataProvider for the "Sources in DB" view.
//
// Shows all sources registered in the live DB.  Active sources are
// flagged in the description; the contextValue gates per-row inline
// icons via package.json menu bindings.
//
// Refresh triggers:
//   - bridge.onDidChange (DB rewritten by CLI)
//   - workspace.onDidChangeConfiguration("elot.activeLabelSources")

import * as vscode from "vscode";
import type { ElotDbBridge } from "../db/bridge.js";
import { normalizeActiveSources } from "../activeSources.js";
import { buildDbSourceItems, type DbSourceItem } from "./sourceItems.js";
import type { SourceKey } from "../sourceCommands/reorder.js";

export class SourcesTreeProvider
  implements vscode.TreeDataProvider<DbSourceItem>
{
  private readonly emitter = new vscode.EventEmitter<void>();
  readonly onDidChangeTreeData = this.emitter.event;

  constructor(private readonly bridge: ElotDbBridge) {}

  refresh(): void {
    this.emitter.fire();
  }

  getTreeItem(item: DbSourceItem): vscode.TreeItem {
    const ti = new vscode.TreeItem(item.label, vscode.TreeItemCollapsibleState.None);
    ti.description = item.description;
    ti.tooltip = item.tooltip;
    ti.contextValue = item.contextValue;
    ti.iconPath = new vscode.ThemeIcon(item.isActive ? "tag" : "tag");
    if (!item.isActive) {
      ti.resourceUri = undefined;
    }
    // Stash key for command-binding lookups.
    (ti as any).__elotKey = item.key;
    return ti;
  }

  async getChildren(): Promise<DbSourceItem[]> {
    const db = await this.bridge.get();
    if (!db) return [];
    const sources = db.listSources();
    const active = readActive();
    const counts = new Map<string, number>();
    for (const r of sources) {
      const ck = `${r.source}\u0000${r.dataSource ?? ""}`;
      try {
        counts.set(ck, db.sourceEntityCount(r.source, r.dataSource ?? ""));
      } catch {
        /* ignore - fall through with no count */
      }
    }
    return buildDbSourceItems(sources, active, counts);
  }
}

function readActive(): SourceKey[] {
  const cfg = vscode.workspace.getConfiguration("elot");
  return normalizeActiveSources(cfg.get<unknown>("activeLabelSources")).map(
    (s) => ({ source: s.source, dataSource: s.dataSource ?? "" }),
  );
}

export function registerSourcesTreeView(
  bridge: ElotDbBridge,
): vscode.Disposable {
  const provider = new SourcesTreeProvider(bridge);
  const view = vscode.window.createTreeView("elot.sourcesView", {
    treeDataProvider: provider,
    showCollapseAll: false,
  });
  const offBridge = bridge.onDidChange(() => provider.refresh());
  const onCfg = vscode.workspace.onDidChangeConfiguration((e) => {
    if (e.affectsConfiguration("elot.activeLabelSources")) provider.refresh();
  });
  return vscode.Disposable.from(view, { dispose: offBridge }, onCfg);
}
