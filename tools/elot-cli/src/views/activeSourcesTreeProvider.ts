// src/views/activeSourcesTreeProvider.ts
//
// Step 2.3.7a: TreeDataProvider for the "Active Sources" view.
//
// Shows elot.activeLabelSources in priority order.  Each row's
// contextValue (first/middle/last/only) gates which inline icons
// (move-up / move-down / deactivate) are shown.

import * as vscode from "vscode";
import type { ElotDbBridge } from "../db/bridge.js";
import { normalizeActiveSources } from "../activeSources.js";
import {
  buildActiveSourceItems,
  type ActiveSourceItem,
} from "./sourceItems.js";
import type { SourceKey } from "../sourceCommands/reorder.js";

export class ActiveSourcesTreeProvider
  implements vscode.TreeDataProvider<ActiveSourceItem>
{
  private readonly emitter = new vscode.EventEmitter<void>();
  readonly onDidChangeTreeData = this.emitter.event;

  constructor(private readonly bridge: ElotDbBridge) {}

  refresh(): void {
    this.emitter.fire();
  }

  getTreeItem(item: ActiveSourceItem): vscode.TreeItem {
    const ti = new vscode.TreeItem(
      `${item.prefix} ${item.label}`,
      vscode.TreeItemCollapsibleState.None,
    );
    ti.description = item.description;
    ti.tooltip = item.tooltip;
    ti.contextValue = item.contextValue;
    ti.iconPath = new vscode.ThemeIcon("list-ordered");
    (ti as any).__elotKey = item.key;
    return ti;
  }

  async getChildren(): Promise<ActiveSourceItem[]> {
    const active: SourceKey[] = readActive();
    let dbSources;
    try {
      const db = await this.bridge.get();
      dbSources = db ? db.listSources() : undefined;
    } catch {
      dbSources = undefined;
    }
    return buildActiveSourceItems(active, dbSources);
  }
}

function readActive(): SourceKey[] {
  const cfg = vscode.workspace.getConfiguration("elot");
  return normalizeActiveSources(cfg.get<unknown>("activeLabelSources")).map(
    (s) => ({ source: s.source, dataSource: s.dataSource ?? "" }),
  );
}

export function registerActiveSourcesTreeView(
  bridge: ElotDbBridge,
): vscode.Disposable {
  const provider = new ActiveSourcesTreeProvider(bridge);
  const view = vscode.window.createTreeView("elot.activeSourcesView", {
    treeDataProvider: provider,
    showCollapseAll: false,
  });
  const offBridge = bridge.onDidChange(() => provider.refresh());
  const onCfg = vscode.workspace.onDidChangeConfiguration((e) => {
    if (e.affectsConfiguration("elot.activeLabelSources")) provider.refresh();
  });
  return vscode.Disposable.from(view, { dispose: offBridge }, onCfg);
}
