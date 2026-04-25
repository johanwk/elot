// src/sourceCommands/reorderUx.ts
//
// Step 2.3.6: per-item-button reorder UX for
// `elot.reorderActiveSources`.  Uses the QuickPick.items[].buttons
// API (VS Code 1.84+) so users can move sources up/down in place
// without leaving the picker.

import * as vscode from "vscode";
import {
  SourceKey,
  indexOfKey,
  moveUp,
  moveDown,
  moveTop,
  moveBottom,
} from "./reorder.js";

interface Row extends vscode.QuickPickItem {
  key: SourceKey;
}

function rowsFor(active: SourceKey[]): Row[] {
  const lastIdx = active.length - 1;
  return active.map((s, i) => {
    const buttons: vscode.QuickInputButton[] = [];
    if (i > 0) {
      buttons.push({
        iconPath: new vscode.ThemeIcon("arrow-circle-up"),
        tooltip: "Move to top",
      });
      buttons.push({
        iconPath: new vscode.ThemeIcon("arrow-up"),
        tooltip: "Move up",
      });
    }
    if (i < lastIdx) {
      buttons.push({
        iconPath: new vscode.ThemeIcon("arrow-down"),
        tooltip: "Move down",
      });
      buttons.push({
        iconPath: new vscode.ThemeIcon("arrow-circle-down"),
        tooltip: "Move to bottom",
      });
    }
    return {
      label: `${i + 1}. ${s.source}`,
      description: s.dataSource ? s.dataSource : undefined,
      buttons,
      key: s,
    };
  });
}

/**
 * Show the reorder picker.  Returns the new ordering, or null if the
 * user dismissed without changes.
 */
export async function reorderWithButtons(
  initial: SourceKey[],
): Promise<SourceKey[] | null> {
  if (initial.length < 2) return null;
  let active: SourceKey[] = initial.map((s) => ({
    source: s.source,
    dataSource: s.dataSource ?? "",
  }));
  let changed = false;

  return new Promise<SourceKey[] | null>((resolve) => {
    const qp = vscode.window.createQuickPick<Row>();
    qp.title = "ELOT: Reorder active sources";
    qp.placeholder =
      "Click the up/down arrows on a row to move it. Press Enter or Esc to finish.";
    qp.canSelectMany = false;
    qp.matchOnDescription = true;
    qp.ignoreFocusOut = true;
    qp.items = rowsFor(active);

    qp.onDidTriggerItemButton((e) => {
      const idx = indexOfKey(active, e.item.key);
      if (idx === -1) return;
      const tip = e.button.tooltip ?? "";
      switch (tip) {
        case "Move up":
          active = moveUp(active, idx);
          break;
        case "Move down":
          active = moveDown(active, idx);
          break;
        case "Move to top":
          active = moveTop(active, idx);
          break;
        case "Move to bottom":
          active = moveBottom(active, idx);
          break;
        default:
          return;
      }
      changed = true;
      qp.items = rowsFor(active);
    });

    qp.onDidAccept(() => qp.hide());
    qp.onDidHide(() => {
      qp.dispose();
      resolve(changed ? active : null);
    });
    qp.show();
  });
}
