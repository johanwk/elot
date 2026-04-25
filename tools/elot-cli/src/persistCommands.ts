// src/persistCommands.ts
//
// Step 2.3.7b: VS Code wiring for
//   - elot.persistActiveSourcesToWorkspace
//   - elot.persistActiveSourcesToUser
//
// Both commands snapshot the *effective* value of
// `elot.activeLabelSources` (whatever the current resolved order is,
// from any scope) and write it to the chosen target scope.  This
// mirrors the Elisp `elot-label-active-sources-persist` UX:
// "save what I'm using right now to the project / to my profile".

import * as vscode from "vscode";
import { buildPersistPlan, describePersistResult } from "./sourceCommands/persist.js";

const SETTING_KEY = "activeLabelSources";

async function persistTo(
  scope: "workspace" | "user",
): Promise<void> {
  const cfg = vscode.workspace.getConfiguration("elot");
  const effective = cfg.get<unknown>(SETTING_KEY);
  const plan = buildPersistPlan(effective, scope);

  if (
    scope === "workspace" &&
    (!vscode.workspace.workspaceFolders ||
      vscode.workspace.workspaceFolders.length === 0)
  ) {
    vscode.window.showErrorMessage(
      "ELOT: cannot persist to workspace settings - no workspace folder is open.",
    );
    return;
  }

  const target =
    scope === "workspace"
      ? vscode.ConfigurationTarget.Workspace
      : vscode.ConfigurationTarget.Global;

  try {
    await cfg.update(SETTING_KEY, plan.value, target);
    vscode.window.showInformationMessage(describePersistResult(plan));
  } catch (err: any) {
    vscode.window.showErrorMessage(
      `ELOT: failed to persist active label sources (${err?.message ?? err}).`,
    );
  }
}

export function registerPersistCommands(): vscode.Disposable {
  const subs: vscode.Disposable[] = [];
  subs.push(
    vscode.commands.registerCommand(
      "elot.persistActiveSourcesToWorkspace",
      () => persistTo("workspace"),
    ),
  );
  subs.push(
    vscode.commands.registerCommand(
      "elot.persistActiveSourcesToUser",
      () => persistTo("user"),
    ),
  );
  return vscode.Disposable.from(...subs);
}
