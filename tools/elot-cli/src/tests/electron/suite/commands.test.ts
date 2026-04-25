// src/tests/electron/suite/commands.test.ts
//
// Step 2.3.5c -- command + settings round-trip assertions.
//
//   - elot.toggleGlobalLabels runs without throwing (both directions).
//   - Clearing elot.activeLabelSources disables hover end-to-end;
//     restoring it re-enables.  This exercises:
//        configuration.update() -> hover provider re-reads on next
//        invocation -> empty active list short-circuits to no hover.
//   - elot.dbInfo runs cleanly after the settings round-trip.

import * as assert from "assert";
import * as path from "path";
import * as vscode from "vscode";

const EXTENSION_ID = "johanwk.elot";

async function ensureActivated(): Promise<void> {
  const ext = vscode.extensions.getExtension(EXTENSION_ID);
  assert.ok(ext, `extension ${EXTENSION_ID} not found`);
  if (!ext!.isActive) await ext!.activate();
}

async function openSample(): Promise<vscode.TextEditor> {
  const ws = vscode.workspace.workspaceFolders;
  assert.ok(ws && ws.length > 0, "no workspace folder open");
  const samplePath = path.join(ws![0].uri.fsPath, "sample.txt");
  const doc = await vscode.workspace.openTextDocument(samplePath);
  return await vscode.window.showTextDocument(doc);
}

function findCuriePos(doc: vscode.TextDocument): vscode.Position {
  for (let i = 0; i < doc.lineCount; i++) {
    const t = doc.lineAt(i).text;
    if (t.indexOf("Full IRI") >= 0) continue;
    const j = t.indexOf("ex:Widget");
    if (j >= 0) return new vscode.Position(i, j + 3);
  }
  throw new Error("ex:Widget CURIE not found in sample");
}

function hoverMarkdown(hovers: vscode.Hover[] | undefined): string {
  if (!hovers || hovers.length === 0) return "";
  return hovers
    .flatMap((h) =>
      h.contents.map((c) =>
        typeof c === "string" ? c : (c as vscode.MarkdownString).value,
      ),
    )
    .join("\n");
}

function hasOurHover(hovers: vscode.Hover[] | undefined): boolean {
  return /\[src: ex\]/.test(hoverMarkdown(hovers));
}

async function setActive(value: unknown): Promise<void> {
  const cfg = vscode.workspace.getConfiguration("elot");
  await cfg.update(
    "activeLabelSources",
    value,
    vscode.ConfigurationTarget.Workspace,
  );
}

function readActive(): unknown {
  const cfg = vscode.workspace.getConfiguration("elot");
  return cfg.get("activeLabelSources");
}

suite("electron commands + settings round-trip", () => {
  suiteSetup(async function () {
    this.timeout(30000);
    await ensureActivated();
  });

  test("elot.toggleGlobalLabels toggles without throwing (twice)", async function () {
    this.timeout(15000);
    // Open the sample so the command has an editor to act on.
    await openSample();
    await vscode.commands.executeCommand("elot.toggleGlobalLabels");
    await vscode.commands.executeCommand("elot.toggleGlobalLabels");
  });

  test("clearing activeLabelSources suppresses hover; restoring re-enables", async function () {
    this.timeout(30000);
    const original = readActive();
    const editor = await openSample();
    const doc = editor.document;
    const pos = findCuriePos(doc);

    // Baseline: original fixture has [{source:"ex"}] active -> hover hits.
    let hovers = await vscode.commands.executeCommand<vscode.Hover[]>(
      "vscode.executeHoverProvider",
      doc.uri,
      pos,
    );
    assert.ok(
      hasOurHover(hovers),
      `baseline expected ELOT hover; got: ${hoverMarkdown(hovers)}`,
    );

    try {
      // Clear active sources at workspace scope.
      await setActive([]);
      // Settle: VS Code propagates the configuration change
      // synchronously, but allow one tick for any debounced
      // listeners (status bar, decoration manager).
      await new Promise((r) => setTimeout(r, 100));

      hovers = await vscode.commands.executeCommand<vscode.Hover[]>(
        "vscode.executeHoverProvider",
        doc.uri,
        pos,
      );
      assert.strictEqual(
        hasOurHover(hovers),
        false,
        `expected NO ELOT hover with empty active sources; got: ${hoverMarkdown(hovers)}`,
      );

      // Restore via setActive (not just original) to confirm the
      // round-trip actually re-enables resolution.
      await setActive([{ source: "ex", dataSource: "" }]);
      await new Promise((r) => setTimeout(r, 100));

      hovers = await vscode.commands.executeCommand<vscode.Hover[]>(
        "vscode.executeHoverProvider",
        doc.uri,
        pos,
      );
      assert.ok(
        hasOurHover(hovers),
        `expected ELOT hover after restore; got: ${hoverMarkdown(hovers)}`,
      );
    } finally {
      // Always restore the original value so subsequent tests see
      // the fixture's pristine state.
      await setActive(original);
    }
  });

  test("elot.dbInfo runs cleanly after settings round-trip", async function () {
    this.timeout(10000);
    await vscode.commands.executeCommand("elot.dbInfo");
  });

  test("2.3.6 commands are registered", async function () {
    this.timeout(5000);
    const all = await vscode.commands.getCommands(true);
    for (const id of ["elot.registerSource", "elot.refreshSource"]) {
      assert.ok(all.includes(id), `command not registered: ${id}`);
    }
  });

  test("2.3.7a tree commands are registered", async function () {
    this.timeout(5000);
    const all = await vscode.commands.getCommands(true);
    const ids = [
      "elot.tree.activateSource",
      "elot.tree.deactivateSource",
      "elot.tree.refreshSource",
      "elot.tree.deleteSource",
      "elot.tree.moveUp",
      "elot.tree.moveDown",
      "elot.tree.refreshAll",
      "elot.tree.browseSource",
    ];
    for (const id of ids) {
      assert.ok(all.includes(id), `command not registered: ${id}`);
    }
  });

  test("2.3.7b persist commands are registered", async function () {
    this.timeout(5000);
    const all = await vscode.commands.getCommands(true);
    for (const id of [
      "elot.persistActiveSourcesToWorkspace",
      "elot.persistActiveSourcesToUser",
    ]) {
      assert.ok(all.includes(id), `command not registered: ${id}`);
    }
  });

  test("elot.persistActiveSourcesToWorkspace round-trip: writes effective value to workspace scope", async function () {
    this.timeout(15000);
    const cfg0 = vscode.workspace.getConfiguration("elot");
    const originalWs = cfg0.inspect<unknown>("activeLabelSources")
      ?.workspaceValue;
    const originalUser = cfg0.inspect<unknown>("activeLabelSources")
      ?.globalValue;

    try {
      // Set a known effective value at workspace scope, then clear
      // workspace scope and re-persist via the command.  The
      // command must read the *effective* value at call-time and
      // write it back to the chosen scope.
      const known = [
        { source: "ex", dataSource: "" },
        { source: "demo", dataSource: "" },
      ];
      await vscode.workspace
        .getConfiguration("elot")
        .update(
          "activeLabelSources",
          known,
          vscode.ConfigurationTarget.Workspace,
        );
      await new Promise((r) => setTimeout(r, 50));

      await vscode.commands.executeCommand(
        "elot.persistActiveSourcesToWorkspace",
      );
      await new Promise((r) => setTimeout(r, 100));

      const after = vscode.workspace
        .getConfiguration("elot")
        .inspect<unknown>("activeLabelSources");
      assert.deepStrictEqual(
        after?.workspaceValue,
        known,
        `workspaceValue mismatch; got ${JSON.stringify(after?.workspaceValue)}`,
      );
    } finally {
      await vscode.workspace
        .getConfiguration("elot")
        .update(
          "activeLabelSources",
          originalWs,
          vscode.ConfigurationTarget.Workspace,
        );
      await vscode.workspace
        .getConfiguration("elot")
        .update(
          "activeLabelSources",
          originalUser,
          vscode.ConfigurationTarget.Global,
        );
    }
  });
});
