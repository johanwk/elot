// src/tests/electron/suite/smoke.test.ts
//
// Step 2.3.5b - First electron smoke test.
//
// Asserts that the extension activates inside the fixture workspace
// without throwing, and that `elot.dbInfo` runs to completion.  This
// exercises the full activation path:
//
//   - extension.ts entrypoint loads
//   - bridge resolves the DB path from .vscode/settings.json
//   - sql.js opens the seeded DB
//   - all four DB-feature commands register
//   - status bar item appears
//
// Per-feature assertions (hover markdown, decoration ranges, settings
// round-trip on activate/deactivate/reorder) live in sibling test files
// and will be added on subsequent turns; this file is the harness
// proof-of-life.

import * as assert from "assert";
import * as vscode from "vscode";

const EXTENSION_ID = "johanwk.elot";

suite("electron smoke", () => {
  test("extension is present", () => {
    const ext = vscode.extensions.getExtension(EXTENSION_ID);
    assert.ok(ext, `extension ${EXTENSION_ID} not found`);
  });

  test("extension activates without throwing", async function () {
    this.timeout(30000);
    const ext = vscode.extensions.getExtension(EXTENSION_ID);
    assert.ok(ext);
    await ext!.activate();
    assert.strictEqual(ext!.isActive, true, "extension did not activate");
  });

  test("DB-feature commands are registered", async () => {
    const all = await vscode.commands.getCommands(true);
    for (const cmd of [
      "elot.dbInfo",
      "elot.toggleGlobalLabels",
      "elot.activateSource",
      "elot.deactivateSource",
      "elot.reorderActiveSources",
    ]) {
      assert.ok(all.includes(cmd), `missing command: ${cmd}`);
    }
  });

  test("elot.dbInfo runs without throwing", async function () {
    this.timeout(15000);
    // dbInfo pops a non-modal info message; it should resolve quickly.
    await vscode.commands.executeCommand("elot.dbInfo");
  });
});
