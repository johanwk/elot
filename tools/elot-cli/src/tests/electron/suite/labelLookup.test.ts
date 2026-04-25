// src/tests/electron/suite/labelLookup.test.ts
//
// Step 2.4.2 -- end-to-end electron test for the label-lookup feature.
//
// Coverage:
//   1. All three commands (`elot.labelLookup`, `elot.labelLookupLocal`,
//      `elot.labelLookupExternal`) are registered with the host.
//   2. Running `elot.labelLookupExternal` on the seeded fixture
//      auto-selects "Widget" via a stubbed `showQuickPick` and
//      inserts `ex:Widget` (the CURIE form, since the DB has the
//      `ex:` prefix registered) at the cursor.
//   3. `elot.labelLookup.scope` round-trips through
//      `configuration.update()` and is observable via
//      `getConfiguration().get()`.
//   4. With empty `elot.activeLabelSources`, `labelLookupExternal`
//      shows an information message instead of opening a QuickPick
//      (stubbed `showInformationMessage` captures the call).
//
// Stubbing strategy: monkey-patch `vscode.window.showQuickPick` and
// `vscode.window.showInformationMessage` for the duration of one
// test, restoring originals in `finally`.  This is the standard
// pattern for scripting QuickPick acceptance from inside the host;
// VS Code does not expose an API to drive the QuickPick UI directly.

import * as assert from "assert";
import * as vscode from "vscode";

const EXTENSION_ID = "johanwk.elot";
const SETTING_SCOPE = "labelLookup.scope";

async function ensureActivated(): Promise<void> {
  const ext = vscode.extensions.getExtension(EXTENSION_ID);
  assert.ok(ext, `extension ${EXTENSION_ID} not found`);
  if (!ext!.isActive) await ext!.activate();
}

/**
 * Open a fresh untitled `plaintext` document with the given content
 * and cursor at (line, col).  We use an untitled doc rather than
 * `sample.txt` so we never mutate the on-disk fixture (and so we
 * never leak buffer state into the hover/commands suites that read
 * `sample.txt` from disk).
 */
async function openUntitled(
  text: string,
  line: number,
  col: number,
): Promise<vscode.TextEditor> {
  const doc = await vscode.workspace.openTextDocument({
    language: "plaintext",
    content: text,
  });
  const editor = await vscode.window.showTextDocument(doc);
  const pos = new vscode.Position(line, col);
  editor.selection = new vscode.Selection(pos, pos);
  return editor;
}

async function closeActiveEditor(): Promise<void> {
  await vscode.commands.executeCommand(
    "workbench.action.revertAndCloseActiveEditor",
  );
}

interface PickArgs {
  /** Picker label to match (case-insensitive substring). */
  matchLabel: string;
  /** Captured items the QuickPick was offered. */
  captured: vscode.QuickPickItem[];
  /** Will be set true once the stubbed picker fires. */
  fired: { value: boolean };
}

/** Install a `showQuickPick` stub that auto-selects an item by label. */
function stubQuickPick(
  args: PickArgs,
): { restore: () => void } {
  const original = vscode.window.showQuickPick;
  // The real signature is overloaded; cast to `any` to assign.
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  (vscode.window as any).showQuickPick = async (
    items: vscode.QuickPickItem[] | Thenable<vscode.QuickPickItem[]>,
    _options?: unknown,
  ) => {
    const resolved = Array.isArray(items) ? items : await items;
    // Mutate the array in place so the outer `captured` reference
    // (held by the test) sees the items.  Reassigning `args.captured`
    // would only update the field on this stub-args object.
    args.captured.length = 0;
    for (const it of resolved) args.captured.push(it);
    args.fired.value = true;
    const needle = args.matchLabel.toLowerCase();
    return resolved.find((it) => it.label.toLowerCase().includes(needle));
  };
  return {
    restore: () => {
      // eslint-disable-next-line @typescript-eslint/no-explicit-any
      (vscode.window as any).showQuickPick = original;
    },
  };
}

interface InfoArgs {
  messages: string[];
}
function stubInfoMessage(args: InfoArgs): { restore: () => void } {
  const original = vscode.window.showInformationMessage;
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  (vscode.window as any).showInformationMessage = async (
    msg: string,
    ..._rest: unknown[]
  ) => {
    args.messages.push(msg);
    return undefined;
  };
  return {
    restore: () => {
      // eslint-disable-next-line @typescript-eslint/no-explicit-any
      (vscode.window as any).showInformationMessage = original;
    },
  };
}

suite("electron labelLookup", () => {
  suiteSetup(async function () {
    this.timeout(30000);
    await ensureActivated();
    // Ensure the DB bridge is fully loaded before the first lookup
    // test runs.  `elot.dbInfo` triggers bridge.get() and awaits it.
    await vscode.commands.executeCommand("elot.dbInfo");
    // Belt-and-braces: give the host a tick to settle.
    await new Promise((r) => setTimeout(r, 200));
  });

  test("all three label-lookup commands are registered", async function () {
    this.timeout(5000);
    const all = await vscode.commands.getCommands(true);
    for (const id of [
      "elot.labelLookup",
      "elot.labelLookupLocal",
      "elot.labelLookupExternal",
    ]) {
      assert.ok(all.includes(id), `command not registered: ${id}`);
    }
  });

  test("labelLookupExternal: pick 'Widget' inserts 'ex:Widget' at cursor", async function () {
    this.timeout(30000);
    const editor = await openUntitled("INSERT> <\n", 0, 8);

    const captured: vscode.QuickPickItem[] = [];
    const fired = { value: false };
    const qp = stubQuickPick({ matchLabel: "Widget", captured, fired });
    try {
      await vscode.commands.executeCommand("elot.labelLookupExternal");
    } finally {
      qp.restore();
    }
    assert.strictEqual(
      fired.value,
      true,
      "showQuickPick was not invoked",
    );
    // The QuickPick should have offered both Widget and Gadget.
    const labels = captured.map((it) => it.label).sort();
    const diag =
      `captured ${captured.length} item(s): ` +
      JSON.stringify(
        captured.map((it) => ({
          label: it.label,
          description: it.description,
          detail: it.detail,
        })),
      );
    assert.ok(
      labels.includes("Widget"),
      `QuickPick missing Widget. ${diag}`,
    );
    assert.ok(
      labels.includes("Gadget"),
      `QuickPick missing Gadget. ${diag}`,
    );

    // Inserted token = ex:Widget (CURIE form; ex: prefix is in DB).
    const after = editor.document.getText();
    assert.strictEqual(
      after,
      "INSERT> ex:Widget<\n",
      `unexpected buffer contents: ${JSON.stringify(after)}`,
    );
    await closeActiveEditor();
  });

  test("elot.labelLookup.scope round-trips via configuration.update()", async function () {
    this.timeout(10000);
    const cfg = () => vscode.workspace.getConfiguration("elot");
    const original = cfg().get<string>(SETTING_SCOPE);
    try {
      for (const v of ["local", "external", "both"]) {
        await cfg().update(
          SETTING_SCOPE,
          v,
          vscode.ConfigurationTarget.Workspace,
        );
        // Re-read fresh; do NOT reuse the captured cfg.
        const got = cfg().get<string>(SETTING_SCOPE);
        assert.strictEqual(got, v, `scope round-trip failed for ${v}`);
      }
    } finally {
      await cfg().update(
        SETTING_SCOPE,
        original,
        vscode.ConfigurationTarget.Workspace,
      );
    }
  });

  test("labelLookupExternal with empty active sources shows info, no QuickPick", async function () {
    this.timeout(20000);
    const editor = await openUntitled("EMPTY> X\n", 0, 7);

    const cfg = vscode.workspace.getConfiguration("elot");
    const originalActive = cfg.get("activeLabelSources");

    const captured: vscode.QuickPickItem[] = [];
    const fired = { value: false };
    const qp = stubQuickPick({ matchLabel: "Widget", captured, fired });
    const info: InfoArgs = { messages: [] };
    const im = stubInfoMessage(info);
    try {
      await vscode.workspace
        .getConfiguration("elot")
        .update(
          "activeLabelSources",
          [],
          vscode.ConfigurationTarget.Workspace,
        );
      // Settle.
      await new Promise((r) => setTimeout(r, 100));

      await vscode.commands.executeCommand("elot.labelLookupExternal");

      assert.strictEqual(
        fired.value,
        false,
        "showQuickPick should NOT have fired with empty active sources",
      );
      assert.ok(
        info.messages.some((m) => /no active label sources/i.test(m)),
        `expected info message; got: ${info.messages.join(" | ")}`,
      );
      // Buffer should be unchanged.
      assert.strictEqual(editor.document.getText(), "EMPTY> X\n");
    } finally {
      qp.restore();
      im.restore();
      await vscode.workspace
        .getConfiguration("elot")
        .update(
          "activeLabelSources",
          originalActive,
          vscode.ConfigurationTarget.Workspace,
        );
      await closeActiveEditor();
    }
  });
});
