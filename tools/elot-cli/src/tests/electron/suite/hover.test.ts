// src/tests/electron/suite/hover.test.ts
//
// Step 2.3.5c -- end-to-end hover provider assertions.
//
// Opens the seeded fixture (sample.txt), invokes
// vscode.executeHoverProvider at known CURIE / IRI positions, and
// asserts the rendered Markdown contains the expected label and
// the [src: ex] provenance footer that uniquely identifies *our*
// hover (other extensions may also contribute hovers).

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

/** Locate the first line containing `needle` and matching the
 *  predicate; return cursor position somewhere inside the match. */
function findPos(
  doc: vscode.TextDocument,
  needle: string,
  predicate?: (line: string) => boolean,
): vscode.Position | null {
  for (let i = 0; i < doc.lineCount; i++) {
    const t = doc.lineAt(i).text;
    if (predicate && !predicate(t)) continue;
    const j = t.indexOf(needle);
    if (j >= 0) {
      // Pick a column a few chars into the match so we land
      // unambiguously inside the token.
      return new vscode.Position(i, j + Math.min(3, needle.length - 1));
    }
  }
  return null;
}

/** Flatten Hover[] -> single Markdown string for grepping. */
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

/** Filter to hovers that came from our DB hover provider, identified
 *  by the [src: ex] provenance footer the renderer always emits. */
function ourHoverCount(hovers: vscode.Hover[] | undefined): number {
  if (!hovers) return 0;
  return hovers.filter((h) =>
    h.contents.some((c) => {
      const v =
        typeof c === "string" ? c : (c as vscode.MarkdownString).value;
      return /\[src: ex\]/.test(v);
    }),
  ).length;
}

suite("electron hover", () => {
  suiteSetup(async function () {
    this.timeout(30000);
    await ensureActivated();
  });

  test("hover on known CURIE 'ex:Widget' returns label + provenance", async function () {
    this.timeout(20000);
    const editor = await openSample();
    const doc = editor.document;
    const pos = findPos(
      doc,
      "ex:Widget",
      (t) => t.indexOf("Full IRI") < 0, // skip the IRI line
    );
    assert.ok(pos, "ex:Widget CURIE position not found in sample");

    const hovers = await vscode.commands.executeCommand<vscode.Hover[]>(
      "vscode.executeHoverProvider",
      doc.uri,
      pos!,
    );
    const md = hoverMarkdown(hovers);
    assert.ok(/\*\*Widget\*\*/.test(md), `hover missing **Widget** label: ${md}`);
    assert.ok(/\[src: ex\]/.test(md), `hover missing provenance: ${md}`);
    // Featured prop should also appear.
    assert.ok(
      /skos:definition[^\n]*A test widget/.test(md) ||
        /A test widget/.test(md),
      `hover missing skos:definition: ${md}`,
    );
  });

  test("hover on full IRI returns same label", async function () {
    this.timeout(20000);
    const editor = await openSample();
    const doc = editor.document;
    const pos = findPos(doc, "<http://example.org/ex/Widget>");
    assert.ok(pos, "IRI position not found in sample");

    const hovers = await vscode.commands.executeCommand<vscode.Hover[]>(
      "vscode.executeHoverProvider",
      doc.uri,
      pos!,
    );
    const md = hoverMarkdown(hovers);
    assert.ok(/\*\*Widget\*\*/.test(md), `IRI hover missing label: ${md}`);
    assert.ok(/\[src: ex\]/.test(md), `IRI hover missing provenance: ${md}`);
  });

  test("hover on second CURIE 'ex:Gadget' resolves independently", async function () {
    this.timeout(20000);
    const editor = await openSample();
    const doc = editor.document;
    const pos = findPos(doc, "ex:Gadget");
    assert.ok(pos, "ex:Gadget position not found in sample");

    const hovers = await vscode.commands.executeCommand<vscode.Hover[]>(
      "vscode.executeHoverProvider",
      doc.uri,
      pos!,
    );
    const md = hoverMarkdown(hovers);
    assert.ok(/\*\*Gadget\*\*/.test(md), `hover missing **Gadget** label: ${md}`);
    assert.ok(/\[src: ex\]/.test(md), `hover missing provenance: ${md}`);
  });

  test("hover on unknown CURIE 'ex:NotInDB' yields no ELOT hover", async function () {
    this.timeout(20000);
    const editor = await openSample();
    const doc = editor.document;
    const pos = findPos(doc, "ex:NotInDB");
    assert.ok(pos, "ex:NotInDB position not found in sample");

    const hovers = await vscode.commands.executeCommand<vscode.Hover[]>(
      "vscode.executeHoverProvider",
      doc.uri,
      pos!,
    );
    // Other extensions may contribute their own hovers; we only
    // assert OURS is absent.
    assert.strictEqual(
      ourHoverCount(hovers),
      0,
      `expected no ELOT hover for unknown id, got ${hovers?.length ?? 0} hovers`,
    );
  });
});
