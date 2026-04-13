// src/tests/foldingProvider.test.ts
//
// Unit tests for the OrgFoldingRangeProvider.
//
// We create a minimal mock of vscode.TextDocument so the provider
// can be exercised without a running VS Code instance.

import { OrgFoldingRangeProvider } from "../foldingProvider.js";

// ─── Minimal VS Code mocks ──────────────────────────────────────

class MockTextLine {
  readonly text: string;
  readonly isEmptyOrWhitespace: boolean;
  constructor(text: string) {
    this.text = text;
    this.isEmptyOrWhitespace = text.trim().length === 0;
  }
}

class MockTextDocument {
  private lines: string[];
  languageId = "org";
  get lineCount() {
    return this.lines.length;
  }
  constructor(content: string) {
    this.lines = content.split("\n");
  }
  lineAt(n: number): MockTextLine {
    return new MockTextLine(this.lines[n]);
  }
}

// Shim the vscode FoldingRange / FoldingRangeKind so the provider
// file can construct real objects.  We define them on globalThis
// since the provider imports them from "vscode".
const FoldingRange = class {
  start: number;
  end: number;
  kind?: number;
  constructor(start: number, end: number, kind?: number) {
    this.start = start;
    this.end = end;
    this.kind = kind;
  }
};
const FoldingRangeKind = { Comment: 1, Imports: 2, Region: 3 };

// Patch the vscode module resolution — we rely on the provider
// being tested via tsx where we can supply a shim.
// For simplicity, this test imports the class directly and
// calls provideFoldingRanges with mocked arguments.

// ─── Helpers ─────────────────────────────────────────────────────

function fold(content: string): Array<{ start: number; end: number }> {
  // Temporarily patch globalThis.vscode for the import
  const provider = new OrgFoldingRangeProvider();
  const doc = new MockTextDocument(content) as any;
  // We need vscode.FoldingRange and vscode.FoldingRangeKind available.
  // The provider file references them, but since we import the class
  // directly the references are already resolved at import time.
  // So we need to supply them globally before importing the module.
  // For this test we assume the esbuild/tsx environment provides
  // the necessary shims, or we run through VS Code's test runner.
  //
  // As a pragmatic workaround the test is structured to verify the
  // *algorithm* by using the class directly with duck-typed args.
  const ranges = provider.provideFoldingRanges(doc, {} as any, {} as any);
  return ranges.map((r: any) => ({ start: r.start, end: r.end }));
}

// ─── Tests ───────────────────────────────────────────────────────

let passed = 0;
let failed = 0;

function assert(condition: boolean, msg: string) {
  if (!condition) {
    console.error(`  FAIL: ${msg}`);
    failed++;
  } else {
    console.log(`  ok: ${msg}`);
    passed++;
  }
}

function deepEqual(a: any, b: any): boolean {
  return JSON.stringify(a) === JSON.stringify(b);
}

console.log("foldingProvider tests");

// Test 1: single headline folds to end of document
{
  const content = [
    "* Heading 1",
    "some body text",
    "more text",
  ].join("\n");

  const ranges = fold(content);
  assert(
    deepEqual(ranges, [{ start: 0, end: 2 }]),
    "single headline folds to end of document",
  );
}

// Test 2: two same-level headlines
{
  const content = [
    "* Heading 1",
    "body 1",
    "* Heading 2",
    "body 2",
  ].join("\n");

  const ranges = fold(content);
  assert(
    deepEqual(ranges, [
      { start: 0, end: 1 },
      { start: 2, end: 3 },
    ]),
    "two same-level headlines produce two regions",
  );
}

// Test 3: nested headlines
{
  const content = [
    "* Heading 1",       // 0
    "** Sub-heading",    // 1
    "sub body",          // 2
    "** Another sub",    // 3
    "sub body 2",        // 4
    "* Heading 2",       // 5
    "body 2",            // 6
  ].join("\n");

  const ranges = fold(content);
  // * Heading 1 → line 0..4  (ends before * Heading 2, skip no blanks)
  // ** Sub-heading → line 1..2
  // ** Another sub → line 3..4
  // * Heading 2 → line 5..6
  assert(
    deepEqual(ranges, [
      { start: 0, end: 4 },
      { start: 1, end: 2 },
      { start: 3, end: 4 },
      { start: 5, end: 6 },
    ]),
    "nested headlines produce correct hierarchy",
  );
}

// Test 4: trailing blank lines are trimmed
{
  const content = [
    "* Heading 1",
    "body",
    "",
    "",
    "* Heading 2",
    "body 2",
  ].join("\n");

  const ranges = fold(content);
  assert(
    deepEqual(ranges, [
      { start: 0, end: 1 },
      { start: 4, end: 5 },
    ]),
    "trailing blank lines before next heading are excluded",
  );
}

// Test 5: no headlines → no folds
{
  const ranges = fold("Just plain text\nno headings here\n");
  assert(deepEqual(ranges, []), "no headlines → no fold ranges");
}

console.log(`\nResults: ${passed} passed, ${failed} failed`);
if (failed > 0) process.exit(1);
