// src/tests/orgIndent.test.ts
//
// Unit tests for the org-indent visual indentation algorithm.
//
// We verify the core logic: given headline structure, which lines
// get which indent levels, and which stars get hidden.

// ─── Minimal mocks ──────────────────────────────────────────────

/** Headline regex (same as orgIndent.ts) */
const HEADLINE_RE = /^(\*+)\s/;
const INDENT_PER_LEVEL = 2;

function indentForLevel(level: number): number {
  return Math.max(0, (level - 1) * INDENT_PER_LEVEL);
}

/**
 * Given document content, compute for each line:
 *   { indent: number, hiddenStars: number }
 *
 * This mirrors the algorithm in orgIndent.ts without needing vscode.
 */
function computeIndents(content: string): Array<{ indent: number; hiddenStars: number }> {
  const lines = content.split("\n");
  const lineCount = lines.length;

  interface HeadlineInfo {
    line: number;
    level: number;
  }
  const headlines: HeadlineInfo[] = [];

  for (let i = 0; i < lineCount; i++) {
    const m = HEADLINE_RE.exec(lines[i]);
    if (m) {
      headlines.push({ line: i, level: m[1].length });
    }
  }

  const result: Array<{ indent: number; hiddenStars: number }> = [];
  // Default: no indent, no hidden stars
  for (let i = 0; i < lineCount; i++) {
    result.push({ indent: 0, hiddenStars: 0 });
  }

  for (let h = 0; h < headlines.length; h++) {
    const startLine = headlines[h].line;
    const endLine = h + 1 < headlines.length ? headlines[h + 1].line : lineCount;
    const indent = indentForLevel(headlines[h].level);

    for (let i = startLine; i < endLine; i++) {
      result[i].indent = indent;
    }

    const level = headlines[h].level;
    if (level > 1) {
      result[startLine].hiddenStars = level - 1;
    }
  }

  return result;
}

// ─── Test harness ────────────────────────────────────────────────

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

console.log("orgIndent tests");

// Test 1: level-1 heading gets no indent
{
  const result = computeIndents("* Heading\nbody line");
  assert(result[0].indent === 0, "level-1 heading: indent = 0");
  assert(result[1].indent === 0, "body under level-1: indent = 0");
  assert(result[0].hiddenStars === 0, "level-1: no hidden stars");
}

// Test 2: level-2 heading and body get 2-space indent
{
  const result = computeIndents("* H1\n** H2\nsub body");
  assert(result[0].indent === 0, "level-1: indent = 0");
  assert(result[1].indent === 2, "level-2 heading: indent = 2");
  assert(result[2].indent === 2, "body under level-2: indent = 2");
  assert(result[1].hiddenStars === 1, "level-2: 1 hidden star");
}

// Test 3: level-3 heading and body get 4-space indent
{
  const result = computeIndents("* H1\n** H2\n*** H3\ndeep body");
  assert(result[0].indent === 0, "level-1: indent = 0");
  assert(result[1].indent === 2, "level-2: indent = 2");
  assert(result[2].indent === 4, "level-3 heading: indent = 4");
  assert(result[3].indent === 4, "body under level-3: indent = 4");
  assert(result[2].hiddenStars === 2, "level-3: 2 hidden stars");
}

// Test 4: sibling headings at level 2 each get indent 2
{
  const result = computeIndents("* H1\n** A\nbody a\n** B\nbody b");
  assert(result[1].indent === 2, "** A: indent = 2");
  assert(result[2].indent === 2, "body a: indent = 2");
  assert(result[3].indent === 2, "** B: indent = 2");
  assert(result[4].indent === 2, "body b: indent = 2");
}

// Test 5: lines before any headline get no indent
{
  const result = computeIndents("preamble\nmore preamble\n* H1\nbody");
  assert(result[0].indent === 0, "preamble line 1: indent = 0");
  assert(result[1].indent === 0, "preamble line 2: indent = 0");
  assert(result[2].indent === 0, "level-1 heading: indent = 0");
  assert(result[3].indent === 0, "body under level-1: indent = 0");
}

// Test 6: no headings → no indentation
{
  const result = computeIndents("just text\nmore text");
  assert(result[0].indent === 0, "no headings line 1: indent = 0");
  assert(result[1].indent === 0, "no headings line 2: indent = 0");
}

// Test 7: back to level 1 after nested content
{
  const content = [
    "* H1",         // 0  indent=0
    "** Sub",       // 1  indent=2
    "sub body",     // 2  indent=2
    "* H2",         // 3  indent=0
    "h2 body",      // 4  indent=0
  ].join("\n");
  const result = computeIndents(content);
  assert(result[0].indent === 0, "* H1: indent=0");
  assert(result[1].indent === 2, "** Sub: indent=2");
  assert(result[2].indent === 2, "sub body: indent=2");
  assert(result[3].indent === 0, "* H2: indent=0");
  assert(result[4].indent === 0, "h2 body: indent=0");
}

console.log(`\nResults: ${passed} passed, ${failed} failed`);
if (failed > 0) process.exit(1);
