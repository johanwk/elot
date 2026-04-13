// src/tests/headlineBoldDecorations.test.ts
//
// Tests for the headline regex matching used in
// headlineBoldDecorations.ts.

import assert from "node:assert/strict";

// ─── Regex under test (duplicated here to test in isolation) ─────

const HEADLINE_RE = /^(\*+)\s/;

// ─── Tests ───────────────────────────────────────────────────────

function testLevel1Headline() {
  const line = "* Top-level heading";
  const m = HEADLINE_RE.exec(line);
  assert.ok(m, "should match a level-1 headline");
  assert.equal(m[1], "*");
  assert.equal(m[1].length, 1);
  console.log("  ✓ level-1 headline");
}

function testLevel2Headline() {
  const line = "** Sub-heading";
  const m = HEADLINE_RE.exec(line);
  assert.ok(m, "should match a level-2 headline");
  assert.equal(m[1], "**");
  assert.equal(m[1].length, 2);
  console.log("  ✓ level-2 headline");
}

function testLevel3Headline() {
  const line = "*** Deep heading";
  const m = HEADLINE_RE.exec(line);
  assert.ok(m, "should match a level-3 headline");
  assert.equal(m[1].length, 3);
  console.log("  ✓ level-3 headline");
}

function testLevel5Headline() {
  const line = "***** Very deep heading";
  const m = HEADLINE_RE.exec(line);
  assert.ok(m, "should match a level-5 headline");
  assert.equal(m[1].length, 5);
  console.log("  ✓ level-5 headline");
}

function testBodyTextNotMatched() {
  const line = "Just some body text";
  const m = HEADLINE_RE.exec(line);
  assert.equal(m, null, "should not match body text");
  console.log("  ✓ body text not matched");
}

function testStarsWithoutSpace() {
  // `***word` should NOT match — Org requires a space after stars
  const line = "***word";
  const m = HEADLINE_RE.exec(line);
  assert.equal(m, null, "should not match stars without trailing space");
  console.log("  ✓ stars without space not matched");
}

function testEmptyLine() {
  const line = "";
  const m = HEADLINE_RE.exec(line);
  assert.equal(m, null, "should not match empty line");
  console.log("  ✓ empty line not matched");
}

function testIndentedStars() {
  // Leading whitespace before stars is NOT an Org headline
  const line = "  ** Not a headline";
  const m = HEADLINE_RE.exec(line);
  assert.equal(m, null, "should not match indented stars");
  console.log("  ✓ indented stars not matched");
}

function testListItemNotMatched() {
  const line = " - list item";
  const m = HEADLINE_RE.exec(line);
  assert.equal(m, null, "should not match list items");
  console.log("  ✓ list item not matched");
}

function testHeadlineWithTags() {
  const line = "* Heading with tags  :tag1:tag2:";
  const m = HEADLINE_RE.exec(line);
  assert.ok(m, "should match headline even with trailing tags");
  assert.equal(m[1].length, 1);
  console.log("  ✓ headline with tags");
}

// ─── Run ─────────────────────────────────────────────────────────

console.log("\nheadlineBoldDecorations tests:");
testLevel1Headline();
testLevel2Headline();
testLevel3Headline();
testLevel5Headline();
testBodyTextNotMatched();
testStarsWithoutSpace();
testEmptyLine();
testIndentedStars();
testListItemNotMatched();
testHeadlineWithTags();
console.log("All headlineBoldDecorations tests passed ✓\n");
