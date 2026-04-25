// src/tests/db/dbHoverToken.test.ts
//
// Step 2.3.2: detectTokenAtOffset coverage.

import { detectTokenAtOffset } from "../../dbHover/token.js";

let passed = 0;
let failed = 0;

function test(name: string, fn: () => void): void {
  try {
    fn();
    passed++;
  } catch (err) {
    failed++;
    console.error(
      `FAIL ${name}:`,
      err instanceof Error ? err.stack ?? err.message : err,
    );
  }
}

function eq<T>(a: T, e: T, msg = ""): void {
  if (a !== e)
    throw new Error(`${msg}\n  got:      ${String(a)}\n  expected: ${String(e)}`);
}

function tru(v: unknown, msg = ""): void {
  if (!v) throw new Error(msg);
}

// ── CURIE ────────────────────────────────────────────────────────
test("CURIE: simple obo:BFO_0000001 at middle of token", () => {
  const line = "see obo:BFO_0000001 for details";
  const hit = detectTokenAtOffset(line, 8);
  tru(hit, "found");
  eq(hit!.kind, "curie");
  eq(hit!.token, "obo:BFO_0000001");
  eq(hit!.start, 4);
  eq(hit!.end, 19);
});

test("CURIE: at start of token", () => {
  const line = "obo:BFO_0000001 ok";
  const hit = detectTokenAtOffset(line, 0);
  tru(hit);
  eq(hit!.token, "obo:BFO_0000001");
});

test("CURIE: at end of token", () => {
  const line = "obo:BFO_0000001 ok";
  const hit = detectTokenAtOffset(line, 15);
  tru(hit);
  eq(hit!.token, "obo:BFO_0000001");
});

test("CURIE: empty prefix :Widget", () => {
  const line = "use :Widget here";
  const hit = detectTokenAtOffset(line, 5);
  tru(hit);
  eq(hit!.kind, "curie");
  eq(hit!.token, ":Widget");
});

test("CURIE: dotted prefix dc11:contributor", () => {
  const line = "set dc11:contributor here";
  const hit = detectTokenAtOffset(line, 8);
  tru(hit);
  eq(hit!.token, "dc11:contributor");
});

test("CURIE: not detected when cursor on whitespace", () => {
  const line = "  obo:Widget  ";
  const hit = detectTokenAtOffset(line, 0);
  eq(hit, null);
});

// ── Angle-bracketed IRI ──────────────────────────────────────────
test("Angle IRI: <http://...> strips brackets", () => {
  const line = "see <http://example.org/Widget> here";
  const hit = detectTokenAtOffset(line, 10);
  tru(hit);
  eq(hit!.kind, "angle-iri");
  eq(hit!.token, "http://example.org/Widget");
  eq(hit!.start, 4);
  eq(hit!.end, 31);
});

test("Angle IRI: cursor on opening <", () => {
  const line = "<http://a.b/c>";
  const hit = detectTokenAtOffset(line, 0);
  tru(hit);
  eq(hit!.kind, "angle-iri");
  eq(hit!.token, "http://a.b/c");
});

test("Angle IRI: cursor on closing >", () => {
  const line = "<http://a.b/c>";
  const hit = detectTokenAtOffset(line, 13);
  tru(hit);
  eq(hit!.kind, "angle-iri");
});

// ── Bare IRI ─────────────────────────────────────────────────────
test("Bare IRI: http://... no brackets", () => {
  const line = "go to http://example.org/Widget today";
  const hit = detectTokenAtOffset(line, 12);
  tru(hit);
  eq(hit!.kind, "bare-iri");
  eq(hit!.token, "http://example.org/Widget");
});

test("Bare IRI: stops at trailing comma", () => {
  const line = "see http://example.org/A, then http://example.org/B";
  const hit = detectTokenAtOffset(line, 10);
  tru(hit);
  eq(hit!.token, "http://example.org/A");
});

test("Bare IRI: stops at trailing close-paren", () => {
  const line = "(http://example.org/X)";
  const hit = detectTokenAtOffset(line, 5);
  tru(hit);
  eq(hit!.token, "http://example.org/X");
});

// ── Resolution priority ──────────────────────────────────────────
test("Angle wins over bare when cursor inside <...>", () => {
  const line = "<http://example.org/Widget>";
  const hit = detectTokenAtOffset(line, 5);
  tru(hit);
  eq(hit!.kind, "angle-iri");
  eq(hit!.token, "http://example.org/Widget");
});

// ── Misses ────────────────────────────────────────────────────────
test("Empty line -> null", () => {
  eq(detectTokenAtOffset("", 0), null);
});

test("Plain text -> null", () => {
  eq(detectTokenAtOffset("hello world", 3), null);
});

test("Cursor past end -> null when no token there", () => {
  eq(detectTokenAtOffset("foo", 10), null);
});

console.log(`dbHoverToken tests: ${passed} passed, ${failed} failed`);
if (failed > 0) process.exit(1);
