// src/tests/definitionProvider.test.ts
//
// Tests for the definition provider logic.
// Since we can't import vscode in a CLI test, we test the underlying
// headline scanning and entity-matching logic directly.

import { readFileSync } from "fs";
import { resolve } from "path";
import { entityFromHeader } from "../entityFromHeader.js";

const orgPath = resolve(__dirname, "../../examples/bfo-core.org");
const orgText = readFileSync(orgPath, "utf-8");
const lines = orgText.split("\n");

let passed = 0;
let failed = 0;

function assert(condition: boolean, name: string, detail?: string) {
  if (condition) {
    passed++;
    console.log(`  ✓ ${name}`);
  } else {
    failed++;
    console.error(`  ✗ ${name}${detail ? ": " + detail : ""}`);
  }
}

// ─── Test: build a definition index from headlines ───────────────

const HEADLINE_RE = /^(\*+)\s+(.+)$/;

function buildDefinitionIndex(text: string): Map<string, number> {
  const index = new Map<string, number>();
  const fileLines = text.split("\n");
  for (let i = 0; i < fileLines.length; i++) {
    const m = fileLines[i].match(HEADLINE_RE);
    if (!m) continue;
    const entity = entityFromHeader(m[2]);
    if (entity && !index.has(entity.id)) {
      index.set(entity.id, i);
    }
  }
  return index;
}

const index = buildDefinitionIndex(orgText);

// ─── Test: known CURIEs have definitions ─────────────────────────

{
  assert(
    index.has("obo:BFO_0000001"),
    "obo:BFO_0000001 is in the definition index",
  );
  assert(
    index.has("obo:BFO_0000002"),
    "obo:BFO_0000002 is in the definition index",
  );
}

// ─── Test: definition points to a headline line ──────────────────

{
  const lineNum = index.get("obo:BFO_0000001");
  assert(lineNum !== undefined, "obo:BFO_0000001 has a line number");
  if (lineNum !== undefined) {
    const line = lines[lineNum];
    assert(
      line.startsWith("*"),
      `Line ${lineNum} is a headline`,
      `got: "${line.substring(0, 60)}"`,
    );
    assert(
      line.includes("BFO_0000001"),
      `Headline contains BFO_0000001`,
      `got: "${line.substring(0, 80)}"`,
    );
  }
}

// ─── Test: index has multiple entries ────────────────────────────

{
  assert(
    index.size > 10,
    `Definition index has many entries (${index.size})`,
  );
  console.log(`    → ${index.size} entity definitions found`);
}

// ─── Test: non-entity headings are not in the index ──────────────

{
  // "Prefixes" is a heading but not an entity
  let hasPrefixes = false;
  for (const [key] of index) {
    if (key.toLowerCase().includes("prefixes")) {
      hasPrefixes = true;
      break;
    }
  }
  assert(
    !hasPrefixes,
    "Non-entity heading 'Prefixes' is not in definition index",
  );
}

// ─── Test: entityFromHeader extracts from parenthesised CURIEs ───

{
  const result = entityFromHeader('"entity"@en (obo:BFO_0000001)');
  assert(result !== null, 'entityFromHeader parses "entity"@en (obo:BFO_0000001)');
  assert(
    result?.id === "obo:BFO_0000001",
    "Extracted CURIE is obo:BFO_0000001",
    `got: ${result?.id}`,
  );
  assert(
    result?.label === '"entity"@en',
    'Label is "entity"@en',
    `got: ${result?.label}`,
  );
}

// ─── Test: CURIE at start of line ────────────────────────────────

{
  const result = entityFromHeader("obo:BFO_0000001");
  assert(result !== null, "entityFromHeader parses bare CURIE");
  assert(
    result?.id === "obo:BFO_0000001",
    "Bare CURIE matches",
    `got: ${result?.id}`,
  );
}

// ─── Test: heading with no entity returns null ───────────────────

{
  const result = entityFromHeader("Prefixes");
  assert(
    result === null,
    "entityFromHeader returns null for non-entity heading",
  );
}

// ─── Summary ─────────────────────────────────────────────────────

console.log(`\n${passed} passed, ${failed} failed`);
if (failed > 0) process.exit(1);
