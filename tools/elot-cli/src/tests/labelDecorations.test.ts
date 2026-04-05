// src/tests/labelDecorations.test.ts
//
// Unit-level tests for the label decoration logic.
//
// We can't test the actual VS Code decoration API here (no editor),
// but we CAN test:
//   1. The CURIE regex finds CURIEs in realistic Org/OMN text
//   2. The buildSlurp→label lookup produces correct replacements
//   3. The toggle state management works

import { readFileSync } from "fs";
import { resolve, dirname } from "path";
import { fileURLToPath } from "url";
import { parseOrg } from "../parseOrgWasm.js";
import { buildSlurp, buildLabelMap } from "../buildSlurp.js";

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

// ─── CURIE regex (duplicated for testing; same as in labelDecorations.ts) ────
const CURIE_GLOBAL_RE = /(?:[a-zA-Z][-a-zA-Z0-9_.]*|):[-\w_./]+/g;

// ─── Test infrastructure ─────────────────────────────────────────

let passed = 0;
let failed = 0;

function assert(condition: boolean, msg: string) {
  if (condition) {
    console.log(`  ✓ ${msg}`);
    passed++;
  } else {
    console.error(`  ✗ ${msg}`);
    failed++;
  }
}

// ─── Load test fixture ──────────────────────────────────────────

const orgPath = resolve(__dirname, "../../examples/bfo-core.org");
const orgText = readFileSync(orgPath, "utf-8");
const root = parseOrg(orgText);
const slurpMap = buildSlurp(root);
const labelMap = buildLabelMap(root);

console.log(`\n  Loaded bfo-core.org: ${slurpMap.size} entities\n`);

// ─── Test 1: CURIE regex finds CURIEs in Org text ───────────────

{
  const testLines = [
    "*** obo:BFO_0000001 entity",
    "  - rdf:type :: owl:Class",
    "  - rdfs:label :: \"entity\"@en",
    "SubClassOf: obo:BFO_0000001",
    "  obo:BFO_0000002 and obo:BFO_0000003",
  ];

  for (const line of testLines) {
    CURIE_GLOBAL_RE.lastIndex = 0;
    const matches: string[] = [];
    let m: RegExpExecArray | null;
    while ((m = CURIE_GLOBAL_RE.exec(line)) !== null) {
      matches.push(m[0]);
    }
    assert(matches.length > 0, `CURIE regex finds match(es) in: "${line}" → [${matches.join(", ")}]`);
  }
}

// ─── Test 2: All CURIEs in the real Org file that map to a label ─

{
  CURIE_GLOBAL_RE.lastIndex = 0;
  let totalMatches = 0;
  let labelledMatches = 0;
  let m: RegExpExecArray | null;
  while ((m = CURIE_GLOBAL_RE.exec(orgText)) !== null) {
    totalMatches++;
    if (labelMap.has(m[0])) {
      labelledMatches++;
    }
  }
  console.log(`  CURIE occurrences in bfo-core.org: ${totalMatches} total, ${labelledMatches} have labels`);
  assert(totalMatches > 0, `Found CURIE occurrences in Org text (${totalMatches})`);
  assert(labelledMatches > 0, `Some CURIEs have labels (${labelledMatches})`);
}

// ─── Test 3: Label differs from URI (replacement would be visible) ─

{
  let differentCount = 0;
  for (const [uri, entry] of slurpMap) {
    if (entry.label !== entry.uri) {
      differentCount++;
    }
  }
  assert(
    differentCount > 0,
    `At least some entities have labels different from their URI (${differentCount}/${slurpMap.size})`,
  );
}

// ─── Test 4: Known entity produces expected decoration ───────────

{
  const entry = slurpMap.get("obo:BFO_0000001");
  assert(!!entry, "obo:BFO_0000001 found in slurp map");
  if (entry) {
    assert(
      entry.label.includes("entity"),
      `obo:BFO_0000001 label contains "entity": "${entry.label}"`,
    );
    assert(
      entry.label !== "obo:BFO_0000001",
      "Label differs from CURIE (decoration would be useful)",
    );
  }
}

// ─── Test 5: Count decoratable CURIEs (would be replaced) ──────

{
  CURIE_GLOBAL_RE.lastIndex = 0;
  let decoratable = 0;
  const seen = new Set<string>();
  let m: RegExpExecArray | null;
  while ((m = CURIE_GLOBAL_RE.exec(orgText)) !== null) {
    const entry = slurpMap.get(m[0]);
    if (entry && entry.label !== entry.uri) {
      decoratable++;
      if (!seen.has(m[0])) {
        seen.add(m[0]);
      }
    }
  }
  console.log(`  Decoratable CURIE occurrences: ${decoratable} (${seen.size} unique)`);
  assert(decoratable > 5, `Many decoratable CURIE occurrences (${decoratable})`);

  // Show a few examples
  console.log("\n  Sample label replacements:");
  let count = 0;
  for (const curie of seen) {
    if (count >= 5) break;
    const entry = slurpMap.get(curie);
    if (entry) {
      console.log(`    ${curie} → ${entry.label}`);
    }
    count++;
  }
}

// ─── Summary ────────────────────────────────────────────────────

console.log(`\n${passed} passed, ${failed} failed\n`);
process.exit(failed > 0 ? 1 : 0);
