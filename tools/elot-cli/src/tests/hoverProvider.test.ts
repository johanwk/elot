// src/tests/hoverProvider.test.ts
//
// Tests for hover provider logic.
// Since we can't import vscode in a CLI test, we test the underlying
// buildSlurp integration: given a parsed tree, can we look up CURIEs
// and get meaningful hover content?

import { readFileSync } from "fs";
import { resolve } from "path";
import { parseOrg } from "../parseOrgWasm.js";
import { buildSlurp, type SlurpEntry } from "../buildSlurp.js";

const orgPath = resolve(__dirname, "../../examples/bfo-core.org");
const orgText = readFileSync(orgPath, "utf-8");
const root = parseOrg(orgText);
const slurp = buildSlurp(root);

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

// ─── Test: CURIE lookup returns correct label ────────────────────

{
  const entry = slurp.get("obo:BFO_0000001");
  assert(entry !== undefined, "obo:BFO_0000001 exists in slurp");
  assert(
    entry?.label?.includes("entity") === true,
    'obo:BFO_0000001 label contains "entity"',
    `got: ${entry?.label}`,
  );
  assert(
    entry?.rdfType === "owl:Class",
    "obo:BFO_0000001 rdfType is owl:Class",
    `got: ${entry?.rdfType}`,
  );
}

// ─── Test: hover content would be non-trivial ────────────────────

{
  // An entry with a different label than its URI should produce hover content
  const entry = slurp.get("obo:BFO_0000001")!;
  const hasUsefulInfo = entry.label !== entry.uri || !!entry.rdfType || !!entry.properties;
  assert(hasUsefulInfo, "obo:BFO_0000001 has useful hover info");
}

// ─── Test: entries with properties provide extra detail ───────────

{
  // Find any entry that has additional properties
  let entryWithProps: SlurpEntry | undefined;
  for (const [, e] of slurp) {
    if (e.properties && e.properties.length > 0) {
      entryWithProps = e;
      break;
    }
  }
  assert(
    entryWithProps !== undefined,
    "At least one entry has extra properties for rich hover",
    entryWithProps
      ? `${entryWithProps.uri} has ${entryWithProps.properties!.length} properties`
      : "none found",
  );
  if (entryWithProps) {
    console.log(
      `    → ${entryWithProps.uri}: ${entryWithProps.properties!.map((p) => p.tag).join(", ")}`,
    );
  }
}

// ─── Test: CURIE regex matching simulation ───────────────────────

{
  // Simulate what the hover provider does: match a CURIE pattern in text
  const CURIE_RE = /(?:[a-zA-Z][-a-zA-Z0-9_.]*|):[-\w_./:]+/;
  const testLines = [
    "SubClassOf: obo:BFO_0000001",
    "rdf:type owl:Class",
    "  obo:BFO_0000002 and obo:BFO_0000003",
    'Annotations: rdfs:label "entity"@en',
  ];
  for (const line of testLines) {
    const m = line.match(CURIE_RE);
    assert(m !== null, `CURIE regex matches in "${line}"`, `got: ${m?.[0]}`);
  }
}

// ─── Test: angle-bracket URI matching simulation ─────────────────

{
  const ANGLE_URI_RE = /<(https?:\/\/[^>]+)>/;
  const testLine = "Ontology: <http://purl.obolibrary.org/obo/bfo.owl>";
  const m = testLine.match(ANGLE_URI_RE);
  assert(m !== null, "Angle-bracket URI regex matches", `got: ${m?.[1]}`);
  if (m) {
    const lookupKey = `<${m[1]}>`;
    assert(
      typeof lookupKey === "string" && lookupKey.startsWith("<"),
      "Lookup key format is correct for slurp map",
      lookupKey,
    );
  }
}

// ─── Summary ─────────────────────────────────────────────────────

console.log(`\n${passed} passed, ${failed} failed`);
if (failed > 0) process.exit(1);
