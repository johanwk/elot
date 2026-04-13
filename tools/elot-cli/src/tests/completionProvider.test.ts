// src/tests/completionProvider.test.ts
//
// Tests for the completion provider logic.
// Since we can't import vscode in a CLI test, we test the underlying
// slurp-map–to–completion-item mapping logic directly.

import { readFileSync } from "fs";
import { resolve } from "path";
import { parseOrg } from "../parseOrgWasm.js";
import { buildSlurp } from "../buildSlurp.js";
import type { SlurpEntry } from "../buildSlurp.js";

const orgPath = resolve(__dirname, "../../examples/bfo-core.org");
const orgText = readFileSync(orgPath, "utf-8");

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

// ─── Build slurp map (same as the provider does) ─────────────────

const root = parseOrg(orgText);
const slurpMap = buildSlurp(root);

// ─── Test: slurp map has entries ─────────────────────────────────

{
  assert(
    slurpMap.size > 10,
    `Slurp map has many entries (${slurpMap.size})`,
  );
  console.log(`    → ${slurpMap.size} entities available for completion`);
}

// ─── Test: known entity has label, URI, and rdfType ──────────────

{
  const entry = slurpMap.get("obo:BFO_0000001");
  assert(entry !== undefined, "obo:BFO_0000001 is in the slurp map");
  if (entry) {
    assert(
      entry.label !== undefined && entry.label.length > 0,
      "Entry has a label",
      `got: "${entry.label}"`,
    );
    assert(
      entry.uri === "obo:BFO_0000001",
      "Entry URI matches key",
      `got: "${entry.uri}"`,
    );
    assert(
      entry.rdfType !== undefined,
      "Entry has an rdfType",
      `got: "${entry.rdfType}"`,
    );
  }
}

// ─── Test: label differs from URI (for labelled entities) ────────

{
  const entry = slurpMap.get("obo:BFO_0000001");
  if (entry) {
    assert(
      entry.label !== entry.uri,
      "Label is not identical to the URI",
      `label: "${entry.label}", uri: "${entry.uri}"`,
    );
  }
}

// ─── Test: completion item shape ─────────────────────────────────
// Simulate the mapping from SlurpEntry to completion item fields.

{
  const entry = slurpMap.get("obo:BFO_0000001");
  if (entry) {
    // filterText should contain both label and URI
    const filterText = `${entry.label} ${entry.uri}`;
    assert(
      filterText.includes("obo:BFO_0000001"),
      "filterText contains the CURIE",
    );
    assert(
      filterText.includes(entry.label),
      "filterText contains the label",
    );

    // insertText should be the CURIE
    const insertText = entry.uri;
    assert(
      insertText === "obo:BFO_0000001",
      "insertText is the CURIE",
      `got: "${insertText}"`,
    );
  }
}

// ─── Test: rdfType distribution ──────────────────────────────────
// Check that we have entries of various types (for icon mapping).

{
  const types = new Set<string>();
  for (const entry of slurpMap.values()) {
    if (entry.rdfType) types.add(entry.rdfType);
  }
  console.log(`    → RDF types found: ${[...types].join(", ")}`);
  assert(
    types.has("owl:Class"),
    "Has owl:Class entities",
  );
}

// ─── Test: entries with properties ───────────────────────────────
// Some entities should have description properties for the doc panel.

{
  let withProps = 0;
  for (const entry of slurpMap.values()) {
    if (entry.properties && entry.properties.length > 0) {
      withProps++;
    }
  }
  console.log(`    → ${withProps} entities have description properties`);
  assert(
    withProps > 0,
    "Some entities have description properties for the documentation panel",
  );
}

// ─── Test: kindForType mapping (inline) ──────────────────────────
// Reproduce the kind mapping to verify correctness.

{
  function kindForType(rdfType?: string): string {
    switch (rdfType) {
      case "owl:Class": return "Class";
      case "owl:ObjectProperty":
      case "owl:DatatypeProperty":
      case "owl:AnnotationProperty": return "Property";
      case "owl:NamedIndividual": return "Value";
      case "owl:Ontology": return "Module";
      case "rdfs:Datatype": return "TypeParameter";
      default: return "Reference";
    }
  }

  assert(kindForType("owl:Class") === "Class", "owl:Class → Class icon");
  assert(kindForType("owl:ObjectProperty") === "Property", "owl:ObjectProperty → Property icon");
  assert(kindForType("owl:NamedIndividual") === "Value", "owl:NamedIndividual → Value icon");
  assert(kindForType("owl:Ontology") === "Module", "owl:Ontology → Module icon");
  assert(kindForType(undefined) === "Reference", "undefined type → Reference icon");
}

// ─── Summary ─────────────────────────────────────────────────────

console.log(`\n${passed} passed, ${failed} failed`);
if (failed > 0) process.exit(1);
