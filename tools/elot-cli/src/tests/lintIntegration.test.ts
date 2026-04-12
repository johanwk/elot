// src/tests/lintIntegration.test.ts
//
// Stage 7 integration tests: verify that collectAllLintErrors aggregates
// diagnostics from all 8 checkers correctly.
//
// These tests construct ElotNode trees with known issues and verify
// the combined output.

import type { ElotNode, DescriptionItem, PrefixEntry } from "../types.js";
import { collectAllLintErrors } from "../collectLintErrors.js";

// ─── Helpers ─────────────────────────────────────────────────────

function assert(condition: boolean, msg: string) {
  if (!condition) {
    console.error(`FAIL: ${msg}`);
    process.exit(1);
  }
}

function assertCount(
  diagnostics: { message: string; severity: string }[],
  expected: number,
  label: string,
) {
  assert(
    diagnostics.length === expected,
    `${label}: expected ${expected} diagnostics, got ${diagnostics.length}` +
      (diagnostics.length > 0
        ? ` -- ${diagnostics.map((d) => `[${d.severity}] ${d.message}`).join("; ")}`
        : ""),
  );
}

function assertHas(
  diagnostics: { message: string; severity: string }[],
  severity: string,
  substring: string,
  label: string,
) {
  const found = diagnostics.some(
    (d) => d.severity === severity && d.message.includes(substring),
  );
  assert(
    found,
    `${label}: expected a ${severity} containing "${substring}" -- got: ${JSON.stringify(diagnostics.map((d) => `[${d.severity}] ${d.message}`))}`,
  );
}

function assertNotHas(
  diagnostics: { message: string; severity: string }[],
  substring: string,
  label: string,
) {
  const found = diagnostics.some((d) => d.message.includes(substring));
  assert(
    !found,
    `${label}: did NOT expect diagnostic containing "${substring}" -- got: ${JSON.stringify(diagnostics.map((d) => `[${d.severity}] ${d.message}`))}`,
  );
}

// ─── Tree builders ──────────────────────────────────────────────

function makeRoot(children: ElotNode[]): ElotNode {
  return { level: 0, title: "ROOT", children };
}

const STANDARD_PREFIXES: PrefixEntry[] = [
  { prefix: "pizza:", uri: "http://example.org/pizza#" },
  { prefix: "owl:", uri: "http://www.w3.org/2002/07/owl#" },
  { prefix: "rdfs:", uri: "http://www.w3.org/2000/01/rdf-schema#" },
  { prefix: "skos:", uri: "http://www.w3.org/2004/02/skos/core#" },
  { prefix: "obo:", uri: "http://purl.obolibrary.org/obo/" },
];

/**
 * Build a well-formed ontology tree with all 7 required sections
 * and a prefix table. Optionally inject entity nodes into the
 * class hierarchy section.
 */
function makeValidOntology(
  classEntities: ElotNode[] = [],
  extraSections: ElotNode[] = [],
): ElotNode {
  const prefixDefs: ElotNode = {
    level: 2,
    title: "Prefixes",
    prefixdefs: true,
    prefixes: STANDARD_PREFIXES,
  };

  const ontologyDecl: ElotNode = {
    level: 2,
    title: "Ontology declaration",
    id: "pizza-ontology-declaration",
    resourcedefs: true,
  };

  const datatypes: ElotNode = {
    level: 2,
    title: "Datatypes",
    id: "pizza-datatypes",
    resourcedefs: true,
  };

  const classHierarchy: ElotNode = {
    level: 2,
    title: "Class hierarchy",
    id: "pizza-class-hierarchy",
    resourcedefs: true,
    children: classEntities.length > 0 ? classEntities : undefined,
  };

  const objectProps: ElotNode = {
    level: 2,
    title: "Object property hierarchy",
    id: "pizza-object-property-hierarchy",
    resourcedefs: true,
  };

  const dataProps: ElotNode = {
    level: 2,
    title: "Data property hierarchy",
    id: "pizza-data-property-hierarchy",
    resourcedefs: true,
  };

  const annotProps: ElotNode = {
    level: 2,
    title: "Annotation property hierarchy",
    id: "pizza-annotation-property-hierarchy",
    resourcedefs: true,
  };

  const individuals: ElotNode = {
    level: 2,
    title: "Individuals",
    id: "pizza-individuals",
    resourcedefs: true,
  };

  const ontology: ElotNode = {
    level: 1,
    title: "Pizza Ontology",
    elotContextType: "ontology",
    elotContextLocalname: "pizza",
    elotDefaultPrefix: "pizza:",
    id: "pizza",
    tangleTargetOmn: "pizza.omn",
    children: [
      prefixDefs,
      ontologyDecl,
      datatypes,
      classHierarchy,
      objectProps,
      dataProps,
      annotProps,
      individuals,
      ...extraSections,
    ],
  };

  return makeRoot([ontology]);
}

function makeEntity(
  curie: string,
  descriptions: DescriptionItem[] = [],
  rdfType: string = "owl:Class",
): ElotNode {
  return {
    level: 3,
    title: `Entity (${curie})`,
    uri: curie,
    label: "Entity",
    rdfType,
    descriptions,
  };
}

// ─── Tests ──────────────────────────────────────────────────────

function testValidOntology() {
  const root = makeValidOntology([
    makeEntity("pizza:Food", [
      { tag: "SubClassOf", value: "owl:Thing" },
      { tag: "rdfs:comment", value: "A food item" },
    ]),
    makeEntity("pizza:Topping", [
      { tag: "SubClassOf", value: "pizza:Food" },
    ]),
  ]);
  const diags = collectAllLintErrors(root);
  assertCount(diags, 0, "valid ontology");
  console.log("  valid ontology (no errors): OK");
}

function testNoOntologyHeading() {
  const root = makeRoot([
    { level: 1, title: "Random Document" },
  ]);
  const diags = collectAllLintErrors(root);
  // Checker #1: no ontology heading → 1 error
  assertHas(diags, "error", "No top-level heading", "no ontology");
  console.log("  no ontology heading (error from checker #1): OK");
}

function testMissingId() {
  // Ontology heading without :ID:
  const ontology: ElotNode = {
    level: 1,
    title: "Pizza Ontology",
    elotContextType: "ontology",
    elotContextLocalname: "pizza",
    tangleTargetOmn: "pizza.omn",
    children: [
      {
        level: 2,
        title: "Prefixes",
        prefixdefs: true,
        prefixes: STANDARD_PREFIXES,
      },
    ],
  };
  const root = makeRoot([ontology]);
  const diags = collectAllLintErrors(root);
  assertHas(diags, "error", "missing :ID:", "missing ID");
  console.log("  missing :ID: on ontology heading (checker #2): OK");
}

function testMissingPrefixTable() {
  // Ontology with no prefixdefs node
  const ontology: ElotNode = {
    level: 1,
    title: "Pizza Ontology",
    elotContextType: "ontology",
    elotContextLocalname: "pizza",
    id: "pizza",
    tangleTargetOmn: "pizza.omn",
  };
  const root = makeRoot([ontology]);
  const diags = collectAllLintErrors(root);
  assertHas(diags, "error", "Prefix table is missing", "missing prefix table");
  console.log("  missing prefix table (checker #3): OK");
}

function testMissingSections() {
  // Ontology with prefix table but no sections
  const ontology: ElotNode = {
    level: 1,
    title: "Pizza Ontology",
    elotContextType: "ontology",
    elotContextLocalname: "pizza",
    elotDefaultPrefix: "pizza:",
    id: "pizza",
    tangleTargetOmn: "pizza.omn",
    children: [
      {
        level: 2,
        title: "Prefixes",
        prefixdefs: true,
        prefixes: STANDARD_PREFIXES,
      },
    ],
  };
  const root = makeRoot([ontology]);
  const diags = collectAllLintErrors(root);
  // Should get 7 warnings for missing sections
  const sectionWarnings = diags.filter(
    (d) => d.severity === "warning" && d.message.includes("Missing section"),
  );
  assert(
    sectionWarnings.length === 7,
    `missing sections: expected 7 warnings, got ${sectionWarnings.length}`,
  );
  console.log("  missing all 7 sections (checker #4): OK");
}

function testHeadingWithNoUri() {
  const root = makeValidOntology([
    // Entity heading with no URI — should trigger checker #5
    {
      level: 3,
      title: "Missing identifier",
      // no uri, no tags
    },
  ]);
  const diags = collectAllLintErrors(root);
  assertHas(diags, "error", "No identifier found", "no URI");
  console.log("  heading with no URI (checker #5): OK");
}

function testUnknownAnnotationProperty() {
  const root = makeValidOntology([
    makeEntity("pizza:Food", [
      { tag: "foo:unknownProp", value: "some value" },
    ]),
  ]);
  const diags = collectAllLintErrors(root);
  assertHas(
    diags,
    "warning",
    "Unknown or invalid annotation property: foo:unknownProp",
    "unknown annotation",
  );
  console.log("  unknown annotation property (checker #6): OK");
}

function testUnknownCurieInAxiom() {
  const root = makeValidOntology([
    makeEntity("pizza:Food", [
      { tag: "SubClassOf", value: "pizza:NonExistent" },
    ]),
  ]);
  const diags = collectAllLintErrors(root);
  assertHas(
    diags,
    "warning",
    "Unknown CURIE in axiom: pizza:NonExistent",
    "unknown CURIE in axiom",
  );
  console.log("  unknown CURIE in axiom (checker #7): OK");
}

function testWrongKeywordForSection() {
  const root = makeValidOntology([
    makeEntity("pizza:Food", [
      { tag: "Domain", value: "pizza:Food" },
    ]),
  ]);
  const diags = collectAllLintErrors(root);
  assertHas(
    diags,
    "error",
    '"Domain" is not valid in Classes section',
    "wrong keyword",
  );
  console.log("  wrong keyword for section (checker #8): OK");
}

function testMultipleIssuesCombined() {
  // Tree with issues triggering multiple checkers simultaneously
  const root = makeValidOntology([
    // Checker #5: no URI
    { level: 3, title: "Bad heading no URI" },
    // Checker #6: unknown annotation property
    makeEntity("pizza:Food", [
      { tag: "foo:bogus", value: "val" },
    ]),
    // Checker #7: unknown CURIE in axiom
    makeEntity("pizza:Topping", [
      { tag: "SubClassOf", value: "pizza:Missing" },
    ]),
    // Checker #8: wrong keyword
    makeEntity("pizza:Drink", [
      { tag: "Domain", value: "pizza:Food" },
    ]),
  ]);
  const diags = collectAllLintErrors(root);

  assertHas(diags, "error", "No identifier found", "combined #5");
  assertHas(diags, "warning", "Unknown or invalid annotation property: foo:bogus", "combined #6");
  assertHas(diags, "warning", "Unknown CURIE in axiom: pizza:Missing", "combined #7");
  assertHas(diags, "error", '"Domain" is not valid in Classes', "combined #8");

  console.log("  multiple issues combined (checkers #5–#8): OK");
}

function testNodeclareSkipped() {
  const root = makeValidOntology([
    // Tagged :nodeclare: — should NOT trigger checker #5
    {
      level: 3,
      title: "Helper heading",
      tags: ["nodeclare"],
    },
  ]);
  const diags = collectAllLintErrors(root);
  assertNotHas(diags, "No identifier found", "nodeclare skipped");
  console.log("  :nodeclare: heading skipped (checker #5): OK");
}

function testKnownAnnotationPropertyPasses() {
  const root = makeValidOntology([
    makeEntity("pizza:Food", [
      { tag: "rdfs:comment", value: "A food" },
      { tag: "rdfs:seeAlso", value: "http://example.org" },
    ]),
  ]);
  const diags = collectAllLintErrors(root);
  // rdfs:comment and rdfs:seeAlso are known annotation properties — no warnings
  assertNotHas(diags, "Unknown or invalid annotation property", "known annot props");
  console.log("  known annotation properties (rdfs:comment, rdfs:seeAlso) pass: OK");
}

function testBuiltinCuriesInAxiom() {
  const root = makeValidOntology([
    makeEntity("pizza:Food", [
      { tag: "SubClassOf", value: "owl:Thing" },
      { tag: "EquivalentTo", value: "xsd:string" },
    ]),
  ]);
  const diags = collectAllLintErrors(root);
  assertNotHas(diags, "Unknown CURIE in axiom", "builtin CURIEs");
  console.log("  built-in CURIEs (owl:Thing, xsd:string) pass: OK");
}

function testDiagnosticsFromAllCheckers() {
  // Construct a tree that triggers at least one diagnostic from each checker:
  // #1: skip (we have an ontology heading)
  // #2: missing tangle target
  // #3: empty prefix table
  // #4: missing sections (only 1 section provided)
  // #5: heading with no URI
  // #6: unknown annotation property
  // #7: unknown CURIE in axiom
  // #8: wrong keyword for section

  const ontology: ElotNode = {
    level: 1,
    title: "Test Ontology",
    elotContextType: "ontology",
    elotContextLocalname: "test",
    id: "test",
    // No tangleTargetOmn → checker #2 error
    children: [
      {
        level: 2,
        title: "Prefixes",
        prefixdefs: true,
        // Empty prefixes → checker #3 error
        prefixes: [],
      },
      {
        level: 2,
        title: "Class hierarchy",
        id: "test-class-hierarchy",
        resourcedefs: true,
        children: [
          // No URI → checker #5 error
          { level: 3, title: "Bad heading" },
          // Unknown annotation property → checker #6 warning
          {
            level: 3,
            title: "Entity (test:A)",
            uri: "test:A",
            label: "A",
            rdfType: "owl:Class",
            descriptions: [
              { tag: "foo:weird", value: "val" },
              // Wrong keyword → checker #8 error
              { tag: "Domain", value: "test:B" },
              // Unknown CURIE → checker #7 warning
              { tag: "SubClassOf", value: "test:Nonexistent" },
            ],
          },
        ],
      },
      // Only 1 of 7 sections → 6 missing section warnings from checker #4
    ],
  };
  const root = makeRoot([ontology]);
  const diags = collectAllLintErrors(root);

  // Checker #2: missing tangle target
  assertHas(diags, "error", ":tangle missing or invalid", "all-checkers #2");
  // Checker #3: empty prefix table
  assertHas(diags, "error", "Prefix table is empty", "all-checkers #3");
  // Checker #4: missing sections (6 of 7)
  const sectionWarnings = diags.filter(
    (d) => d.message.includes("Missing section"),
  );
  assert(
    sectionWarnings.length === 6,
    `all-checkers #4: expected 6 missing-section warnings, got ${sectionWarnings.length}`,
  );
  // Checker #5: no URI
  assertHas(diags, "error", "No identifier found", "all-checkers #5");
  // Checker #6: unknown annotation property
  assertHas(diags, "warning", "Unknown or invalid annotation property: foo:weird", "all-checkers #6");
  // Checker #7: unknown CURIE
  assertHas(diags, "warning", "Unknown CURIE in axiom: test:Nonexistent", "all-checkers #7");
  // Checker #8: wrong keyword
  assertHas(diags, "error", '"Domain" is not valid in Classes', "all-checkers #8");

  console.log("  diagnostics from all checkers combined: OK");
}

// ─── Main ───────────────────────────────────────────────────────

function main() {
  let passed = 0;

  console.log("\n=== Integration: collectAllLintErrors ===");

  testValidOntology(); passed++;
  testNoOntologyHeading(); passed++;
  testMissingId(); passed++;
  testMissingPrefixTable(); passed++;
  testMissingSections(); passed++;
  testHeadingWithNoUri(); passed++;
  testUnknownAnnotationProperty(); passed++;
  testUnknownCurieInAxiom(); passed++;
  testWrongKeywordForSection(); passed++;
  testMultipleIssuesCombined(); passed++;
  testNodeclareSkipped(); passed++;
  testKnownAnnotationPropertyPasses(); passed++;
  testBuiltinCuriesInAxiom(); passed++;
  testDiagnosticsFromAllCheckers(); passed++;

  console.log(`\n${passed} passed, 0 failed`);
}

main();
