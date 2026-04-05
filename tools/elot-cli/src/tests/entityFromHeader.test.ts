// src/tests/entityFromHeader.test.ts
//
// Unit tests for entityFromHeader — the heading title parser.
// Port of `elot-entity-from-header` from elot-tangle.el.

import { entityFromHeader, EntityInfo } from "../entityFromHeader.js";

interface TestCase {
  name: string;
  input: string;
  expected: EntityInfo | null;
}

const cases: TestCase[] = [
  // ── Full URIs ──────────────────────────────────────────────────
  {
    name: "Full URI at start of line (no angles)",
    input: "http://example.org/Ontology",
    expected: { id: "<http://example.org/Ontology>", label: null },
  },
  {
    name: "Full URI at start of line (with angles)",
    input: "<http://example.org/Ontology>",
    expected: { id: "<http://example.org/Ontology>", label: null },
  },
  {
    name: "Full URI in parentheses",
    input: 'My Ontology (http://example.org/Ontology)',
    expected: { id: "<http://example.org/Ontology>", label: "My Ontology" },
  },
  {
    name: "Full URI in parentheses with angles",
    input: 'My Ontology (<http://example.org/Ontology>)',
    expected: { id: "<http://example.org/Ontology>", label: "My Ontology" },
  },

  // ── CURIEs ─────────────────────────────────────────────────────
  {
    name: "CURIE at start of line",
    input: "ex:MyClass",
    expected: { id: "ex:MyClass", label: null },
  },
  {
    name: "CURIE at start of line (default prefix)",
    input: ":MyClass",
    expected: { id: ":MyClass", label: null },
  },
  {
    name: "CURIE in parentheses",
    input: '"entity"@en (obo:BFO_0000001)',
    expected: { id: "obo:BFO_0000001", label: '"entity"@en' },
  },
  {
    name: "CURIE with underscores in parentheses",
    input: '"continuant"@en (obo:BFO_0000002)',
    expected: { id: "obo:BFO_0000002", label: '"continuant"@en' },
  },
  {
    name: "CURIE with dots and hyphens",
    input: '"my thing"@en (ex:My-Class_v1.0)',
    expected: { id: "ex:My-Class_v1.0", label: '"my thing"@en' },
  },
  {
    name: "CURIE with slash in local name",
    input: "obo:bfo.owl",
    expected: { id: "obo:bfo.owl", label: null },
  },

  // ── Ontology declarations (two identifiers) ────────────────────
  {
    name: "CURIE + URI in parentheses (ontology + version)",
    input: "bfo.owl ontology (obo:bfo.owl <http://purl.obolibrary.org/obo/bfo/2020/bfo-core.ttl>)",
    expected: {
      id: "obo:bfo.owl <http://purl.obolibrary.org/obo/bfo/2020/bfo-core.ttl>",
      label: "bfo.owl ontology",
    },
  },
  {
    name: "Two full URIs in parentheses",
    input: "My Ont (http://example.org/ont http://example.org/v1)",
    expected: {
      id: "<http://example.org/ont> <http://example.org/v1>",
      label: "My Ont",
    },
  },
  {
    name: "Two CURIEs in parentheses",
    input: "My Ont (ex:ont ex:v1)",
    expected: { id: "ex:ont ex:v1", label: "My Ont" },
  },

  // ── URN identifiers ────────────────────────────────────────────
  {
    name: "Bare URN",
    input: "<urn:isbn:0943396611>",
    expected: { id: "<urn:isbn:0943396611>", label: null },
  },
  {
    name: "URN in parentheses",
    input: "Some Book (<urn:isbn:0943396611>)",
    expected: { id: "<urn:isbn:0943396611>", label: "Some Book" },
  },

  // ── Edge cases ─────────────────────────────────────────────────
  {
    name: "No identifier found",
    input: "Disjointness clauses",
    expected: null,
  },
  {
    name: "Empty string",
    input: "",
    expected: null,
  },
  {
    name: "Label with special characters",
    input: '"Yongqun \\"Oliver\\" He" (ex:He)',
    expected: { id: "ex:He", label: '"Yongqun \\"Oliver\\" He"' },
  },
  {
    name: "Annotation property (bare CURIE at start)",
    input: "skos:altLabel",
    expected: { id: "skos:altLabel", label: null },
  },
  {
    name: "Datatype (bare CURIE at start)",
    input: "rdf:langString",
    expected: { id: "rdf:langString", label: null },
  },
  {
    name: "Heading with nodeclare content",
    input: "Disjointness clauses",
    expected: null,
  },

  // ── Real-world examples from bfo-core.org ──────────────────────
  {
    name: "BFO entity heading",
    input: '"entity"@en (obo:BFO_0000001)',
    expected: { id: "obo:BFO_0000001", label: '"entity"@en' },
  },
  {
    name: "BFO continuant heading",
    input: '"continuant"@en (obo:BFO_0000002)',
    expected: { id: "obo:BFO_0000002", label: '"continuant"@en' },
  },
  {
    name: "BFO ontology declaration heading",
    input: "bfo.owl ontology (obo:bfo.owl <http://purl.obolibrary.org/obo/bfo/2020/bfo-core.ttl>)",
    expected: {
      id: "obo:bfo.owl <http://purl.obolibrary.org/obo/bfo/2020/bfo-core.ttl>",
      label: "bfo.owl ontology",
    },
  },
  {
    name: "BFO concretizes property",
    input: '"concretizes"@en (obo:BFO_0000059)',
    expected: { id: "obo:BFO_0000059", label: '"concretizes"@en' },
  },
];

// ── Runner ──────────────────────────────────────────────────────────

let passed = 0;
let failed = 0;

for (const tc of cases) {
  const result = entityFromHeader(tc.input);

  const resultMatch =
    tc.expected === null
      ? result === null
      : result !== null &&
        result.id === tc.expected.id &&
        result.label === tc.expected.label;

  if (resultMatch) {
    console.log(`PASS ${tc.name}`);
    passed++;
  } else {
    console.error(`FAIL ${tc.name}`);
    console.error(`  input:    ${JSON.stringify(tc.input)}`);
    console.error(`  expected: ${JSON.stringify(tc.expected)}`);
    console.error(`  got:      ${JSON.stringify(result)}`);
    failed++;
  }
}

console.log(`\n${passed} passed, ${failed} failed out of ${cases.length}`);
if (failed > 0) process.exit(1);
