// src/tests/annotationValue.test.ts
//
// Tests for annotationStringOrUri and unprefixUri

import { annotationStringOrUri, unprefixUri } from "../annotationValue.js";

let passed = 0;
let failed = 0;
const total: string[] = [];

function assert(name: string, actual: string | null, expected: string | null) {
  total.push(name);
  if (actual === expected) {
    console.log(`PASS ${name}`);
    passed++;
  } else {
    console.log(`FAIL ${name}`);
    console.log(`  expected: ${JSON.stringify(expected)}`);
    console.log(`  actual:   ${JSON.stringify(actual)}`);
    failed++;
  }
}

// ── Build a prefix map for testing ──

const prefixes = new Map<string, string>([
  ["obo", "http://purl.obolibrary.org/obo/"],
  ["dc11", "http://purl.org/dc/elements/1.1/"],
  ["skos", "http://www.w3.org/2004/02/skos/core#"],
  ["rdfs", "http://www.w3.org/2000/01/rdf-schema#"],
  ["xsd", "http://www.w3.org/2001/XMLSchema#"],
  ["", "http://example.org/default#"],  // default prefix
]);

// ── unprefixUri tests ──

assert(
  "unprefixUri: expand known CURIE",
  unprefixUri("obo:BFO_0000001", prefixes),
  "<http://purl.obolibrary.org/obo/BFO_0000001>"
);

assert(
  "unprefixUri: expand default prefix",
  unprefixUri(":Foo", prefixes),
  "<http://example.org/default#Foo>"
);

assert(
  "unprefixUri: unknown prefix passes through",
  unprefixUri("unknown:Thing", prefixes),
  "unknown:Thing"
);

assert(
  "unprefixUri: unknown prefix with noerror returns null",
  unprefixUri("unknown:Thing", prefixes, true),
  null
);

assert(
  "unprefixUri: no prefix map returns unchanged",
  unprefixUri("obo:BFO_0000001", null),
  "obo:BFO_0000001"
);

assert(
  "unprefixUri: non-CURIE string returns unchanged",
  unprefixUri("just a plain string", prefixes),
  "just a plain string"
);

assert(
  "unprefixUri: CURIE with dots and slashes",
  unprefixUri("obo:bfo/2020/core.owl", prefixes),
  "<http://purl.obolibrary.org/obo/bfo/2020/core.owl>"
);

// ── annotationStringOrUri tests ──

// Numbers
assert(
  "number: integer",
  annotationStringOrUri("42", prefixes),
  "  42"
);

assert(
  "number: decimal",
  annotationStringOrUri("3.14", prefixes),
  "  3.14"
);

// Bare URIs
assert(
  "bare URI (no brackets)",
  annotationStringOrUri("http://example.org/foo", prefixes),
  "  <http://example.org/foo>"
);

assert(
  "bare URI in Org double brackets",
  annotationStringOrUri("[[http://example.org/foo]]", prefixes),
  "  <http://example.org/foo>"
);

assert(
  "bare URI already in angles",
  annotationStringOrUri("<http://example.org/foo>", prefixes),
  "  <http://example.org/foo>"
);

// URN
assert(
  "URN in angles",
  annotationStringOrUri("<urn:isbn:0943396611>", prefixes),
  "  <urn:isbn:0943396611>"
);

assert(
  "urn:uuid without angles → xsd:string",
  annotationStringOrUri("urn:uuid:550e8400-e29b-41d4-a716-446655440000", prefixes),
  '  "urn:uuid:550e8400-e29b-41d4-a716-446655440000"^^xsd:string'
);

// Booleans
assert(
  "boolean true",
  annotationStringOrUri("true", prefixes),
  ' "true"^^xsd:boolean'
);

assert(
  "boolean false",
  annotationStringOrUri("false", prefixes),
  ' "false"^^xsd:boolean'
);

// Typed strings
assert(
  "typed string passthrough",
  annotationStringOrUri('"hello"^^xsd:string', prefixes),
  '  "hello"^^xsd:string'
);

assert(
  "typed string with CURIE type",
  annotationStringOrUri('"42"^^xsd:integer', prefixes),
  '  "42"^^xsd:integer'
);

// Language-tagged strings
assert(
  "language-tagged string",
  annotationStringOrUri('"An entity is anything"@en', prefixes),
  ' "An entity is anything"@en'
);

assert(
  "language-tagged string with quotes inside",
  annotationStringOrUri('"(Elucidation) An entity is anything that exists"@en', prefixes),
  ' "(Elucidation) An entity is anything that exists"@en'
);

// CURIEs that expand
assert(
  "CURIE expands to URI",
  annotationStringOrUri("obo:BFO_0000001", prefixes),
  "  <http://purl.obolibrary.org/obo/BFO_0000001>"
);

assert(
  "default prefix CURIE expands",
  annotationStringOrUri(":Foo", prefixes),
  "  <http://example.org/default#Foo>"
);

// Plain strings
assert(
  "plain string gets wrapped in quotes",
  annotationStringOrUri("Alan Ruttenberg", prefixes),
  '  "Alan Ruttenberg"'
);

assert(
  "plain string with internal quotes gets escaped",
  annotationStringOrUri('Yongqun "Oliver" He', prefixes),
  '  "Yongqun \\"Oliver\\" He"'
);

assert(
  "BFO identifier string",
  annotationStringOrUri("001-BFO", prefixes),
  '  "001-BFO"'
);

// Edge cases
assert(
  "empty string becomes empty quoted string",
  annotationStringOrUri("", prefixes),
  '  ""'
);

assert(
  "string that looks like CURIE but prefix unknown → plain string",
  annotationStringOrUri("unknown:Thing", prefixes),
  '  "unknown:Thing"'
);

assert(
  "multi-line language-tagged string",
  annotationStringOrUri('"Line1\nLine2"@en', prefixes),
  ' "Line1\nLine2"@en'
);

// ── Summary ──

console.log(`\n${passed} passed, ${failed} failed out of ${total.length}`);
if (failed > 0) process.exit(1);
