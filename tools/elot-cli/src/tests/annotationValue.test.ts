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

// ── stripContinuationIndent tests ──

import { stripContinuationIndent } from "../annotationValue.js";

assert(
  "stripContinuationIndent: no newline returns unchanged",
  stripContinuationIndent("hello world"),
  "hello world"
);

assert(
  "stripContinuationIndent: strips common indent from continuation lines",
  stripContinuationIndent("first line\n          second line\n          third line"),
  "first line\nsecond line\nthird line"
);

assert(
  "stripContinuationIndent: preserves relative indent differences",
  stripContinuationIndent("A\n    B\n        C\n    D"),
  "A\nB\n    C\nD"
);

assert(
  "stripContinuationIndent: no indent → unchanged",
  stripContinuationIndent("A\nB\nC"),
  "A\nB\nC"
);

// Test that annotationStringOrUri strips continuation indent before quoting
assert(
  "annotationStringOrUri: multi-line with continuation indent stripped",
  annotationStringOrUri(
    "A 3D model represents some kind of 3D content. For the\n                     case of a single file, use zip.",
    prefixes
  ),
  '  "A 3D model represents some kind of 3D content. For the\ncase of a single file, use zip."'
);

// Test that double newlines (blank lines between paragraphs) are preserved
assert(
  "annotationStringOrUri: double newline preserved",
  annotationStringOrUri(
    "A reservation for boat travel.\n\nNote: This type is for information about actual reservations.",
    prefixes
  ),
  '  "A reservation for boat travel.\n\nNote: This type is for information about actual reservations."'
);

// Test: the Elisp `elot--strip-continuation-indent` only counts lines that
// HAVE leading spaces when computing minIndent.  A blank line (empty string)
// has no leading spaces so it does NOT contribute a 0 to the minimum.
// This means a blank-line-separated paragraph where the continuation
// lines before the blank line have indent 4 → minIndent = 4, and those
// 4 spaces are stripped.  The blank line and the unindented paragraph
// after it are left untouched (no prefix match).
// With per-paragraph stripping, each blank-line-separated paragraph
// has its own continuation indent stripped independently.
assert(
  "stripContinuationIndent: blank line does not force minIndent to 0",
  stripContinuationIndent(
    "A FundingAgency manages\n    the granting process.\n    A funding agency is not always required.\n\nExamples of funding agencies include ERC."
  ),
  "A FundingAgency manages\nthe granting process.\nA funding agency is not always required.\n\nExamples of funding agencies include ERC."
);

assert(
  "annotationStringOrUri: FundingAgency-style multi-paragraph with continuation indent",
  annotationStringOrUri(
    "A FundingAgency manages\n    the granting process.\n    A funding agency is not always required.\n\nExamples of funding agencies include ERC.",
    prefixes
  ),
  '  "A FundingAgency manages\nthe granting process.\nA funding agency is not always required.\n\nExamples of funding agencies include ERC."'
);

// Test: FundingAgency full case — paragraph 1 has 14-space indent,
// paragraph 2 has 10-space indent.  Each paragraph is treated independently.
assert(
  "stripContinuationIndent: FundingAgency full Org-format value",
  stripContinuationIndent(
    "A FundingAgency is an organization that implements one or more [[FundingScheme]]s and manages\n              the granting process (via [[Grant]]s, typically [[MonetaryGrant]]s).\n              A funding agency is not always required for grant funding, e.g. philanthropic giving, corporate sponsorship etc.\n\n          Examples of funding agencies include ERC, REA, NIH, Bill and Melinda Gates Foundation, ..."
  ),
  "A FundingAgency is an organization that implements one or more [[FundingScheme]]s and manages\nthe granting process (via [[Grant]]s, typically [[MonetaryGrant]]s).\nA funding agency is not always required for grant funding, e.g. philanthropic giving, corporate sponsorship etc.\n\nExamples of funding agencies include ERC, REA, NIH, Bill and Melinda Gates Foundation, ..."
);

// Test: IPTCDigitalSourceEnumeration case — continuation lines have
// 10 and 11 spaces.  Single-pass strips minIndent=10, leaving line 3
// with 1 leading space (" for <a...") which is meaningful content.
assert(
  "stripContinuationIndent: preserves relative indent (IPTC case)",
  stripContinuationIndent(
    "IPTC codes for use with digitalSourceType.\n          In general these codes are not declared.\n           for detailed definitions of all terms."
  ),
  "IPTC codes for use with digitalSourceType.\nIn general these codes are not declared.\n for detailed definitions of all terms."
);

// ── Summary ──

console.log(`\n${passed} passed, ${failed} failed out of ${total.length}`);
if (failed > 0) process.exit(1);
