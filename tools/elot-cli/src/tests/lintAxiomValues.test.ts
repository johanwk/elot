// src/tests/lintAxiomValues.test.ts
//
// Tests for Stage 6: axiom value CURIE checker (checker #7) and
// OMN keyword appropriateness checker (checker #8).

import type { ElotNode, DescriptionItem } from "../types.js";
import type { SlurpEntry } from "../buildSlurp.js";
import {
  checkAxiomValueCuries,
  checkOmnKeywordAppropriateness,
  isParenthesesBalanced,
  extractCurieTokens,
  BUILTIN_RESOURCES,
} from "../lintAxiomValues.js";

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

// ─── Tree builders ──────────────────────────────────────────────

function makeRoot(children: ElotNode[]): ElotNode {
  return { level: 0, title: "ROOT", children };
}

function makeOntologyTree(
  entityNodes: ElotNode[],
  sectionId: string = "pizza-class-hierarchy",
): ElotNode {
  const prefixDefs: ElotNode = {
    level: 2,
    title: "Prefixes",
    prefixdefs: true,
    prefixes: [
      { prefix: "pizza", uri: "http://example.org/pizza#" },
      { prefix: "owl", uri: "http://www.w3.org/2002/07/owl#" },
      { prefix: "rdfs", uri: "http://www.w3.org/2000/01/rdf-schema#" },
      { prefix: "obo", uri: "http://purl.obolibrary.org/obo/" },
      { prefix: "skos", uri: "http://www.w3.org/2004/02/skos/core#" },
    ],
  };

  const section: ElotNode = {
    level: 2,
    title: "Section",
    id: sectionId,
    resourcedefs: true,
    children: entityNodes,
  };

  const ontologyHeading: ElotNode = {
    level: 1,
    title: "Pizza Ontology",
    elotContextType: "ontology",
    elotContextLocalname: "pizza",
    id: "pizza",
    children: [prefixDefs, section],
  };

  return makeRoot([ontologyHeading]);
}

function makeEntity(
  curie: string,
  descriptions: DescriptionItem[],
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

// ─── Utility tests ──────────────────────────────────────────────

function testUtilities() {
  let passed = 0;

  // ── isParenthesesBalanced ─────────────────────────────────────
  {
    assert(isParenthesesBalanced("(a and b)"), "simple balanced");
    assert(isParenthesesBalanced("a and b"), "no parens");
    assert(isParenthesesBalanced("((a) and (b))"), "nested balanced");
    assert(!isParenthesesBalanced("(a and b"), "missing close");
    assert(!isParenthesesBalanced("a and b)"), "missing open");
    assert(!isParenthesesBalanced(")("), "reversed");
    assert(isParenthesesBalanced(""), "empty string");
    console.log("  isParenthesesBalanced: OK");
    passed++;
  }

  // ── extractCurieTokens ────────────────────────────────────────
  {
    const tokens1 = extractCurieTokens("pizza:Food and pizza:Topping");
    assert(tokens1.length === 2, "extract 2 curies");
    assert(tokens1[0] === "pizza:Food", "first curie");
    assert(tokens1[1] === "pizza:Topping", "second curie");

    const tokens2 = extractCurieTokens("pizza:A, pizza:B, pizza:C");
    assert(tokens2.length === 3, "comma-separated");

    const tokens3 = extractCurieTokens("http://example.org/foo");
    assert(tokens3.length === 0, "http URL excluded");

    const tokens4 = extractCurieTokens("some pizza:hasTopping");
    assert(tokens4.length === 1, "bare word filtered");
    assert(tokens4[0] === "pizza:hasTopping", "correct curie");

    const tokens5 = extractCurieTokens("not (pizza:Food or pizza:Drink)");
    // "not" and "or" have no colon, "(pizza:Food" has paren attached
    // Actually: split on whitespace: ["not", "(pizza:Food", "or", "pizza:Drink)"]
    // "(pizza:Food" doesn't match CURIE_REGEX because of '('
    // "pizza:Drink)" doesn't match because of ')'
    assert(tokens5.length === 0, "parens in tokens");

    const tokens6 = extractCurieTokens("owl:Thing");
    assert(tokens6.length === 1, "builtin");
    assert(tokens6[0] === "owl:Thing", "builtin value");

    console.log("  extractCurieTokens: OK");
    passed++;
  }

  // ── BUILTIN_RESOURCES ─────────────────────────────────────────
  {
    assert(BUILTIN_RESOURCES.has("owl:Thing"), "owl:Thing is builtin");
    assert(BUILTIN_RESOURCES.has("owl:Nothing"), "owl:Nothing is builtin");
    assert(BUILTIN_RESOURCES.has("xsd:string"), "xsd:string is builtin");
    assert(BUILTIN_RESOURCES.has("rdf:PlainLiteral"), "rdf:PlainLiteral is builtin");
    assert(!BUILTIN_RESOURCES.has("pizza:Food"), "pizza:Food not builtin");
    assert(BUILTIN_RESOURCES.size === 26, `builtin size is ${BUILTIN_RESOURCES.size}, expected 26`);
    console.log("  BUILTIN_RESOURCES: OK");
    passed++;
  }

  return passed;
}

// ─── Checker #7 tests ───────────────────────────────────────────

function testCheckAxiomValueCuries() {
  let passed = 0;

  // ── Valid CURIEs — all known in slurp ─────────────────────────
  {
    const root = makeOntologyTree([
      makeEntity("pizza:Apple", [
        { tag: "SubClassOf", value: "pizza:Food" },
      ]),
    ]);
    const slurp = new Map<string, SlurpEntry>([
      ["pizza:Food", { uri: "pizza:Food", label: "Food", rdfType: "owl:Class" }],
    ]);
    const diags = checkAxiomValueCuries(root, slurp);
    assertCount(diags, 0, "valid CURIEs");
    console.log("  valid CURIEs in axiom: OK");
    passed++;
  }

  // ── Built-in CURIEs (owl:Thing, xsd:string) — always OK ──────
  {
    const root = makeOntologyTree([
      makeEntity("pizza:Apple", [
        { tag: "SubClassOf", value: "owl:Thing" },
        { tag: "EquivalentTo", value: "xsd:string" },
      ]),
    ]);
    const slurp = new Map<string, SlurpEntry>();
    const diags = checkAxiomValueCuries(root, slurp);
    assertCount(diags, 0, "builtin CURIEs");
    console.log("  built-in CURIEs (owl:Thing, xsd:string): OK");
    passed++;
  }

  // ── Unknown CURIE — not in slurp or builtins ──────────────────
  {
    const root = makeOntologyTree([
      makeEntity("pizza:Apple", [
        { tag: "SubClassOf", value: "pizza:UnknownThing" },
      ]),
    ]);
    const slurp = new Map<string, SlurpEntry>();
    const diags = checkAxiomValueCuries(root, slurp);
    assertCount(diags, 1, "unknown CURIE");
    assertHas(diags, "warning", "Unknown CURIE in axiom: pizza:UnknownThing", "unknown CURIE");
    console.log("  unknown CURIE in axiom: OK");
    passed++;
  }

  // ── Annotation property in axiom position (non-annotation section) ─
  {
    const root = makeOntologyTree([
      makeEntity("pizza:Apple", [
        { tag: "SubClassOf", value: "skos:definition" },
      ]),
    ]);
    const slurp = new Map<string, SlurpEntry>([
      [
        "skos:definition",
        { uri: "skos:definition", label: "definition", rdfType: "owl:AnnotationProperty" },
      ],
    ]);
    const diags = checkAxiomValueCuries(root, slurp);
    assertCount(diags, 1, "annotation property in axiom");
    assertHas(
      diags,
      "warning",
      "Annotation property used in axiom: skos:definition",
      "annotation in axiom",
    );
    console.log("  annotation property in axiom (non-annotation section): OK");
    passed++;
  }

  // ── Annotation property in annotation section — OK ────────────
  {
    const root = makeOntologyTree(
      [
        makeEntity("pizza:myAnnotProp", [
          { tag: "SubPropertyOf", value: "skos:definition" },
        ]),
      ],
      "pizza-annotation-property-hierarchy",
    );
    const slurp = new Map<string, SlurpEntry>([
      [
        "skos:definition",
        { uri: "skos:definition", label: "definition", rdfType: "owl:AnnotationProperty" },
      ],
    ]);
    const diags = checkAxiomValueCuries(root, slurp);
    assertCount(diags, 0, "annotation in annotation section");
    console.log("  annotation property in annotation section: OK");
    passed++;
  }

  // ── Unbalanced parentheses ────────────────────────────────────
  {
    const root = makeOntologyTree([
      makeEntity("pizza:Apple", [
        { tag: "SubClassOf", value: "(pizza:Food and pizza:Topping" },
      ]),
    ]);
    const slurp = new Map<string, SlurpEntry>([
      ["pizza:Food", { uri: "pizza:Food", label: "Food", rdfType: "owl:Class" }],
      ["pizza:Topping", { uri: "pizza:Topping", label: "Topping", rdfType: "owl:Class" }],
    ]);
    const diags = checkAxiomValueCuries(root, slurp);
    // The parens make "(pizza:Food" not match CURIE regex (has leading paren),
    // so the CURIE tokens extracted are: none! But there's still the unbalanced-paren check.
    // Actually "and" has no colon, "pizza:Topping" does not match because it has no trailing paren.
    // Let me reconsider: "(pizza:Food" => doesn't match, "and" => doesn't match,
    // "pizza:Topping" => matches CURIE regex (no trailing paren issue... wait, the value has no trailing paren)
    // Value is: "(pizza:Food and pizza:Topping"
    // Tokens: ["(pizza:Food", "and", "pizza:Topping"]
    // "(pizza:Food" => has '(' which is not in [-_./\w], so doesn't match CURIE_REGEX. Good.
    // "pizza:Topping" => matches CURIE_REGEX.
    // So: pizza:Topping is in slurp (owl:Class, not annotation) => no warning.
    // Plus 1 unbalanced-parens warning.
    assertCount(diags, 1, "unbalanced parens");
    assertHas(diags, "warning", "Unbalanced parentheses", "unbalanced parens");
    console.log("  unbalanced parentheses: OK");
    passed++;
  }

  // ── Non-OMN keyword tag — skipped ─────────────────────────────
  {
    const root = makeOntologyTree([
      makeEntity("pizza:Apple", [
        { tag: "rdfs:comment", value: "pizza:Unknown not checked here" },
      ]),
    ]);
    const slurp = new Map<string, SlurpEntry>();
    const diags = checkAxiomValueCuries(root, slurp);
    assertCount(diags, 0, "non-OMN tag skipped");
    console.log("  non-OMN keyword tag (rdfs:comment) skipped: OK");
    passed++;
  }

  // ── Misc keyword (DisjointClasses) — still checked ────────────
  {
    const root = makeOntologyTree([
      makeEntity("pizza:Apple", [
        { tag: "DisjointClasses", value: "pizza:Unknown1, pizza:Unknown2" },
      ]),
    ]);
    const slurp = new Map<string, SlurpEntry>();
    const diags = checkAxiomValueCuries(root, slurp);
    assertCount(diags, 2, "misc keyword CURIEs checked");
    assertHas(diags, "warning", "pizza:Unknown1", "misc keyword curie1");
    assertHas(diags, "warning", "pizza:Unknown2", "misc keyword curie2");
    console.log("  misc keyword (DisjointClasses) CURIEs checked: OK");
    passed++;
  }

  // ── Multiple CURIEs in one axiom value ────────────────────────
  {
    const root = makeOntologyTree([
      makeEntity("pizza:Apple", [
        { tag: "SubClassOf", value: "pizza:Food and pizza:Unknown" },
      ]),
    ]);
    const slurp = new Map<string, SlurpEntry>([
      ["pizza:Food", { uri: "pizza:Food", label: "Food", rdfType: "owl:Class" }],
    ]);
    const diags = checkAxiomValueCuries(root, slurp);
    // "pizza:Food" is known, "and" has no colon, "pizza:Unknown" is unknown
    assertCount(diags, 1, "multiple CURIEs");
    assertHas(diags, "warning", "Unknown CURIE in axiom: pizza:Unknown", "multiple CURIEs");
    console.log("  multiple CURIEs (1 known, 1 unknown): OK");
    passed++;
  }

  // ── No descriptions — no diagnostics ──────────────────────────
  {
    const root = makeOntologyTree([
      { level: 3, title: "Entity (pizza:X)", uri: "pizza:X", label: "X", rdfType: "owl:Class" },
    ]);
    const diags = checkAxiomValueCuries(root);
    assertCount(diags, 0, "no descriptions");
    console.log("  no descriptions: OK");
    passed++;
  }

  // ── Outside ontology context — not visited ────────────────────
  {
    const diagrams: ElotNode = {
      level: 1,
      title: "Diagrams",
      children: [
        {
          level: 2,
          title: "Some section",
          resourcedefs: true,
          children: [
            makeEntity("pizza:X", [
              { tag: "SubClassOf", value: "pizza:Bogus" },
            ]),
          ],
        },
      ],
    };
    const root = makeRoot([diagrams]);
    const slurp = new Map<string, SlurpEntry>();
    const diags = checkAxiomValueCuries(root, slurp);
    assertCount(diags, 0, "outside ontology");
    console.log("  outside ontology context: OK");
    passed++;
  }

  // ── http:// URLs in axiom values — excluded ───────────────────
  {
    const root = makeOntologyTree([
      makeEntity("pizza:Apple", [
        { tag: "SubClassOf", value: "http://example.org/Food" },
      ]),
    ]);
    const slurp = new Map<string, SlurpEntry>();
    const diags = checkAxiomValueCuries(root, slurp);
    assertCount(diags, 0, "http URL excluded");
    console.log("  http:// URLs excluded from CURIE check: OK");
    passed++;
  }

  return passed;
}

// ─── Checker #8 tests ───────────────────────────────────────────

function testCheckOmnKeywordAppropriateness() {
  let passed = 0;

  // ── SubClassOf under classes — OK ─────────────────────────────
  {
    const root = makeOntologyTree(
      [
        makeEntity("pizza:Apple", [
          { tag: "SubClassOf", value: "pizza:Food" },
        ]),
      ],
      "pizza-class-hierarchy",
    );
    const diags = checkOmnKeywordAppropriateness(root);
    assertCount(diags, 0, "SubClassOf under classes");
    console.log("  SubClassOf under classes: OK");
    passed++;
  }

  // ── Domain under classes — ERROR ──────────────────────────────
  {
    const root = makeOntologyTree(
      [
        makeEntity("pizza:Apple", [
          { tag: "Domain", value: "pizza:Food" },
        ]),
      ],
      "pizza-class-hierarchy",
    );
    const diags = checkOmnKeywordAppropriateness(root);
    assertCount(diags, 1, "Domain under classes");
    assertHas(diags, "error", '"Domain" is not valid in Classes section', "Domain under classes");
    console.log("  Domain under classes (error): OK");
    passed++;
  }

  // ── DisjointClasses — misc keyword, valid anywhere ────────────
  {
    const root = makeOntologyTree(
      [
        makeEntity("pizza:Apple", [
          { tag: "DisjointClasses", value: "pizza:A, pizza:B" },
        ]),
      ],
      "pizza-class-hierarchy",
    );
    const diags = checkOmnKeywordAppropriateness(root);
    assertCount(diags, 0, "DisjointClasses misc");
    console.log("  DisjointClasses (misc keyword, valid anywhere): OK");
    passed++;
  }

  // ── Types under individuals — OK ──────────────────────────────
  {
    const root = makeOntologyTree(
      [
        makeEntity("pizza:myIndiv", [
          { tag: "Types", value: "pizza:Food" },
        ]),
      ],
      "pizza-individuals",
    );
    const diags = checkOmnKeywordAppropriateness(root);
    assertCount(diags, 0, "Types under individuals");
    console.log("  Types under individuals: OK");
    passed++;
  }

  // ── SubClassOf under individuals — ERROR ──────────────────────
  {
    const root = makeOntologyTree(
      [
        makeEntity("pizza:myIndiv", [
          { tag: "SubClassOf", value: "pizza:Food" },
        ]),
      ],
      "pizza-individuals",
    );
    const diags = checkOmnKeywordAppropriateness(root);
    assertCount(diags, 1, "SubClassOf under individuals");
    assertHas(
      diags,
      "error",
      '"SubClassOf" is not valid in Individuals section',
      "SubClassOf under individuals",
    );
    console.log("  SubClassOf under individuals (error): OK");
    passed++;
  }

  // ── Characteristics under object properties — OK ──────────────
  {
    const root = makeOntologyTree(
      [
        makeEntity("pizza:hasTopping", [
          { tag: "Characteristics", value: "Functional" },
        ]),
      ],
      "pizza-object-property-hierarchy",
    );
    const diags = checkOmnKeywordAppropriateness(root);
    assertCount(diags, 0, "Characteristics under object properties");
    console.log("  Characteristics under object properties: OK");
    passed++;
  }

  // ── Characteristics under annotation properties — ERROR ───────
  {
    const root = makeOntologyTree(
      [
        makeEntity("pizza:myAnnot", [
          { tag: "Characteristics", value: "Functional" },
        ]),
      ],
      "pizza-annotation-property-hierarchy",
    );
    const diags = checkOmnKeywordAppropriateness(root);
    assertCount(diags, 1, "Characteristics under annotation properties");
    assertHas(
      diags,
      "error",
      '"Characteristics" is not valid in Annotation properties section',
      "Characteristics under annotation",
    );
    console.log("  Characteristics under annotation properties (error): OK");
    passed++;
  }

  // ── EquivalentTo under datatypes — OK ─────────────────────────
  {
    const root = makeOntologyTree(
      [
        makeEntity("pizza:MyDatatype", [
          { tag: "EquivalentTo", value: "xsd:string" },
        ]),
      ],
      "pizza-datatypes",
    );
    const diags = checkOmnKeywordAppropriateness(root);
    assertCount(diags, 0, "EquivalentTo under datatypes");
    console.log("  EquivalentTo under datatypes: OK");
    passed++;
  }

  // ── SubClassOf under datatypes — ERROR ────────────────────────
  {
    const root = makeOntologyTree(
      [
        makeEntity("pizza:MyDatatype", [
          { tag: "SubClassOf", value: "xsd:string" },
        ]),
      ],
      "pizza-datatypes",
    );
    const diags = checkOmnKeywordAppropriateness(root);
    assertCount(diags, 1, "SubClassOf under datatypes");
    assertHas(
      diags,
      "error",
      '"SubClassOf" is not valid in Datatypes section',
      "SubClassOf under datatypes",
    );
    console.log("  SubClassOf under datatypes (error): OK");
    passed++;
  }

  // ── Multiple keywords, mixed valid/invalid ────────────────────
  {
    const root = makeOntologyTree(
      [
        makeEntity("pizza:Apple", [
          { tag: "SubClassOf", value: "pizza:Food" },       // OK
          { tag: "Domain", value: "pizza:Food" },            // ERROR
          { tag: "EquivalentTo", value: "pizza:Food" },     // OK
          { tag: "Characteristics", value: "Functional" },  // ERROR
          { tag: "DisjointClasses", value: "pizza:A, pizza:B" }, // misc, OK
        ]),
      ],
      "pizza-class-hierarchy",
    );
    const diags = checkOmnKeywordAppropriateness(root);
    assertCount(diags, 2, "mixed keywords");
    assertHas(diags, "error", '"Domain" is not valid in Classes', "mixed Domain");
    assertHas(diags, "error", '"Characteristics" is not valid in Classes', "mixed Characteristics");
    console.log("  multiple keywords (2 invalid, 3 valid): OK");
    passed++;
  }

  // ── Unknown section (no suffix match) — no errors ─────────────
  {
    const section: ElotNode = {
      level: 2,
      title: "Random section",
      id: "pizza-random-stuff",
      resourcedefs: true,
      children: [
        makeEntity("pizza:X", [
          { tag: "SubClassOf", value: "pizza:Y" },
        ]),
      ],
    };
    const ontology: ElotNode = {
      level: 1,
      title: "Pizza",
      elotContextType: "ontology",
      id: "pizza",
      elotContextLocalname: "pizza",
      children: [section],
    };
    const root = makeRoot([ontology]);
    const diags = checkOmnKeywordAppropriateness(root);
    assertCount(diags, 0, "unknown section suffix");
    console.log("  unknown section suffix (no validation): OK");
    passed++;
  }

  // ── Non-OMN keyword — not checked ─────────────────────────────
  {
    const root = makeOntologyTree(
      [
        makeEntity("pizza:Apple", [
          { tag: "rdfs:comment", value: "not checked" },
        ]),
      ],
      "pizza-class-hierarchy",
    );
    const diags = checkOmnKeywordAppropriateness(root);
    assertCount(diags, 0, "non-OMN keyword");
    console.log("  non-OMN keyword (rdfs:comment) not checked: OK");
    passed++;
  }

  // ── No ontology heading — no diagnostics ──────────────────────
  {
    const root: ElotNode = {
      level: 0,
      title: "ROOT",
      children: [{ level: 1, title: "Random" }],
    };
    const diags = checkOmnKeywordAppropriateness(root);
    assertCount(diags, 0, "no ontology");
    console.log("  no ontology heading: OK");
    passed++;
  }

  // ── Deeper nesting — section suffix found from ancestor ───────
  {
    const deepEntity: ElotNode = {
      level: 4,
      title: "Deep Entity (pizza:Deep)",
      uri: "pizza:Deep",
      label: "Deep",
      rdfType: "owl:Class",
      descriptions: [
        { tag: "Domain", value: "pizza:Food" }, // ERROR: Domain not valid in classes
      ],
    };
    const wrapper: ElotNode = {
      level: 3,
      title: "Wrapper",
      children: [deepEntity],
    };
    const root = makeOntologyTree([wrapper], "pizza-class-hierarchy");
    const diags = checkOmnKeywordAppropriateness(root);
    assertCount(diags, 1, "deep nesting");
    assertHas(diags, "error", '"Domain" is not valid in Classes', "deep nesting Domain");
    console.log("  deeper nesting (section suffix from ancestor): OK");
    passed++;
  }

  // ── -ontology-declaration — no keyword restrictions ────────────
  {
    const root = makeOntologyTree(
      [
        makeEntity("pizza:X", [
          { tag: "Import", value: "http://example.org/other" },
        ]),
      ],
      "pizza-ontology-declaration",
    );
    const diags = checkOmnKeywordAppropriateness(root);
    assertCount(diags, 0, "ontology-declaration");
    console.log("  -ontology-declaration (no keyword restrictions): OK");
    passed++;
  }

  return passed;
}

// ─── Main ───────────────────────────────────────────────────────

function main() {
  let total = 0;

  console.log("\n=== Utility tests ===");
  total += testUtilities();

  console.log("\n=== checkAxiomValueCuries (checker #7) ===");
  total += testCheckAxiomValueCuries();

  console.log("\n=== checkOmnKeywordAppropriateness (checker #8) ===");
  total += testCheckOmnKeywordAppropriateness();

  console.log(`\n${total} passed, 0 failed`);
}

main();
