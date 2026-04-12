// src/tests/lintDescriptionCuries.test.ts
//
// Tests for Stage 5: description list CURIE checker (checker #6).

import type { ElotNode, DescriptionItem } from "../types.js";
import type { SlurpEntry } from "../buildSlurp.js";
import { checkDescriptionListCuries } from "../lintDescriptionCuries.js";

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
        ? ` — ${diagnostics.map((d) => `[${d.severity}] ${d.message}`).join("; ")}`
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
    `${label}: expected a ${severity} containing "${substring}" — got: ${JSON.stringify(diagnostics.map((d) => `[${d.severity}] ${d.message}`))}`,
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
      { prefix: "skos", uri: "http://www.w3.org/2004/02/skos/core#" },
      { prefix: "obo", uri: "http://purl.obolibrary.org/obo/" },
    ],
  };

  const classHierarchy: ElotNode = {
    level: 2,
    title: "Class hierarchy",
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
    children: [prefixDefs, classHierarchy],
  };

  return makeRoot([ontologyHeading]);
}

function makeEntity(
  curie: string,
  descriptions: DescriptionItem[],
): ElotNode {
  return {
    level: 3,
    title: `Entity (${curie})`,
    uri: curie,
    label: "Entity",
    rdfType: "owl:Class",
    descriptions,
  };
}

// ─── Tests ───────────────────────────────────────────────────────

function main() {
  let passed = 0;

  // ── rdfs:comment — known annotation property, should pass ─────
  {
    const root = makeOntologyTree([
      makeEntity("pizza:Apple", [
        { tag: "rdfs:comment", value: "A tasty fruit topping" },
      ]),
    ]);
    const diags = checkDescriptionListCuries(root);
    assertCount(diags, 0, "rdfs:comment");
    console.log("  rdfs:comment (known annotation property): OK");
    passed++;
  }

  // ── rdfs:label — known annotation property, should pass ───────
  {
    const root = makeOntologyTree([
      makeEntity("pizza:Apple", [
        { tag: "rdfs:label", value: '"Apple"@en' },
      ]),
    ]);
    const diags = checkDescriptionListCuries(root);
    assertCount(diags, 0, "rdfs:label");
    console.log("  rdfs:label (known annotation property): OK");
    passed++;
  }

  // ── rdfs:seeAlso + rdfs:isDefinedBy — known, should pass ─────
  {
    const root = makeOntologyTree([
      makeEntity("pizza:Apple", [
        { tag: "rdfs:seeAlso", value: "http://example.org" },
        { tag: "rdfs:isDefinedBy", value: "http://example.org/onto" },
      ]),
    ]);
    const diags = checkDescriptionListCuries(root);
    assertCount(diags, 0, "rdfs:seeAlso + isDefinedBy");
    console.log("  rdfs:seeAlso + rdfs:isDefinedBy: OK");
    passed++;
  }

  // ── skos:definition declared as AnnotationProperty in slurp ───
  {
    const root = makeOntologyTree([
      makeEntity("pizza:Apple", [
        { tag: "skos:definition", value: "A definition" },
      ]),
    ]);
    const slurp = new Map<string, SlurpEntry>([
      [
        "skos:definition",
        {
          uri: "skos:definition",
          label: "definition",
          rdfType: "owl:AnnotationProperty",
        },
      ],
    ]);
    const diags = checkDescriptionListCuries(root, slurp);
    assertCount(diags, 0, "skos:definition in slurp");
    console.log("  skos:definition (declared AnnotationProperty in slurp): OK");
    passed++;
  }

  // ── foo:unknown not in slurp → WARNING ────────────────────────
  {
    const root = makeOntologyTree([
      makeEntity("pizza:Apple", [
        { tag: "foo:unknown", value: "something" },
      ]),
    ]);
    const slurp = new Map<string, SlurpEntry>();
    const diags = checkDescriptionListCuries(root, slurp);
    assertCount(diags, 1, "foo:unknown");
    assertHas(
      diags,
      "warning",
      "Unknown or invalid annotation property: foo:unknown",
      "foo:unknown",
    );
    console.log("  foo:unknown (not in slurp): OK");
    passed++;
  }

  // ── CURIE in slurp but NOT an AnnotationProperty → WARNING ────
  {
    const root = makeOntologyTree([
      makeEntity("pizza:Apple", [
        { tag: "obo:BFO_0000050", value: "something" },
      ]),
    ]);
    const slurp = new Map<string, SlurpEntry>([
      [
        "obo:BFO_0000050",
        {
          uri: "obo:BFO_0000050",
          label: "part of",
          rdfType: "owl:ObjectProperty",
        },
      ],
    ]);
    const diags = checkDescriptionListCuries(root, slurp);
    assertCount(diags, 1, "ObjectProperty as annotation tag");
    assertHas(
      diags,
      "warning",
      "Unknown or invalid annotation property: obo:BFO_0000050",
      "ObjectProperty as annotation",
    );
    console.log("  CURIE in slurp but ObjectProperty (not AnnotationProperty): OK");
    passed++;
  }

  // ── SubClassOf — OMN keyword, should be skipped ───────────────
  {
    const root = makeOntologyTree([
      makeEntity("pizza:Apple", [
        { tag: "SubClassOf", value: "pizza:Food" },
      ]),
    ]);
    const diags = checkDescriptionListCuries(root);
    assertCount(diags, 0, "SubClassOf skipped");
    console.log("  SubClassOf (OMN keyword, skipped): OK");
    passed++;
  }

  // ── DisjointClasses — misc OMN keyword, should be skipped ─────
  {
    const root = makeOntologyTree([
      makeEntity("pizza:Apple", [
        { tag: "DisjointClasses", value: "pizza:Apple, pizza:Banana" },
      ]),
    ]);
    const diags = checkDescriptionListCuries(root);
    assertCount(diags, 0, "DisjointClasses skipped");
    console.log("  DisjointClasses (misc OMN keyword, skipped): OK");
    passed++;
  }

  // ── Non-CURIE tags (plain text) should be skipped ─────────────
  {
    const root = makeOntologyTree([
      makeEntity("pizza:Apple", [
        { tag: "rdf:type", value: "owl:Class" },
        { tag: "Some plain text", value: "not a CURIE tag" },
        { tag: "123", value: "number" },
      ]),
    ]);
    const slurp = new Map<string, SlurpEntry>();
    const diags = checkDescriptionListCuries(root, slurp);
    // "rdf:type" matches CURIE regex and is NOT a known annotation property
    // or OMN keyword, so it would warn unless in slurp. But "Some plain text"
    // and "123" don't match CURIE regex, so they're skipped.
    assertCount(diags, 1, "non-CURIE tags");
    assertHas(
      diags,
      "warning",
      "rdf:type",
      "non-CURIE tags rdf:type",
    );
    console.log("  non-CURIE tags (plain text skipped, rdf:type flagged): OK");
    passed++;
  }

  // ── Multiple description items, mixed results ─────────────────
  {
    const root = makeOntologyTree([
      makeEntity("pizza:Apple", [
        { tag: "rdfs:comment", value: "OK" },
        { tag: "SubClassOf", value: "pizza:Food" },
        { tag: "skos:definition", value: "A definition" },
        { tag: "foo:bar", value: "unknown" },
      ]),
    ]);
    const slurp = new Map<string, SlurpEntry>([
      [
        "skos:definition",
        {
          uri: "skos:definition",
          label: "definition",
          rdfType: "owl:AnnotationProperty",
        },
      ],
    ]);
    const diags = checkDescriptionListCuries(root, slurp);
    // rdfs:comment → known, skip
    // SubClassOf → OMN keyword, skip
    // skos:definition → in slurp as AnnotationProperty, skip
    // foo:bar → unknown, warn
    assertCount(diags, 1, "mixed descriptions");
    assertHas(diags, "warning", "foo:bar", "mixed descriptions");
    console.log("  mixed descriptions (1 warning for foo:bar): OK");
    passed++;
  }

  // ── Heading outside ontology context → not visited ────────────
  {
    const diagrams: ElotNode = {
      level: 1,
      title: "Diagrams",
      children: [
        {
          level: 2,
          title: "A diagram entity",
          descriptions: [
            { tag: "foo:whatever", value: "should not be checked" },
          ],
        },
      ],
    };
    const root = makeRoot([diagrams]);
    const diags = checkDescriptionListCuries(root);
    assertCount(diags, 0, "outside ontology");
    console.log("  heading outside ontology context: OK");
    passed++;
  }

  // ── Multiple entities with warnings ───────────────────────────
  {
    const root = makeOntologyTree([
      makeEntity("pizza:Apple", [
        { tag: "foo:annot1", value: "bad" },
      ]),
      makeEntity("pizza:Banana", [
        { tag: "bar:annot2", value: "also bad" },
      ]),
      makeEntity("pizza:Cherry", [
        { tag: "rdfs:comment", value: "good" },
      ]),
    ]);
    const slurp = new Map<string, SlurpEntry>();
    const diags = checkDescriptionListCuries(root, slurp);
    assertCount(diags, 2, "multiple entities");
    assertHas(diags, "warning", "foo:annot1", "multiple entities annot1");
    assertHas(diags, "warning", "bar:annot2", "multiple entities annot2");
    console.log("  multiple entities with warnings: OK");
    passed++;
  }

  // ── No descriptions at all → no diagnostics ───────────────────
  {
    const root = makeOntologyTree([
      {
        level: 3,
        title: "Entity (pizza:X)",
        uri: "pizza:X",
        label: "X",
        rdfType: "owl:Class",
        // no descriptions
      },
    ]);
    const diags = checkDescriptionListCuries(root);
    assertCount(diags, 0, "no descriptions");
    console.log("  no descriptions: OK");
    passed++;
  }

  // ── No ontology → no diagnostics ──────────────────────────────
  {
    const root: ElotNode = {
      level: 0,
      title: "ROOT",
      children: [{ level: 1, title: "Random file" }],
    };
    const diags = checkDescriptionListCuries(root);
    assertCount(diags, 0, "no ontology");
    console.log("  no ontology heading: OK");
    passed++;
  }

  // ── CURIE with dots/hyphens/underscores in prefix or local ────
  {
    const root = makeOntologyTree([
      makeEntity("pizza:Apple", [
        { tag: "my-ns:some_prop.v2", value: "value" },
      ]),
    ]);
    const slurp = new Map<string, SlurpEntry>();
    const diags = checkDescriptionListCuries(root, slurp);
    // "my-ns:some_prop.v2" matches CURIE regex, not known or OMN → warn
    assertCount(diags, 1, "complex CURIE");
    assertHas(diags, "warning", "my-ns:some_prop.v2", "complex CURIE");
    console.log("  CURIE with dots/hyphens/underscores: OK");
    passed++;
  }

  // ── Empty prefix CURIE (:localname) ───────────────────────────
  {
    const root = makeOntologyTree([
      makeEntity("pizza:Apple", [
        { tag: ":myAnnot", value: "value" },
      ]),
    ]);
    const slurp = new Map<string, SlurpEntry>();
    const diags = checkDescriptionListCuries(root, slurp);
    // ":myAnnot" matches CURIE regex, not known or OMN → warn
    assertCount(diags, 1, "empty prefix CURIE");
    assertHas(diags, "warning", ":myAnnot", "empty prefix CURIE");
    console.log("  empty prefix CURIE (:myAnnot): OK");
    passed++;
  }

  console.log(`\n${passed} passed, 0 failed`);
}

main();
