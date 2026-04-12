// src/tests/lintHeadingIdentity.test.ts
//
// Tests for Stage 4: heading identity checker (checker #5).

import type { ElotNode } from "../types.js";
import { checkHeadingIdentity } from "../lintHeadingIdentity.js";

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

function makeOntologyTree(entityNodes: ElotNode[]): ElotNode {
  const prefixDefs: ElotNode = {
    level: 2,
    title: "Prefixes",
    prefixdefs: true,
    prefixes: [
      { prefix: "pizza", uri: "http://example.org/pizza#" },
      { prefix: "owl", uri: "http://www.w3.org/2002/07/owl#" },
      { prefix: "obo", uri: "http://purl.obolibrary.org/obo/" },
      { prefix: "rdfs", uri: "http://www.w3.org/2000/01/rdf-schema#" },
    ],
  };

  const classHierarchy: ElotNode = {
    level: 2,
    title: "Class hierarchy",
    id: "pizza-class-hierarchy",
    resourcedefs: true,
    children: entityNodes,
  };

  const ontologyHeading: ElotNode = {
    level: 1,
    title: "Pizza Ontology",
    elotContextType: "ontology",
    elotContextLocalname: "pizza",
    elotDefaultPrefix: "pizza",
    id: "pizza",
    tangleTargetOmn: "pizza.omn",
    children: [prefixDefs, classHierarchy],
  };

  return makeRoot([ontologyHeading]);
}

// ─── Tests ───────────────────────────────────────────────────────

function main() {
  let passed = 0;

  // ── Valid headings: all have CURIEs with known prefix and clean labels ──
  {
    const root = makeOntologyTree([
      {
        level: 3,
        title: '"Apple"@en (pizza:Apple)',
        uri: "pizza:Apple",
        label: '"Apple"@en',
        rdfType: "owl:Class",
      },
      {
        level: 3,
        title: "Banana (pizza:Banana)",
        uri: "pizza:Banana",
        label: "Banana",
        rdfType: "owl:Class",
      },
    ]);
    const diags = checkHeadingIdentity(root);
    assertCount(diags, 0, "valid headings");
    console.log("  valid headings: OK");
    passed++;
  }

  // ── Heading with no CURIE/URI → ERROR ─────────────────────────
  {
    const root = makeOntologyTree([
      {
        level: 3,
        title: "Some heading without identifier",
        // no uri
        label: "Some heading without identifier",
        rdfType: "owl:Class",
      },
    ]);
    const diags = checkHeadingIdentity(root);
    assertCount(diags, 1, "no CURIE");
    assertHas(diags, "error", "No identifier found", "no CURIE");
    console.log("  heading with no CURIE: OK");
    passed++;
  }

  // ── Heading with unknown prefix → ERROR ───────────────────────
  {
    const root = makeOntologyTree([
      {
        level: 3,
        title: "Widget (foo:Widget)",
        uri: "foo:Widget",
        label: "Widget",
        rdfType: "owl:Class",
      },
    ]);
    const diags = checkHeadingIdentity(root);
    assertCount(diags, 1, "unknown prefix");
    assertHas(diags, "error", "Unknown prefix", "unknown prefix");
    console.log("  heading with unknown prefix: OK");
    passed++;
  }

  // ── Heading tagged :nodeclare: → skipped ──────────────────────
  {
    const root = makeOntologyTree([
      {
        level: 3,
        title: "Skip me",
        tags: ["nodeclare"],
        // no uri — would normally error, but :nodeclare: overrides
        rdfType: "owl:Class",
      },
    ]);
    const diags = checkHeadingIdentity(root);
    assertCount(diags, 0, "nodeclare skipped");
    console.log("  heading tagged :nodeclare: skipped: OK");
    passed++;
  }

  // ── Heading with ancestor tagged :nodeclare: → skipped ────────
  {
    const wrapper: ElotNode = {
      level: 3,
      title: "Wrapper",
      tags: ["nodeclare"],
      rdfType: "owl:Class",
      uri: "pizza:Wrapper",
      label: "Wrapper",
      children: [
        {
          level: 4,
          title: "Child without URI",
          // no uri — would error, but ancestor is :nodeclare:
          rdfType: "owl:Class",
        },
      ],
    };
    const root = makeOntologyTree([wrapper]);
    const diags = checkHeadingIdentity(root);
    // Both the wrapper (which is nodeclare itself) and the child
    // (ancestor is nodeclare) should be skipped
    assertCount(diags, 0, "ancestor nodeclare");
    console.log("  ancestor tagged :nodeclare: skipped: OK");
    passed++;
  }

  // ── Section root node (resourcedefs=true) → skipped ───────────
  {
    // The class hierarchy node itself has resourcedefs=true and should
    // be skipped, even if it has no URI
    const root = makeOntologyTree([]);
    const diags = checkHeadingIdentity(root);
    assertCount(diags, 0, "section root skipped");
    console.log("  section root (resourcedefs) skipped: OK");
    passed++;
  }

  // ── Label with unbalanced quotes → WARNING ────────────────────
  {
    const root = makeOntologyTree([
      {
        level: 3,
        title: '"Bad label (pizza:Bad)',
        uri: "pizza:Bad",
        label: '"Bad label',
        rdfType: "owl:Class",
      },
    ]);
    const diags = checkHeadingIdentity(root);
    assertCount(diags, 1, "bad label");
    assertHas(diags, "warning", "Label should have no quotes", "bad label");
    console.log("  label with unbalanced quotes: OK");
    passed++;
  }

  // ── Label with quotes but valid "text"@lang → OK ──────────────
  {
    const root = makeOntologyTree([
      {
        level: 3,
        title: '"entity"@en (obo:BFO_0000001)',
        uri: "obo:BFO_0000001",
        label: '"entity"@en',
        rdfType: "owl:Class",
      },
    ]);
    const diags = checkHeadingIdentity(root);
    assertCount(diags, 0, "valid label with quotes");
    console.log('  label "text"@lang valid: OK');
    passed++;
  }

  // ── Full URI in angle brackets → no prefix check ──────────────
  {
    const root = makeOntologyTree([
      {
        level: 3,
        title: "Widget (<http://example.org/Widget>)",
        uri: "<http://example.org/Widget>",
        label: "Widget",
        rdfType: "owl:Class",
      },
    ]);
    const diags = checkHeadingIdentity(root);
    assertCount(diags, 0, "full URI");
    console.log("  full URI in angle brackets: OK");
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
          title: "Some diagram",
          // no uri, no resourcedefs — outside ontology scope
        },
      ],
    };
    const prefixDefs: ElotNode = {
      level: 2,
      title: "Prefixes",
      prefixdefs: true,
      prefixes: [{ prefix: "pizza", uri: "http://example.org/pizza#" }],
    };
    const classHierarchy: ElotNode = {
      level: 2,
      title: "Class hierarchy",
      id: "pizza-class-hierarchy",
      resourcedefs: true,
      children: [
        {
          level: 3,
          title: "Good (pizza:Good)",
          uri: "pizza:Good",
          label: "Good",
          rdfType: "owl:Class",
        },
      ],
    };
    const ontologyHeading: ElotNode = {
      level: 1,
      title: "Pizza Ontology",
      elotContextType: "ontology",
      elotContextLocalname: "pizza",
      id: "pizza",
      children: [prefixDefs, classHierarchy],
    };
    const root = makeRoot([ontologyHeading, diagrams]);
    const diags = checkHeadingIdentity(root);
    assertCount(diags, 0, "outside ontology");
    console.log("  heading outside ontology context: OK");
    passed++;
  }

  // ── Multiple issues in one tree ───────────────────────────────
  {
    const root = makeOntologyTree([
      {
        level: 3,
        title: "No identifier",
        rdfType: "owl:Class",
        // no uri
      },
      {
        level: 3,
        title: "Bad prefix (baz:Widget)",
        uri: "baz:Widget",
        label: "Bad prefix",
        rdfType: "owl:Class",
      },
      {
        level: 3,
        title: '"bad"quotes (pizza:BQ)',
        uri: "pizza:BQ",
        label: '"bad"quotes',
        rdfType: "owl:Class",
      },
      {
        level: 3,
        title: "Good (pizza:Good)",
        uri: "pizza:Good",
        label: "Good",
        rdfType: "owl:Class",
      },
    ]);
    const diags = checkHeadingIdentity(root);
    assertCount(diags, 3, "multiple issues");
    assertHas(diags, "error", "No identifier found", "multiple: no id");
    assertHas(diags, "error", "Unknown prefix", "multiple: bad prefix");
    assertHas(diags, "warning", "Label should have no quotes", "multiple: bad label");
    console.log("  multiple issues in one tree: OK");
    passed++;
  }

  // ── Empty prefix part (default namespace) CURIE like :LocalName ─
  {
    // The default namespace ":" has empty prefix part
    const prefixDefs: ElotNode = {
      level: 2,
      title: "Prefixes",
      prefixdefs: true,
      prefixes: [
        { prefix: ":", uri: "http://example.org/default#" },
        { prefix: "owl", uri: "http://www.w3.org/2002/07/owl#" },
      ],
    };
    const classHierarchy: ElotNode = {
      level: 2,
      title: "Class hierarchy",
      id: "pizza-class-hierarchy",
      resourcedefs: true,
      children: [
        {
          level: 3,
          title: "Widget (:Widget)",
          uri: ":Widget",
          label: "Widget",
          rdfType: "owl:Class",
        },
      ],
    };
    const ontologyHeading: ElotNode = {
      level: 1,
      title: "Test Ontology",
      elotContextType: "ontology",
      id: "test",
      children: [prefixDefs, classHierarchy],
    };
    const root = makeRoot([ontologyHeading]);
    const diags = checkHeadingIdentity(root);
    // The empty prefix "" should be in the map (buildPrefixMap strips the trailing colon)
    assertCount(diags, 0, "default namespace CURIE");
    console.log("  default namespace CURIE (:Widget): OK");
    passed++;
  }

  // ── No ontology → no diagnostics ──────────────────────────────
  {
    const root: ElotNode = {
      level: 0,
      title: "ROOT",
      children: [{ level: 1, title: "Random file" }],
    };
    const diags = checkHeadingIdentity(root);
    assertCount(diags, 0, "no ontology");
    console.log("  no ontology heading: OK");
    passed++;
  }

  console.log(`\n${passed} passed, 0 failed`);
}

main();
