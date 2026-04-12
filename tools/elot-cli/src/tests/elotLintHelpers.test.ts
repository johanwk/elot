// src/tests/elotLintHelpers.test.ts
//
// Tests for Stage 1: scoping helpers and tree-walking infrastructure.

import type { ElotNode } from "../types.js";
import {
  isInsideOntologyContext,
  isInsideResourcedefs,
  isInsideElotScope,
  getSectionSuffix,
  findOntologyRoots,
  isNodeclare,
  isCurieKnown,
  walkResourceNodes,
} from "../elotLintHelpers.js";

// ─── Helpers ─────────────────────────────────────────────────────

function assert(condition: boolean, msg: string) {
  if (!condition) {
    console.error(`FAIL: ${msg}`);
    process.exit(1);
  }
}

// ─── Shared test tree ────────────────────────────────────────────
//
// Structure:
//   ROOT (level 0)
//     ├─ Ontology heading (level 1, elotContextType: "ontology")
//     │   ├─ Prefix defs (level 2, prefixdefs: true)
//     │   ├─ Class hierarchy (level 2, id: "pizza-class-hierarchy", resourcedefs: true)
//     │   │   ├─ Entity A (level 3, uri: "pizza:A", label: '"A"@en')
//     │   │   └─ Entity B (level 3, uri: "pizza:B", tags: ["nodeclare"])
//     │   ├─ Object prop hierarchy (level 2, id: "pizza-object-property-hierarchy", resourcedefs: true)
//     │   │   └─ Entity C (level 3, uri: "pizza:hasTop")
//     │   └─ No-resourcedefs section (level 2)
//     │       └─ Entity D (level 3, uri: "pizza:D")
//     └─ Diagrams heading (level 1, no elotContextType)
//         └─ Some content (level 2)

const entityA: ElotNode = {
  level: 3,
  title: '"A"@en (pizza:A)',
  uri: "pizza:A",
  label: '"A"@en',
  rdfType: "owl:Class",
};

const entityB: ElotNode = {
  level: 3,
  title: "B (pizza:B)",
  uri: "pizza:B",
  tags: ["nodeclare"],
};

const classHierarchy: ElotNode = {
  level: 2,
  title: "Class hierarchy",
  id: "pizza-class-hierarchy",
  resourcedefs: true,
  rdfType: "owl:Class",
  children: [entityA, entityB],
};

const entityC: ElotNode = {
  level: 3,
  title: "hasTop (pizza:hasTop)",
  uri: "pizza:hasTop",
  rdfType: "owl:ObjectProperty",
};

const objPropHierarchy: ElotNode = {
  level: 2,
  title: "Object property hierarchy",
  id: "pizza-object-property-hierarchy",
  resourcedefs: true,
  rdfType: "owl:ObjectProperty",
  children: [entityC],
};

const entityD: ElotNode = {
  level: 3,
  title: '"D"@en (pizza:D)',
  uri: "pizza:D",
};

const noResourcedefsSection: ElotNode = {
  level: 2,
  title: "Some non-resourcedefs section",
  children: [entityD],
};

const prefixDefs: ElotNode = {
  level: 2,
  title: "Prefixes",
  prefixdefs: true,
  prefixes: [
    { prefix: "pizza", uri: "http://example.org/pizza#" },
    { prefix: "obo", uri: "http://purl.obolibrary.org/obo/" },
  ],
};

const ontologyHeading: ElotNode = {
  level: 1,
  title: "Pizza Ontology",
  elotContextType: "ontology",
  elotContextLocalname: "pizza",
  id: "pizza",
  children: [prefixDefs, classHierarchy, objPropHierarchy, noResourcedefsSection],
};

const diagramContent: ElotNode = {
  level: 2,
  title: "Diagram 1",
};

const diagramsHeading: ElotNode = {
  level: 1,
  title: "Diagrams",
  children: [diagramContent],
};

const root: ElotNode = {
  level: 0,
  title: "ROOT",
  children: [ontologyHeading, diagramsHeading],
};

// ─── Tests ───────────────────────────────────────────────────────

function main() {
  let passed = 0;

  // ── isInsideOntologyContext ────────────────────────────────────
  {
    // Entity A is under ontology heading → true
    assert(
      isInsideOntologyContext(entityA, [root, ontologyHeading, classHierarchy]),
      "entityA should be inside ontology context",
    );
    // Diagram content is NOT under ontology heading → false
    assert(
      !isInsideOntologyContext(diagramContent, [root, diagramsHeading]),
      "diagramContent should NOT be inside ontology context",
    );
    // The ontology heading itself: ancestors are [root], which has no elotContextType
    assert(
      !isInsideOntologyContext(ontologyHeading, [root]),
      "ontologyHeading itself: ancestors=[root] → false (the heading is not its own ancestor)",
    );
    console.log("  isInsideOntologyContext: OK");
    passed++;
  }

  // ── isInsideResourcedefs ──────────────────────────────────────
  {
    // Entity A is under classHierarchy (resourcedefs: true) → true
    assert(
      isInsideResourcedefs(entityA, [root, ontologyHeading, classHierarchy]),
      "entityA should be inside resourcedefs",
    );
    // classHierarchy itself has resourcedefs → true
    assert(
      isInsideResourcedefs(classHierarchy, [root, ontologyHeading]),
      "classHierarchy should be inside resourcedefs (its own property)",
    );
    // Entity D is under noResourcedefsSection → false
    assert(
      !isInsideResourcedefs(entityD, [root, ontologyHeading, noResourcedefsSection]),
      "entityD should NOT be inside resourcedefs",
    );
    // Diagram content → false
    assert(
      !isInsideResourcedefs(diagramContent, [root, diagramsHeading]),
      "diagramContent should NOT be inside resourcedefs",
    );
    console.log("  isInsideResourcedefs: OK");
    passed++;
  }

  // ── isInsideElotScope ─────────────────────────────────────────
  {
    // Entity A: inside ontology context AND resourcedefs → true
    assert(
      isInsideElotScope(entityA, [root, ontologyHeading, classHierarchy]),
      "entityA should be inside ELOT scope",
    );
    // Entity D: inside ontology context but NOT resourcedefs → false
    assert(
      !isInsideElotScope(entityD, [root, ontologyHeading, noResourcedefsSection]),
      "entityD should NOT be inside ELOT scope (no resourcedefs)",
    );
    // Diagram content: NOT inside ontology context → false
    assert(
      !isInsideElotScope(diagramContent, [root, diagramsHeading]),
      "diagramContent should NOT be inside ELOT scope",
    );
    console.log("  isInsideElotScope: OK");
    passed++;
  }

  // ── getSectionSuffix ──────────────────────────────────────────
  {
    // Entity A → nearest ancestor with known suffix is classHierarchy → "-class-hierarchy"
    assert(
      getSectionSuffix(entityA, [root, ontologyHeading, classHierarchy]) === "-class-hierarchy",
      "entityA section suffix should be -class-hierarchy",
    );
    // Entity C → "-object-property-hierarchy"
    assert(
      getSectionSuffix(entityC, [root, ontologyHeading, objPropHierarchy]) === "-object-property-hierarchy",
      "entityC section suffix should be -object-property-hierarchy",
    );
    // classHierarchy itself → its own id ends with -class-hierarchy
    assert(
      getSectionSuffix(classHierarchy, [root, ontologyHeading]) === "-class-hierarchy",
      "classHierarchy section suffix should be -class-hierarchy (own id)",
    );
    // Entity D → no known suffix in ancestors
    assert(
      getSectionSuffix(entityD, [root, ontologyHeading, noResourcedefsSection]) === null,
      "entityD section suffix should be null",
    );
    // Diagram content → null
    assert(
      getSectionSuffix(diagramContent, [root, diagramsHeading]) === null,
      "diagramContent section suffix should be null",
    );
    console.log("  getSectionSuffix: OK");
    passed++;
  }

  // ── findOntologyRoots ─────────────────────────────────────────
  {
    const roots = findOntologyRoots(root);
    assert(roots.length === 1, "should find exactly 1 ontology root");
    assert(roots[0] === ontologyHeading, "ontology root should be the ontology heading");
    console.log("  findOntologyRoots: OK");
    passed++;
  }

  // ── findOntologyRoots with no ontology ─────────────────────────
  {
    const emptyRoot: ElotNode = {
      level: 0,
      title: "ROOT",
      children: [diagramsHeading],
    };
    const roots = findOntologyRoots(emptyRoot);
    assert(roots.length === 0, "should find 0 ontology roots");
    console.log("  findOntologyRoots (no ontology): OK");
    passed++;
  }

  // ── isNodeclare ───────────────────────────────────────────────
  {
    // entityB has tags: ["nodeclare"] → true
    assert(
      isNodeclare(entityB, [root, ontologyHeading, classHierarchy]),
      "entityB should be nodeclare (own tag)",
    );
    // entityA has no nodeclare tag → false
    assert(
      !isNodeclare(entityA, [root, ontologyHeading, classHierarchy]),
      "entityA should NOT be nodeclare",
    );
    // Test ancestor nodeclare
    const nodeclareParent: ElotNode = {
      level: 2,
      title: "Nodeclare section",
      tags: ["nodeclare"],
      resourcedefs: true,
    };
    const childUnderNodeclare: ElotNode = {
      level: 3,
      title: "Child",
    };
    assert(
      isNodeclare(childUnderNodeclare, [root, ontologyHeading, nodeclareParent]),
      "child under nodeclare parent should be nodeclare",
    );
    console.log("  isNodeclare: OK");
    passed++;
  }

  // ── isCurieKnown ──────────────────────────────────────────────
  {
    const prefixMap = new Map([
      ["pizza", "http://example.org/pizza#"],
      ["obo", "http://purl.obolibrary.org/obo/"],
    ]);
    assert(isCurieKnown("pizza:A", prefixMap), "pizza:A should be known");
    assert(isCurieKnown("obo:BFO_0000001", prefixMap), "obo:BFO_0000001 should be known");
    assert(!isCurieKnown("unknown:X", prefixMap), "unknown:X should NOT be known");
    assert(!isCurieKnown("nocolon", prefixMap), "nocolon should NOT be known");
    console.log("  isCurieKnown: OK");
    passed++;
  }

  // ── walkResourceNodes ─────────────────────────────────────────
  {
    const visited: string[] = [];
    walkResourceNodes(root, (node, _ancestors) => {
      visited.push(node.title);
    });

    // Should visit: classHierarchy, entityA, entityB, objPropHierarchy, entityC
    // Should NOT visit: prefixDefs, noResourcedefsSection, entityD,
    //                   diagramsHeading, diagramContent, ontologyHeading, root
    assert(
      visited.includes(entityA.title),
      "walkResourceNodes should visit entityA",
    );
    assert(
      visited.includes(entityB.title),
      "walkResourceNodes should visit entityB (nodeclare filtering is not in the walker)",
    );
    assert(
      visited.includes(entityC.title),
      "walkResourceNodes should visit entityC",
    );
    assert(
      visited.includes(classHierarchy.title),
      "walkResourceNodes should visit classHierarchy (it has resourcedefs AND is under ontology)",
    );
    assert(
      visited.includes(objPropHierarchy.title),
      "walkResourceNodes should visit objPropHierarchy",
    );
    assert(
      !visited.includes(entityD.title),
      "walkResourceNodes should NOT visit entityD (no resourcedefs)",
    );
    assert(
      !visited.includes(diagramContent.title),
      "walkResourceNodes should NOT visit diagramContent (not under ontology)",
    );
    assert(
      !visited.includes(prefixDefs.title),
      "walkResourceNodes should NOT visit prefixDefs (no resourcedefs)",
    );
    assert(
      !visited.includes(root.title),
      "walkResourceNodes should NOT visit root",
    );
    assert(
      !visited.includes(ontologyHeading.title),
      "walkResourceNodes should NOT visit ontologyHeading (it's not inside its own ontology context)",
    );

    // Check document order: classHierarchy before entityA before entityB
    const classIdx = visited.indexOf(classHierarchy.title);
    const aIdx = visited.indexOf(entityA.title);
    const bIdx = visited.indexOf(entityB.title);
    const objIdx = visited.indexOf(objPropHierarchy.title);
    const cIdx = visited.indexOf(entityC.title);
    assert(classIdx < aIdx, "classHierarchy should come before entityA");
    assert(aIdx < bIdx, "entityA should come before entityB");
    assert(bIdx < objIdx, "entityB should come before objPropHierarchy");
    assert(objIdx < cIdx, "objPropHierarchy should come before entityC");

    console.log(`  walkResourceNodes: OK (visited ${visited.length} nodes: ${visited.join(", ")})`);
    passed++;
  }

  // ── walkResourceNodes: ancestors are correct ──────────────────
  {
    const ancestorMap: Record<string, string[]> = {};
    walkResourceNodes(root, (node, ancestors) => {
      ancestorMap[node.title] = ancestors.map((a) => a.title);
    });

    // entityA's ancestors should be [ROOT, Pizza Ontology, Class hierarchy]
    const aAnc = ancestorMap[entityA.title];
    assert(aAnc !== undefined, "entityA should have been visited");
    assert(aAnc.length === 3, `entityA should have 3 ancestors, got ${aAnc.length}`);
    assert(aAnc[0] === "ROOT", `entityA ancestor[0] should be ROOT, got ${aAnc[0]}`);
    assert(aAnc[1] === ontologyHeading.title, `entityA ancestor[1] should be ontology heading`);
    assert(aAnc[2] === classHierarchy.title, `entityA ancestor[2] should be class hierarchy`);

    // classHierarchy's ancestors should be [ROOT, Pizza Ontology]
    const chAnc = ancestorMap[classHierarchy.title];
    assert(chAnc !== undefined, "classHierarchy should have been visited");
    assert(chAnc.length === 2, `classHierarchy should have 2 ancestors, got ${chAnc.length}`);

    console.log("  walkResourceNodes ancestors: OK");
    passed++;
  }

  console.log(`\n${passed} passed, 0 failed`);
}

main();
