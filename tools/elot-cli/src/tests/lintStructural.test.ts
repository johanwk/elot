// src/tests/lintStructural.test.ts
//
// Tests for Stage 2: structural lint checkers (ontology presence, header, prefix table).

import type { ElotNode } from "../types.js";
import {
  checkOntologyPresence,
  checkOntologyHeader,
  checkPrefixTable,
} from "../lintStructural.js";

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

// ─── Test tree builders ─────────────────────────────────────────

function makeValidOntologyRoot(): ElotNode {
  const prefixDefs: ElotNode = {
    level: 2,
    title: "Prefixes",
    prefixdefs: true,
    prefixes: [
      { prefix: "pizza", uri: "http://example.org/pizza#" },
      { prefix: "owl", uri: "http://www.w3.org/2002/07/owl#" },
    ],
  };

  const classHierarchy: ElotNode = {
    level: 2,
    title: "Class hierarchy",
    id: "pizza-class-hierarchy",
    resourcedefs: true,
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

  return {
    level: 0,
    title: "ROOT",
    children: [ontologyHeading],
  };
}

// ─── Tests ───────────────────────────────────────────────────────

function main() {
  let passed = 0;

  // ── checkOntologyPresence: valid ontology ─────────────────────
  {
    const root = makeValidOntologyRoot();
    const diags = checkOntologyPresence(root);
    assertCount(diags, 0, "presence: valid ontology");
    console.log("  checkOntologyPresence (valid): OK");
    passed++;
  }

  // ── checkOntologyPresence: no ontology heading ────────────────
  {
    const root: ElotNode = {
      level: 0,
      title: "ROOT",
      children: [
        { level: 1, title: "Diagrams" },
        { level: 1, title: "Notes" },
      ],
    };
    const diags = checkOntologyPresence(root);
    assertCount(diags, 1, "presence: no ontology");
    assertHas(diags, "error", "No top-level heading", "presence: no ontology");
    console.log("  checkOntologyPresence (no ontology): OK");
    passed++;
  }

  // ── checkOntologyPresence: empty file ─────────────────────────
  {
    const root: ElotNode = { level: 0, title: "ROOT" };
    const diags = checkOntologyPresence(root);
    assertCount(diags, 1, "presence: empty file");
    console.log("  checkOntologyPresence (empty file): OK");
    passed++;
  }

  // ── checkOntologyHeader: valid ────────────────────────────────
  {
    const root = makeValidOntologyRoot();
    const diags = checkOntologyHeader(root);
    assertCount(diags, 0, "header: valid");
    console.log("  checkOntologyHeader (valid): OK");
    passed++;
  }

  // ── checkOntologyHeader: missing :ID: ─────────────────────────
  {
    const root = makeValidOntologyRoot();
    const onto = root.children![0];
    delete onto.id;
    const diags = checkOntologyHeader(root);
    assertHas(diags, "error", "missing :ID:", "header: missing ID");
    console.log("  checkOntologyHeader (missing ID): OK");
    passed++;
  }

  // ── checkOntologyHeader: empty :ID: ───────────────────────────
  {
    const root = makeValidOntologyRoot();
    const onto = root.children![0];
    onto.id = "";
    const diags = checkOntologyHeader(root);
    assertHas(diags, "error", "missing :ID:", "header: empty ID");
    console.log("  checkOntologyHeader (empty ID): OK");
    passed++;
  }

  // ── checkOntologyHeader: localname mismatch ───────────────────
  {
    const root = makeValidOntologyRoot();
    const onto = root.children![0];
    onto.elotContextLocalname = "different-name";
    const diags = checkOntologyHeader(root);
    assertHas(
      diags,
      "warning",
      ":ELOT-context-localname: should match :ID:",
      "header: localname mismatch",
    );
    console.log("  checkOntologyHeader (localname mismatch): OK");
    passed++;
  }

  // ── checkOntologyHeader: missing tangle target ────────────────
  {
    const root = makeValidOntologyRoot();
    const onto = root.children![0];
    delete onto.tangleTargetOmn;
    const diags = checkOntologyHeader(root);
    assertHas(diags, "error", ":tangle missing", "header: missing tangle");
    console.log("  checkOntologyHeader (missing tangle target): OK");
    passed++;
  }

  // ── checkOntologyHeader: tangle target doesn't end in .omn ───
  {
    const root = makeValidOntologyRoot();
    const onto = root.children![0];
    onto.tangleTargetOmn = "pizza.txt";
    const diags = checkOntologyHeader(root);
    assertHas(diags, "error", ":tangle missing", "header: bad tangle extension");
    console.log("  checkOntologyHeader (bad tangle extension): OK");
    passed++;
  }

  // ── checkOntologyHeader: default prefix with colon ────────────
  {
    const root = makeValidOntologyRoot();
    const onto = root.children![0];
    onto.elotDefaultPrefix = "pizza:";
    const diags = checkOntologyHeader(root);
    // No prefix-related diagnostics at all (check was removed)
    assertCount(diags, 0, "header: prefix with colon should not warn");
    console.log("  checkOntologyHeader (prefix with colon): OK");
    passed++;
  }

  // ── checkPrefixTable: valid ───────────────────────────────────
  {
    const root = makeValidOntologyRoot();
    const diags = checkPrefixTable(root);
    assertCount(diags, 0, "prefix: valid");
    console.log("  checkPrefixTable (valid): OK");
    passed++;
  }

  // ── checkPrefixTable: missing prefix node ─────────────────────
  {
    const ontologyHeading: ElotNode = {
      level: 1,
      title: "Pizza Ontology",
      elotContextType: "ontology",
      id: "pizza",
      children: [
        {
          level: 2,
          title: "Class hierarchy",
          id: "pizza-class-hierarchy",
          resourcedefs: true,
        },
      ],
    };
    const root: ElotNode = {
      level: 0,
      title: "ROOT",
      children: [ontologyHeading],
    };
    const diags = checkPrefixTable(root);
    assertCount(diags, 1, "prefix: missing");
    assertHas(diags, "error", "Prefix table is missing", "prefix: missing");
    console.log("  checkPrefixTable (missing prefix node): OK");
    passed++;
  }

  // ── checkPrefixTable: empty prefix table ──────────────────────
  {
    const ontologyHeading: ElotNode = {
      level: 1,
      title: "Pizza Ontology",
      elotContextType: "ontology",
      id: "pizza",
      children: [
        {
          level: 2,
          title: "Prefixes",
          prefixdefs: true,
          prefixes: [],
        },
      ],
    };
    const root: ElotNode = {
      level: 0,
      title: "ROOT",
      children: [ontologyHeading],
    };
    const diags = checkPrefixTable(root);
    assertCount(diags, 1, "prefix: empty");
    assertHas(diags, "error", "Prefix table is empty", "prefix: empty");
    console.log("  checkPrefixTable (empty prefix table): OK");
    passed++;
  }

  // ── checkPrefixTable: prefixdefs with no prefixes field ───────
  {
    const ontologyHeading: ElotNode = {
      level: 1,
      title: "Pizza Ontology",
      elotContextType: "ontology",
      id: "pizza",
      children: [
        {
          level: 2,
          title: "Prefixes",
          prefixdefs: true,
          // no prefixes field at all
        },
      ],
    };
    const root: ElotNode = {
      level: 0,
      title: "ROOT",
      children: [ontologyHeading],
    };
    const diags = checkPrefixTable(root);
    assertCount(diags, 1, "prefix: no prefixes field");
    assertHas(diags, "error", "Prefix table is empty", "prefix: no prefixes field");
    console.log("  checkPrefixTable (no prefixes field): OK");
    passed++;
  }

  // ── checkOntologyPresence: multiple ontologies ────────────────
  {
    const root: ElotNode = {
      level: 0,
      title: "ROOT",
      children: [
        {
          level: 1,
          title: "Ontology A",
          elotContextType: "ontology",
          id: "onto-a",
        },
        {
          level: 1,
          title: "Ontology B",
          elotContextType: "ontology",
          id: "onto-b",
        },
      ],
    };
    const diags = checkOntologyPresence(root);
    assertCount(diags, 0, "presence: multiple ontologies");
    console.log("  checkOntologyPresence (multiple ontologies): OK");
    passed++;
  }

  // ── checkOntologyHeader: no ontology → no diagnostics ─────────
  {
    const root: ElotNode = {
      level: 0,
      title: "ROOT",
      children: [{ level: 1, title: "Diagrams" }],
    };
    const diags = checkOntologyHeader(root);
    assertCount(diags, 0, "header: no ontology → no diags");
    console.log("  checkOntologyHeader (no ontology): OK");
    passed++;
  }

  console.log(`\n${passed} passed, 0 failed`);
}

main();
