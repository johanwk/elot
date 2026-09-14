// src/tests/lintFactsPunning.test.ts
//
// Tests for checker #9: punned class CURIEs used as individuals.

import type { ElotNode } from "../types.js";
import { checkFactsPunning } from "../lintFactsPunning.js";

function assert(condition: boolean, msg: string) {
  if (!condition) {
    console.error(`FAIL: ${msg}`);
    process.exitCode = 1;
    throw new Error(msg);
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

// ─── Fixture builder ────────────────────────────────────────────

interface Decl {
  uri: string;
  rdfType: string;
}

/**
 * Build a minimal in-scope tree: an ontology heading with a
 * resourcedefs section holding one heading per declaration, plus a
 * subject heading carrying `descriptions`.
 */
function makeTree(decls: Decl[], descriptions: Array<{ tag: string; value: string }>): ElotNode {
  const declNodes: ElotNode[] = decls.map((d, i) => ({
    level: 3,
    title: d.uri,
    uri: d.uri,
    label: `label ${i}`,
    rdfType: d.rdfType,
  }));

  const subject: ElotNode = {
    level: 3,
    title: "subject",
    uri: "ex:subject",
    label: "subject",
    rdfType: "owl:NamedIndividual",
    descriptions,
  };

  return {
    level: 0,
    title: "ROOT",
    children: [
      {
        level: 1,
        title: "Test Ontology",
        elotContextType: "ontology",
        elotContextLocalname: "test",
        id: "test",
        children: [
          {
            level: 2,
            title: "Individuals",
            id: "test-individuals",
            resourcedefs: true,
            children: [...declNodes, subject],
          },
        ],
      },
    ],
  };
}

// ─── Tests ──────────────────────────────────────────────────────

function main() {
  let passed = 0;

  // ── Class used in Facts without individual declaration → error ──
  {
    const root = makeTree(
      [
        { uri: "ex:Gadget", rdfType: "owl:Class" },
        { uri: "ex:relatedTo", rdfType: "owl:ObjectProperty" },
      ],
      [{ tag: "Facts", value: "ex:relatedTo ex:Gadget" }],
    );
    const diags = checkFactsPunning(root);
    assertCount(diags, 1, "punned class in Facts");
    assert(
      diags[0].severity === "error" &&
        diags[0].message.includes("ex:Gadget"),
      "diagnostic should name the punned class as an error",
    );
    console.log("  checkFactsPunning (punned class in Facts): OK");
    passed++;
  }

  // ── Also declared as an individual → no error ──────────────────
  {
    const root = makeTree(
      [
        { uri: "ex:Gadget", rdfType: "owl:Class" },
        { uri: "ex:relatedTo", rdfType: "owl:ObjectProperty" },
      ],
      [{ tag: "Facts", value: "ex:relatedTo ex:Gadget" }],
    );
    // Add a second declaration of ex:Gadget as an individual by
    // overriding the slurp entry: buildSlurp keeps the last node seen.
    const section = root.children![0].children![0];
    section.children!.push({
      level: 3,
      title: "Gadget (individual)",
      uri: "ex:Gadget",
      label: "Gadget",
      rdfType: "owl:NamedIndividual",
    });
    const diags = checkFactsPunning(root);
    assertCount(diags, 0, "class also declared as individual");
    console.log("  checkFactsPunning (also an individual): OK");
    passed++;
  }

  // ── Class that is also a property, in Facts → exempt ───────────
  {
    const root = makeTree(
      [{ uri: "ex:partOf", rdfType: "owl:ObjectProperty" }],
      [{ tag: "Facts", value: "ex:partOf ex:Thing" }],
    );
    const diags = checkFactsPunning(root);
    assertCount(diags, 0, "property in property position");
    console.log("  checkFactsPunning (property position exempt): OK");
    passed++;
  }

  // ── SameAs with a punned class → error ─────────────────────────
  {
    const root = makeTree(
      [{ uri: "ex:Gadget", rdfType: "owl:Class" }],
      [{ tag: "SameAs", value: "ex:Gadget" }],
    );
    const diags = checkFactsPunning(root);
    assertCount(diags, 1, "punned class in SameAs");
    console.log("  checkFactsPunning (SameAs): OK");
    passed++;
  }

  // ── DifferentFrom, comma-separated, two punned classes ─────────
  {
    const root = makeTree(
      [
        { uri: "ex:A", rdfType: "owl:Class" },
        { uri: "ex:B", rdfType: "owl:Class" },
      ],
      [{ tag: "DifferentFrom", value: "ex:A, ex:B" }],
    );
    const diags = checkFactsPunning(root);
    assertCount(diags, 2, "two punned classes");
    console.log("  checkFactsPunning (comma-separated): OK");
    passed++;
  }

  // ── Non-individual rows are ignored ────────────────────────────
  {
    const root = makeTree(
      [{ uri: "ex:Gadget", rdfType: "owl:Class" }],
      [{ tag: "SubClassOf", value: "ex:Gadget" }],
    );
    const diags = checkFactsPunning(root);
    assertCount(diags, 0, "SubClassOf row ignored");
    console.log("  checkFactsPunning (non-individual row ignored): OK");
    passed++;
  }

  // ── Undeclared CURIE is not this checker's business ────────────
  {
    const root = makeTree(
      [],
      [{ tag: "Facts", value: "ex:relatedTo ex:Unknown" }],
    );
    const diags = checkFactsPunning(root);
    assertCount(diags, 0, "undeclared CURIE ignored");
    console.log("  checkFactsPunning (undeclared CURIE ignored): OK");
    passed++;
  }

  console.log(`\nlintFactsPunning tests: ${passed} passed, 0 failed`);
}

main();
