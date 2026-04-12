// src/tests/lintRequiredSections.test.ts
//
// Tests for Stage 3: required sections checker.

import type { ElotNode } from "../types.js";
import { checkRequiredSections } from "../lintRequiredSections.js";

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

function assertNotHas(
  diagnostics: { message: string; severity: string }[],
  substring: string,
  label: string,
) {
  const found = diagnostics.some((d) => d.message.includes(substring));
  assert(
    !found,
    `${label}: did NOT expect a diagnostic containing "${substring}" — got: ${JSON.stringify(diagnostics.map((d) => `[${d.severity}] ${d.message}`))}`,
  );
}

// ─── Test tree builders ─────────────────────────────────────────

const ALL_SUFFIXES = [
  "-ontology-declaration",
  "-datatypes",
  "-class-hierarchy",
  "-object-property-hierarchy",
  "-data-property-hierarchy",
  "-annotation-property-hierarchy",
  "-individuals",
];

function makeFullOntologyRoot(): ElotNode {
  const sectionChildren: ElotNode[] = ALL_SUFFIXES.map((suffix) => ({
    level: 2,
    title: `Section ${suffix}`,
    id: `pizza${suffix}`,
    resourcedefs: true,
  }));

  const ontologyHeading: ElotNode = {
    level: 1,
    title: "Pizza Ontology",
    elotContextType: "ontology",
    elotContextLocalname: "pizza",
    id: "pizza",
    children: sectionChildren,
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

  // ── All sections present with resourcedefs ────────────────────
  {
    const root = makeFullOntologyRoot();
    const diags = checkRequiredSections(root);
    assertCount(diags, 0, "all sections present");
    console.log("  checkRequiredSections (all present): OK");
    passed++;
  }

  // ── Missing one section (-datatypes) ──────────────────────────
  {
    const root = makeFullOntologyRoot();
    const onto = root.children![0];
    // Remove -datatypes section
    onto.children = onto.children!.filter(
      (c) => c.id !== "pizza-datatypes",
    );
    const diags = checkRequiredSections(root);
    assertCount(diags, 1, "missing -datatypes");
    assertHas(
      diags,
      "warning",
      "Missing section with ID pizza-datatypes",
      "missing -datatypes",
    );
    console.log("  checkRequiredSections (missing -datatypes): OK");
    passed++;
  }

  // ── Section present but without resourcedefs ──────────────────
  {
    const root = makeFullOntologyRoot();
    const onto = root.children![0];
    // Remove resourcedefs from -class-hierarchy
    const classSection = onto.children!.find(
      (c) => c.id === "pizza-class-hierarchy",
    )!;
    classSection.resourcedefs = false;
    const diags = checkRequiredSections(root);
    assertCount(diags, 1, "no resourcedefs on -class-hierarchy");
    assertHas(
      diags,
      "warning",
      "pizza-class-hierarchy should have :resourcedefs: yes",
      "no resourcedefs on -class-hierarchy",
    );
    console.log(
      "  checkRequiredSections (section without resourcedefs): OK",
    );
    passed++;
  }

  // ── Multiple missing sections ─────────────────────────────────
  {
    const root = makeFullOntologyRoot();
    const onto = root.children![0];
    // Remove three sections
    onto.children = onto.children!.filter(
      (c) =>
        c.id !== "pizza-datatypes" &&
        c.id !== "pizza-individuals" &&
        c.id !== "pizza-ontology-declaration",
    );
    const diags = checkRequiredSections(root);
    assertCount(diags, 3, "three missing sections");
    assertHas(diags, "warning", "pizza-datatypes", "missing datatypes");
    assertHas(diags, "warning", "pizza-individuals", "missing individuals");
    assertHas(
      diags,
      "warning",
      "pizza-ontology-declaration",
      "missing ontology-declaration",
    );
    console.log("  checkRequiredSections (multiple missing): OK");
    passed++;
  }

  // ── No ontology heading → no diagnostics ──────────────────────
  {
    const root: ElotNode = {
      level: 0,
      title: "ROOT",
      children: [{ level: 1, title: "Diagrams" }],
    };
    const diags = checkRequiredSections(root);
    assertCount(diags, 0, "no ontology heading");
    console.log("  checkRequiredSections (no ontology heading): OK");
    passed++;
  }

  // ── Ontology heading with no localname → no diagnostics ───────
  {
    const root: ElotNode = {
      level: 0,
      title: "ROOT",
      children: [
        {
          level: 1,
          title: "Some Ontology",
          elotContextType: "ontology",
          id: "onto",
          // no elotContextLocalname
        },
      ],
    };
    const diags = checkRequiredSections(root);
    assertCount(diags, 0, "no localname");
    console.log("  checkRequiredSections (no localname): OK");
    passed++;
  }

  // ── Sections at deeper levels (level 3) ───────────────────────
  {
    const ontologyHeading: ElotNode = {
      level: 1,
      title: "Pizza Ontology",
      elotContextType: "ontology",
      elotContextLocalname: "pizza",
      id: "pizza",
      children: [
        {
          level: 2,
          title: "Wrapper heading",
          children: ALL_SUFFIXES.map((suffix) => ({
            level: 3,
            title: `Section ${suffix}`,
            id: `pizza${suffix}`,
            resourcedefs: true,
          })),
        },
      ],
    };
    const root: ElotNode = {
      level: 0,
      title: "ROOT",
      children: [ontologyHeading],
    };
    const diags = checkRequiredSections(root);
    assertCount(diags, 0, "sections at deeper level");
    console.log("  checkRequiredSections (sections at deeper level): OK");
    passed++;
  }

  // ── Section present with resourcedefs undefined ───────────────
  {
    const root = makeFullOntologyRoot();
    const onto = root.children![0];
    const objSection = onto.children!.find(
      (c) => c.id === "pizza-object-property-hierarchy",
    )!;
    // Remove resourcedefs entirely (undefined, not false)
    delete objSection.resourcedefs;
    const diags = checkRequiredSections(root);
    assertCount(diags, 1, "resourcedefs undefined");
    assertHas(
      diags,
      "warning",
      "pizza-object-property-hierarchy should have :resourcedefs: yes",
      "resourcedefs undefined",
    );
    console.log(
      "  checkRequiredSections (resourcedefs undefined): OK",
    );
    passed++;
  }

  // ── Mixed: one missing, one without resourcedefs ──────────────
  {
    const root = makeFullOntologyRoot();
    const onto = root.children![0];
    // Remove -individuals
    onto.children = onto.children!.filter(
      (c) => c.id !== "pizza-individuals",
    );
    // Remove resourcedefs from -annotation-property-hierarchy
    const annSection = onto.children!.find(
      (c) => c.id === "pizza-annotation-property-hierarchy",
    )!;
    annSection.resourcedefs = false;
    const diags = checkRequiredSections(root);
    assertCount(diags, 2, "mixed missing + no resourcedefs");
    assertHas(
      diags,
      "warning",
      "Missing section with ID pizza-individuals",
      "missing individuals",
    );
    assertHas(
      diags,
      "warning",
      "pizza-annotation-property-hierarchy should have :resourcedefs: yes",
      "no resourcedefs on annotation",
    );
    console.log(
      "  checkRequiredSections (mixed missing + no resourcedefs): OK",
    );
    passed++;
  }

  // ── Diagnostic for missing section points to ontology root ────
  {
    const root = makeFullOntologyRoot();
    const onto = root.children![0];
    onto.children = onto.children!.filter(
      (c) => c.id !== "pizza-datatypes",
    );
    const diags = checkRequiredSections(root);
    assert(
      diags.length === 1 && diags[0].node === onto,
      "missing section diagnostic should reference ontology root node",
    );
    console.log(
      "  checkRequiredSections (missing diag points to ontology root): OK",
    );
    passed++;
  }

  // ── Diagnostic for missing resourcedefs points to section node ─
  {
    const root = makeFullOntologyRoot();
    const onto = root.children![0];
    const classSection = onto.children!.find(
      (c) => c.id === "pizza-class-hierarchy",
    )!;
    classSection.resourcedefs = false;
    const diags = checkRequiredSections(root);
    assert(
      diags.length === 1 && diags[0].node === classSection,
      "resourcedefs diagnostic should reference the section node",
    );
    console.log(
      "  checkRequiredSections (resourcedefs diag points to section node): OK",
    );
    passed++;
  }

  // ── All sections missing → 7 warnings ─────────────────────────
  {
    const root: ElotNode = {
      level: 0,
      title: "ROOT",
      children: [
        {
          level: 1,
          title: "Pizza Ontology",
          elotContextType: "ontology",
          elotContextLocalname: "pizza",
          id: "pizza",
          children: [],
        },
      ],
    };
    const diags = checkRequiredSections(root);
    assertCount(diags, 7, "all sections missing");
    for (const suffix of ALL_SUFFIXES) {
      assertHas(
        diags,
        "warning",
        `pizza${suffix}`,
        `all missing: ${suffix}`,
      );
    }
    console.log("  checkRequiredSections (all sections missing → 7): OK");
    passed++;
  }

  // ── Existing section not counted for different ontology ────────
  {
    // Two ontology headings, sections belong to "pizza" not "wine"
    const root: ElotNode = {
      level: 0,
      title: "ROOT",
      children: [
        {
          level: 1,
          title: "Pizza Ontology",
          elotContextType: "ontology",
          elotContextLocalname: "pizza",
          id: "pizza",
          children: ALL_SUFFIXES.map((suffix) => ({
            level: 2,
            title: `Section ${suffix}`,
            id: `pizza${suffix}`,
            resourcedefs: true,
          })),
        },
        {
          level: 1,
          title: "Wine Ontology",
          elotContextType: "ontology",
          elotContextLocalname: "wine",
          id: "wine",
          children: [],
        },
      ],
    };
    const diags = checkRequiredSections(root);
    // Pizza should be fine (0 warnings), wine should have 7
    assertCount(diags, 7, "wine missing all sections");
    for (const suffix of ALL_SUFFIXES) {
      assertHas(diags, "warning", `wine${suffix}`, `wine: ${suffix}`);
      assertNotHas(diags, `pizza${suffix}`, `pizza should be fine: ${suffix}`);
    }
    console.log(
      "  checkRequiredSections (two ontologies, one complete, one empty): OK",
    );
    passed++;
  }

  console.log(`\n${passed} passed, 0 failed`);
}

main();
