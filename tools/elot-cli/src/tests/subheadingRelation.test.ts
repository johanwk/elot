// src/tests/subheadingRelation.test.ts
//
// Tests for the :ELOT-subheading-relation: feature (port of 8242d25).
//
// A heading carrying :ELOT-subheading-relation: <objectProperty> makes every
// owl:NamedIndividual descendant emit `Facts: <property> <parent-uri>`
// relating it to its *immediate* parent heading.  The value is inherited
// down the subtree; a nested heading's own value overrides it.

import { parseOrg } from "../parseOrgWasm.js";
import { omnResourceDeclarations } from "../omnDeclarations.js";

let passed = 0;
let failed = 0;

function check(name: string, cond: boolean, extra?: string): void {
  if (cond) {
    console.log(`  ${name}: OK`);
    passed++;
  } else {
    console.log(`  FAIL: ${name}${extra ? " -- " + extra : ""}`);
    failed++;
  }
}

function omnFor(org: string): string {
  const root = parseOrg(org);
  return omnResourceDeclarations(root.children ?? [], null, null);
}

// --- 1. basic: relation on the section heading -------------------

const basic = `* onto
:PROPERTIES:
:ID:       onto
:ELOT-context-type: ontology
:END:
** Individuals
:PROPERTIES:
:ID:       onto-individuals
:resourcedefs: yes
:ELOT-subheading-relation: ex:partOf
:END:
*** Norway (ex:Norway)
**** Oslo (ex:Oslo)
`;

{
  const omn = omnFor(basic);
  check(
    "child individual gets Facts to immediate parent",
    /Individual: ex:Oslo[\s\S]*Facts: ex:partOf ex:Norway/.test(omn),
    omn
  );
  check(
    "top individual gets no Facts (no individual parent)",
    !/Individual: ex:Norway[\s\S]*?Facts:/.test(
      omn.slice(omn.indexOf("Individual: ex:Norway"), omn.indexOf("Individual: ex:Oslo"))
    ),
    omn
  );
  check(
    "no SubClassOf emitted for individuals",
    !omn.includes("SubClassOf"),
    omn
  );
}

// --- 2. inheritance three levels deep ----------------------------

const deep = `* onto
:PROPERTIES:
:ID:       onto
:ELOT-context-type: ontology
:END:
** Individuals
:PROPERTIES:
:ID:       onto-individuals
:resourcedefs: yes
:ELOT-subheading-relation: ex:partOf
:END:
*** A (ex:A)
**** B (ex:B)
***** C (ex:C)
`;

{
  const omn = omnFor(deep);
  check(
    "grandchild relates to its immediate parent, not the root",
    /Individual: ex:C[\s\S]*Facts: ex:partOf ex:B/.test(omn),
    omn
  );
}

// --- 3. nested override ------------------------------------------

const override = `* onto
:PROPERTIES:
:ID:       onto
:ELOT-context-type: ontology
:END:
** Individuals
:PROPERTIES:
:ID:       onto-individuals
:resourcedefs: yes
:ELOT-subheading-relation: ex:partOf
:END:
*** A (ex:A)
:PROPERTIES:
:ELOT-subheading-relation: ex:memberOf
:END:
**** B (ex:B)
`;

{
  const omn = omnFor(override);
  check(
    "nested :ELOT-subheading-relation: overrides the inherited one",
    /Individual: ex:B[\s\S]*Facts: ex:memberOf ex:A/.test(omn),
    omn
  );
  check("overridden relation is not used", !omn.includes("ex:partOf"), omn);
}

// --- 4. no relation property -> no Facts -------------------------

const none = `* onto
:PROPERTIES:
:ID:       onto
:ELOT-context-type: ontology
:END:
** Individuals
:PROPERTIES:
:ID:       onto-individuals
:resourcedefs: yes
:END:
*** A (ex:A)
**** B (ex:B)
`;

{
  const omn = omnFor(none);
  check("without the property, no Facts axiom is emitted", !omn.includes("Facts:"), omn);
}

// --- 5. classes are unaffected -----------------------------------

const classes = `* onto
:PROPERTIES:
:ID:       onto
:ELOT-context-type: ontology
:END:
** Classes
:PROPERTIES:
:ID:       onto-class-hierarchy
:resourcedefs: yes
:ELOT-subheading-relation: ex:partOf
:END:
*** A (ex:A)
**** B (ex:B)
`;

{
  const omn = omnFor(classes);
  check("classes still get SubClassOf", omn.includes("SubClassOf: ex:A"), omn);
  check("classes get no Facts axiom", !omn.includes("Facts:"), omn);
}

// --- 6. explicit Facts row is not duplicated ---------------------

const explicit = `* onto
:PROPERTIES:
:ID:       onto
:ELOT-context-type: ontology
:END:
** Individuals
:PROPERTIES:
:ID:       onto-individuals
:resourcedefs: yes
:ELOT-subheading-relation: ex:partOf
:END:
*** A (ex:A)
**** B (ex:B)
 - Facts :: ex:partOf ex:A
`;

{
  const omn = omnFor(explicit);
  const count = (omn.match(/Facts: ex:partOf ex:A/g) ?? []).length;
  check("an explicit identical Facts row is not duplicated", count === 1, omn);
}

console.log(
  `\nsubheadingRelation tests: ${passed} passed, ${failed} failed`
);
process.exitCode = failed > 0 ? 1 : 0;
