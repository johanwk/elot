// src/tests/omnKeywords.test.ts
//
// Quick tests for omnKeywords.ts

import {
  propertyKeywords,
  miscKeywords,
  allKeywords,
  isPropertyKeyword,
  isMiscKeyword,
  isOmnKeyword,
} from "../omnKeywords.js";

let passed = 0;
let failed = 0;

function test(name: string, fn: () => boolean) {
  if (fn()) {
    console.log(`PASS ${name}`);
    passed++;
  } else {
    console.log(`FAIL ${name}`);
    failed++;
  }
}

// --- Property keywords ---
test("SubClassOf is a property keyword", () => isPropertyKeyword("SubClassOf"));
test("DisjointWith is a property keyword", () => isPropertyKeyword("DisjointWith"));
test("Domain is a property keyword", () => isPropertyKeyword("Domain"));
test("Range is a property keyword", () => isPropertyKeyword("Range"));
test("InverseOf is a property keyword", () => isPropertyKeyword("InverseOf"));
test("Characteristics is a property keyword", () => isPropertyKeyword("Characteristics"));
test("SubPropertyOf is a property keyword", () => isPropertyKeyword("SubPropertyOf"));
test("Import is a property keyword", () => isPropertyKeyword("Import"));
test("HasKey is a property keyword", () => isPropertyKeyword("HasKey"));

// --- Misc keywords ---
test("DisjointClasses is a misc keyword", () => isMiscKeyword("DisjointClasses"));
test("EquivalentClasses is a misc keyword", () => isMiscKeyword("EquivalentClasses"));
test("DisjointProperties is a misc keyword", () => isMiscKeyword("DisjointProperties"));
test("Rule is a misc keyword", () => isMiscKeyword("Rule"));

// --- Negative cases ---
test("skos:definition is NOT a property keyword", () => !isPropertyKeyword("skos:definition"));
test("rdfs:label is NOT a property keyword", () => !isPropertyKeyword("rdfs:label"));
test("DisjointClasses is NOT a property keyword", () => !isPropertyKeyword("DisjointClasses"));
test("SubClassOf is NOT a misc keyword", () => !isMiscKeyword("SubClassOf"));
test("rdf:type is NOT an OMN keyword", () => !isOmnKeyword("rdf:type"));
test("dc11:contributor is NOT an OMN keyword", () => !isOmnKeyword("dc11:contributor"));

// --- Combined ---
test("SubClassOf is an OMN keyword", () => isOmnKeyword("SubClassOf"));
test("DisjointClasses is an OMN keyword", () => isOmnKeyword("DisjointClasses"));
test("allKeywords length = property + misc", () =>
  allKeywords.length === propertyKeywords.length + miscKeywords.length
);

// --- Property keywords count ---
test("propertyKeywords has 16 entries", () => propertyKeywords.length === 16);
test("miscKeywords has 7 entries", () => miscKeywords.length === 7);

console.log(`\n${passed} passed, ${failed} failed out of ${passed + failed}`);
if (failed > 0) process.exit(1);
