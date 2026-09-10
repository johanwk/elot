// src/tests/omnImport.test.ts
//
// `Import:` requires a full IRI in angle brackets in Manchester Syntax.
// elot-tangle.el expands a CURIE against the prefix table; the TS port
// used to pass CURIEs through unchanged, which produced
//
//     Import: itxm-ont:itxm-generic-components
//
// and an import that silently fails to resolve.  These tests pin the
// expansion, the other accepted input shapes, and the loud failure when
// the value cannot be resolved to a full IRI.

import { formatRestrictions } from "../omnFrame.js";

let passed = 0;
let failed = 0;

function check(name: string, cond: boolean, detail = ""): void {
  if (cond) {
    console.log(`  ${name}: OK`);
    passed++;
  } else {
    console.log(`  FAIL: ${name}${detail ? "\n    " + detail : ""}`);
    failed++;
  }
}

function eq(name: string, actual: string, expected: string): void {
  check(
    name,
    actual === expected,
    `expected: ${expected}\n    actual:   ${actual}`
  );
}

function throws(name: string, fn: () => unknown): void {
  let threw = false;
  try {
    fn();
  } catch {
    threw = true;
  }
  check(name, threw, "expected a thrown error, got none");
}

const prefixes = new Map<string, string>([
  ["itxm-ont", "http://itxmaritime.org/ontology/itxm/ont/"],
  ["", "http://example.org/default/"],
]);

function imp(value: string, map: Map<string, string> | null = prefixes): string {
  return formatRestrictions([{ tag: "Import", value }], 4, map);
}

console.log("\n=== Import: IRI handling ===");

eq(
  "CURIE is expanded to a full IRI in angle brackets",
  imp("itxm-ont:itxm-generic-components"),
  "    Import: <http://itxmaritime.org/ontology/itxm/ont/itxm-generic-components>"
);

eq(
  "default-prefix CURIE is expanded",
  imp(":thing"),
  "    Import: <http://example.org/default/thing>"
);

eq(
  "bare http URI is wrapped",
  imp("http://example.org/o"),
  "    Import: <http://example.org/o>"
);

eq(
  "already-bracketed IRI is left alone",
  imp("<http://example.org/o>"),
  "    Import: <http://example.org/o>"
);

eq(
  "surrounding whitespace is trimmed",
  imp("  itxm-ont:itxm-generic-components  "),
  "    Import: <http://itxmaritime.org/ontology/itxm/ont/itxm-generic-components>"
);

throws("unknown prefix is an error, not a silently invalid import", () =>
  imp("nosuch:thing")
);

throws("missing prefix table is an error for a CURIE value", () =>
  imp("itxm-ont:itxm-generic-components", null)
);

console.log(`\nomnImport tests: ${passed} passed, ${failed} failed`);
process.exitCode = failed > 0 ? 1 : 0;
