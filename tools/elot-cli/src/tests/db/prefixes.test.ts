// src/tests/db/prefixes.test.ts
//
// Step 2.2.4 - harvestPrefixes() port of elot-source--harvest-prefixes.

import { mkdtempSync, rmSync, writeFileSync } from "fs";
import { tmpdir } from "os";
import { join } from "path";
import { harvestPrefixes } from "../../parsers/prefixes.js";

let passed = 0;
let failed = 0;
function t(name: string, fn: () => void): void {
  try {
    fn();
    passed++;
  } catch (e) {
    failed++;
    console.error(`FAIL ${name}:`, e);
  }
}
function eq<T>(a: T, b: T, msg: string): void {
  if (JSON.stringify(a) !== JSON.stringify(b)) {
    throw new Error(
      `${msg}\n  got:      ${JSON.stringify(a)}\n  expected: ${JSON.stringify(b)}`,
    );
  }
}

const tmp = mkdtempSync(join(tmpdir(), "elot-prefixes-"));

function write(name: string, content: string): string {
  const p = join(tmp, name);
  writeFileSync(p, content, "utf-8");
  return p;
}

t("harvest TTL @prefix declarations in order", () => {
  const f = write(
    "a.ttl",
    "@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .\n" +
      "@prefix ex: <http://example.org/ex/> .\n" +
      "ex:Widget rdfs:label \"W\" .\n",
  );
  eq(
    harvestPrefixes(f),
    [
      ["rdfs", "http://www.w3.org/2000/01/rdf-schema#"],
      ["ex", "http://example.org/ex/"],
    ],
    "ttl prefixes harvested",
  );
});

t("harvest SPARQL PREFIX declarations (case-insensitive)", () => {
  const f = write(
    "q.rq",
    "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>\n" +
      "prefix ex:   <http://example.org/ex/>\n" +
      "SELECT ?id WHERE { ?id rdfs:label ?l . }\n",
  );
  eq(
    harvestPrefixes(f),
    [
      ["rdfs", "http://www.w3.org/2000/01/rdf-schema#"],
      ["ex", "http://example.org/ex/"],
    ],
    "sparql prefixes harvested",
  );
});

t("empty prefix supported", () => {
  const f = write(
    "b.ttl",
    "@prefix : <http://example.org/root/> .\n:Thing a :Class .\n",
  );
  eq(harvestPrefixes(f), [["", "http://example.org/root/"]], "empty prefix");
});

t("first occurrence wins on duplicates", () => {
  const f = write(
    "c.ttl",
    "@prefix ex: <http://a/> .\n@prefix ex: <http://b/> .\n",
  );
  eq(harvestPrefixes(f), [["ex", "http://a/"]], "first wins");
});

t("missing file -> []", () => {
  eq(harvestPrefixes(join(tmp, "nope.ttl")), [], "missing -> []");
});

try {
  rmSync(tmp, { recursive: true, force: true });
} catch {
  /* ignore */
}

console.log(`prefixes tests: ${passed} passed, ${failed} failed`);
process.exit(failed > 0 ? 1 : 0);
