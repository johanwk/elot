// src/parsers/ttl.ts
//
// Turtle parser via ROBOT.  Ports elot-source-parse-ttl from
// elot-package/elot-sources.el (Step 1.3 + Step 1.7.3 + Step 1.16.7):
//
//   1. Write the SPARQL label-extraction query to a tempfile.
//   2. Invoke `robot query --input <ttl> --query <q> <csv-out>`.
//   3. Parse the CSV result (reuses parseSeparatedString; the `lang`
//      column produced by LANG(?label) is picked up automatically
//      per Step 1.16.6 / 1.16.7).
//   4. Harvest @prefix declarations from the TTL file itself; the
//      CSV result doesn't carry them.
//
// Per-project query override:
//   <projectRoot>/.elot/ttl-label-query.rq
// overrides the bundled default.  `projectRoot` is detected by
// walking up from the TTL file looking for `.git`, `.elot`, or
// `.elot-cache`; falls back to the file's directory.

import { existsSync, mkdtempSync, readFileSync, rmSync, writeFileSync } from "fs";
import { tmpdir } from "os";
import { dirname, join, resolve } from "path";
import { ParsedSource, parseSeparatedString } from "./csvTsv.js";
import { harvestPrefixes } from "./prefixes.js";
import { robotAvailable, runRobotQuery } from "./robot.js";

export const DEFAULT_TTL_LABEL_QUERY = `PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
SELECT ?id ?label (LANG(?label) AS ?lang) WHERE {
  ?id rdfs:label ?label .
}
`;

export function projectRootFor(file: string): string {
  let dir = resolve(dirname(file));
  for (;;) {
    for (const marker of [".git", ".elot", ".elot-cache"]) {
      if (existsSync(join(dir, marker))) return dir;
    }
    const parent = dirname(dir);
    if (parent === dir) return dirname(resolve(file));
    dir = parent;
  }
}

export function resolveTtlLabelQuery(file: string): string {
  const override = join(projectRootFor(file), ".elot", "ttl-label-query.rq");
  if (existsSync(override)) return readFileSync(override, "utf-8");
  return DEFAULT_TTL_LABEL_QUERY;
}

function mkTmpdir(): string {
  return mkdtempSync(join(tmpdir(), "elot-ttl-"));
}

export function parseTtl(file: string): ParsedSource {
  if (!robotAvailable()) {
    throw new Error(
      "parseTtl: ROBOT not available (set $ELOT_ROBOT_JAR or install `robot`)",
    );
  }
  const abs = resolve(file);
  const query = resolveTtlLabelQuery(abs);
  const tmp = mkTmpdir();
  const queryFile = join(tmp, "label-query.rq");
  const csvFile = join(tmp, "labels.csv");
  writeFileSync(queryFile, query, "utf-8");
  try {
    runRobotQuery(queryFile, abs, csvFile);
    const csvText = readFileSync(csvFile, "utf-8");
    const parsed = parseSeparatedString(csvText, ",");
    return {
      entries: parsed.entries,
      prefixes: harvestPrefixes(abs),
    };
  } finally {
    // Best-effort cleanup; tempfiles are small.
    try {
      rmSync(tmp, { recursive: true, force: true });
    } catch {
      /* ignore */
    }
  }
}
