// src/tests/db/rqLive.test.ts
//
// Live RQ test against the POSC Caesar Fuseki endpoint.
//
// Double-gated by design:
//   - skipped unless ROBOT is available (set $ELOT_ROBOT_JAR or
//     install `robot` on PATH), AND
//   - skipped unless $ELOT_TEST_NETWORK=1.
//
// Exercises:
//   1. parseRq() with a SERVICE-federated SPARQL query against a
//      real http(s) endpoint (the data-source argument is the
//      endpoint URL; ROBOT runs against a synthesised dummy TTL,
//      and the query's SERVICE clause does the real work).
//   2. .elot-cache freshness contract: a second invocation must
//      hit the cache and skip ROBOT entirely (verified by mtime).
//   3. End-to-end ingest -> ElotDb.updateSource -> getLabel for a
//      known-stable id (PCA_100000011 -> "Pumping").

import { copyFileSync, mkdtempSync, rmSync, statSync, existsSync } from "fs";
import { tmpdir } from "os";
import { join, resolve } from "path";
import { ElotDb } from "../../db/sqljs.js";
import { parseRq, rqCachePath } from "../../parsers/rq.js";
import { robotAvailable } from "../../parsers/robot.js";

let passed = 0;
let failed = 0;
function t(name: string, fn: () => Promise<void> | void): Promise<void> {
  return Promise.resolve(fn()).then(
    () => {
      passed++;
    },
    (e) => {
      failed++;
      console.error(`FAIL ${name}:`, e);
    },
  );
}
function assert(cond: unknown, msg: string): void {
  if (!cond) throw new Error(msg);
}

async function run(): Promise<void> {
  if (!robotAvailable()) {
    console.log(
      "rqLive tests: skipped (ROBOT not available; set $ELOT_ROBOT_JAR or install `robot` on PATH)",
    );
    process.exit(0);
  }
  if (!process.env.ELOT_TEST_NETWORK) {
    console.log(
      "rqLive tests: skipped (network gate; set ELOT_TEST_NETWORK=1 to run)",
    );
    process.exit(0);
  }

  const tmp = mkdtempSync(join(tmpdir(), "elot-rq-live-"));
  // .git marker so projectRootFor() lands the cache under tmp/.
  require("fs").mkdirSync(join(tmp, ".git"), { recursive: true });

  const fixture = resolve(
    __dirname,
    "..",
    "..",
    "..",
    "..",
    "..",
    "test",
    "fixtures",
    "rq",
    "pca-process-labels.rq",
  );
  const rq = join(tmp, "pca-process-labels.rq");
  copyFileSync(fixture, rq);
  const endpoint =
    "https://rds.posccaesar.org/ontology/fuseki/ontology/sparql";

  let cachePath = "";
  let firstMtime = 0;

  await t("parseRq: live federated query returns at least one labelled id", () => {
    const parsed = parseRq(rq, { dataSource: endpoint });
    assert(parsed.entries.length > 0, "no entries returned from live query");
    cachePath = rqCachePath(rq, endpoint);
    assert(existsSync(cachePath), `cache file not written: ${cachePath}`);
    firstMtime = statSync(cachePath).mtimeMs;
    // ROBOT emits a two-column "id,label" CSV; parseSeparatedString
    // stores the second column on entry.label (denormalised) and only
    // emits rdfs:label attribute rows when a `lang` column or
    // `label@TAG` headers are present.  So we check entry.label here
    // rather than attrs.
    const labelled = parsed.entries.filter(
      (e) => typeof e.label === "string" && e.label.length > 0,
    );
    assert(
      labelled.length > 0,
      `no labelled entries in result (got ${parsed.entries.length} entries, none with .label)`,
    );
  });

  await t("parseRq: second call hits cache (no ROBOT re-run)", async () => {
    // Wait a moment so that any rewrite would produce a strictly
    // newer mtime; if the cache is honoured, mtime stays unchanged.
    await new Promise((r) => setTimeout(r, 1100));
    const parsed = parseRq(rq, { dataSource: endpoint });
    assert(parsed.entries.length > 0, "second call returned no entries");
    const m2 = statSync(cachePath).mtimeMs;
    assert(
      m2 === firstMtime,
      `cache was rewritten on second call (mtime ${firstMtime} -> ${m2})`,
    );
  });

  await t("parseRq -> updateSource -> getLabel round-trip on a stable id", async () => {
    const parsed = parseRq(rq, { dataSource: endpoint });
    const dbPath = join(tmp, "elot.sqlite");
    const d = await ElotDb.open(dbPath);
    try {
      d.updateSource("pca", endpoint, "rq", parsed.entries);
      if (parsed.prefixes) {
        for (const [p, e] of parsed.prefixes) d.addPrefix("pca", endpoint, p, e);
      }
      d.save(dbPath);
    } finally {
      d.close();
    }
    const d2 = await ElotDb.open(dbPath);
    try {
      const active = [{ source: "pca", dataSource: endpoint }];
      // PCA_100000011 -> "Pumping" is stable per the fixture comment
      // in the user's plan; if upstream renames it, relax the match
      // to "non-empty label".
      const id = "http://rds.posccaesar.org/ontology/plm/rdl/PCA_100000011";
      const label = d2.getLabel(id, active);
      assert(
        typeof label === "string" && label.length > 0,
        `expected non-empty label for ${id}, got ${JSON.stringify(label)}`,
      );
      // Soft check; warn but don't fail if upstream relabels.
      if (label !== "Pumping") {
        console.warn(
          `note: PCA_100000011 label is now ${JSON.stringify(label)} (expected "Pumping")`,
        );
      }
    } finally {
      d2.close();
    }
  });

  try {
    rmSync(tmp, { recursive: true, force: true });
  } catch {
    /* ignore */
  }
  console.log(`rqLive tests: ${passed} passed, ${failed} failed`);
  process.exit(failed > 0 ? 1 : 0);
}

run().catch((e) => {
  console.error(e);
  process.exit(1);
});
