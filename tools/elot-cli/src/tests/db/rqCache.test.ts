// src/tests/db/rqCache.test.ts
//
// Step 2.2.4 - .rq caching policy tests (no ROBOT required).
// Mirrors the parts of test/elot-sources-test.el that exercise
// elot-source--rq-cache-fresh-p / rq-cache-path without actually
// invoking ROBOT.  Cache freshness depends only on file mtimes.

import { mkdtempSync, rmSync, writeFileSync, utimesSync, mkdirSync } from "fs";
import { tmpdir } from "os";
import { join } from "path";
import {
  rqCachePath,
  rqCacheFresh,
  RQ_CACHE_DIR_NAME,
} from "../../parsers/rq.js";

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
function assert(c: unknown, m: string): void {
  if (!c) throw new Error(m);
}

const tmp = mkdtempSync(join(tmpdir(), "elot-rq-"));
// Create a .git marker so projectRootFor returns `tmp` (and the
// cache lands in tmp/.elot-cache), keeping the test hermetic.
mkdirSync(join(tmp, ".git"), { recursive: true });

function touch(path: string, atSec: number): void {
  utimesSync(path, atSec, atSec);
}

const query = join(tmp, "q.rq");
writeFileSync(
  query,
  "PREFIX ex: <http://example.org/>\nSELECT ?id WHERE { ?id a ex:Thing . }\n",
  "utf-8",
);
const data = join(tmp, "data.ttl");
writeFileSync(data, "@prefix ex: <http://example.org/> .\n", "utf-8");

t("rqCachePath lives under projectRoot/.elot-cache and encodes dataSource", () => {
  const p1 = rqCachePath(query, data);
  const p2 = rqCachePath(query, "http://endpoint/");
  assert(p1.includes(RQ_CACHE_DIR_NAME), "cache dir in path");
  assert(p1.endsWith(".csv"), "cache ends in .csv");
  assert(p1 !== p2, "different data sources -> different cache files");
});

t("rqCacheFresh: cache missing -> false", () => {
  assert(
    !rqCacheFresh(join(tmp, "nope.csv"), query, data),
    "missing cache not fresh",
  );
});

t("rqCacheFresh: cache newer than query+data -> true", () => {
  const cache = rqCachePath(query, data);
  mkdirSync(join(cache, ".."), { recursive: true });
  writeFileSync(cache, "id,label\nex:a,A\n", "utf-8");
  const past = Math.floor(Date.now() / 1000) - 1000;
  const future = past + 500;
  touch(query, past);
  touch(data, past);
  touch(cache, future);
  assert(rqCacheFresh(cache, query, data), "newer cache is fresh");
});

t("rqCacheFresh: query newer than cache -> false", () => {
  const cache = rqCachePath(query, data);
  const past = Math.floor(Date.now() / 1000) - 1000;
  const future = past + 500;
  touch(cache, past);
  touch(data, past);
  touch(query, future);
  assert(!rqCacheFresh(cache, query, data), "stale vs query");
});

t("rqCacheFresh: data newer than cache -> false (local only)", () => {
  const cache = rqCachePath(query, data);
  const past = Math.floor(Date.now() / 1000) - 1000;
  const future = past + 500;
  touch(cache, past);
  touch(query, past);
  touch(data, future);
  assert(!rqCacheFresh(cache, query, data), "stale vs local data");
});

t("rqCacheFresh: http endpoint -> data-source mtime ignored", () => {
  const cache = rqCachePath(query, "http://endpoint/");
  mkdirSync(join(cache, ".."), { recursive: true });
  writeFileSync(cache, "id,label\n", "utf-8");
  const past = Math.floor(Date.now() / 1000) - 1000;
  const future = past + 500;
  touch(query, past);
  touch(cache, future);
  assert(
    rqCacheFresh(cache, query, "http://endpoint/"),
    "http endpoint ignored",
  );
});

try {
  rmSync(tmp, { recursive: true, force: true });
} catch {
  /* ignore */
}

console.log(`rqCache tests: ${passed} passed, ${failed} failed`);
process.exit(failed > 0 ? 1 : 0);
