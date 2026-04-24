// src/parsers/rq.ts
//
// SPARQL .rq parser via ROBOT.  Ports elot-source-parse-rq from
// elot-package/elot-sources.el (Step 1.3.1 + Step 1.7.3).
//
// Semantics:
//
//   - Local data source (file path): ROBOT invoked with --input=<ds>.
//   - http(s) endpoint data source: ROBOT gets a dummy empty TTL as
//     input and the query's own SERVICE clause is expected to carry
//     the endpoint (or, in practice, a project will pre-expand the
//     query).  This matches the Elisp behaviour.
//   - Cache: <projectRoot>/.elot-cache/<base>.<hash>.csv where hash
//     is sha1(dataSource || "")[0..8].  If the cache is newer than
//     both the query and (for local) the data source, ROBOT is NOT
//     re-run.  A fresh but empty result leaves the existing cache
//     untouched, matching the Elisp "preserve cache on empty/failure"
//     contract.
//   - Returns { entries, prefixes } where prefixes come from the
//     .rq file's own PREFIX lines (the cached CSV doesn't carry
//     them).

import {
  existsSync,
  mkdirSync,
  readFileSync,
  renameSync,
  rmSync,
  statSync,
  writeFileSync,
} from "fs";
import { createHash } from "crypto";
import { tmpdir } from "os";
import { dirname, join, resolve, basename, extname } from "path";
import { ParsedSource, parseSeparatedString } from "./csvTsv.js";
import { harvestPrefixes } from "./prefixes.js";
import { robotAvailable, runRobotQuery } from "./robot.js";
import { projectRootFor } from "./ttl.js";

export const RQ_CACHE_DIR_NAME = ".elot-cache";

export function rqCachePath(
  queryFile: string,
  dataSource: string | null | undefined,
): string {
  const root = projectRootFor(queryFile);
  const base = basename(queryFile, extname(queryFile));
  const hash = createHash("sha1")
    .update(dataSource ?? "")
    .digest("hex")
    .slice(0, 8);
  return join(root, RQ_CACHE_DIR_NAME, `${base}.${hash}.csv`);
}

function mtime(path: string): number | null {
  try {
    return statSync(path).mtimeMs / 1000;
  } catch {
    return null;
  }
}

function isHttp(ds: string | null | undefined): boolean {
  return !!ds && /^https?:\/\//i.test(ds);
}

export function rqCacheFresh(
  cache: string,
  queryFile: string,
  dataSource: string | null | undefined,
): boolean {
  const c = mtime(cache);
  const q = mtime(queryFile);
  if (c == null || q == null) return false;
  if (c < q) return false;
  if (dataSource && !isHttp(dataSource)) {
    const d = mtime(dataSource);
    if (d != null && c < d) return false;
  }
  return true;
}

function executeRq(
  queryFile: string,
  dataSource: string | null | undefined,
  outputFile: string,
): void {
  if (!robotAvailable()) {
    throw new Error(
      "parseRq: ROBOT not available (set $ELOT_ROBOT_JAR or install `robot`)",
    );
  }
  if (!dataSource) {
    throw new Error("parseRq: .rq source requires a --data-source");
  }
  if (isHttp(dataSource)) {
    // Synthesise a dummy input so ROBOT is happy; the query's SERVICE
    // clause is expected to reach the endpoint.
    const dummy = join(tmpdir(), `elot-rq-dummy-${process.pid}.ttl`);
    writeFileSync(
      dummy,
      "@prefix owl: <http://www.w3.org/2002/07/owl#> .\n",
      "utf-8",
    );
    try {
      runRobotQuery(queryFile, dummy, outputFile);
    } finally {
      try {
        rmSync(dummy, { force: true });
      } catch {
        /* ignore */
      }
    }
  } else {
    runRobotQuery(queryFile, resolve(dataSource), outputFile);
  }
}

function csvLooksNonEmpty(path: string): boolean {
  try {
    const text = readFileSync(path, "utf-8");
    const lines = text.split(/\r?\n/).filter((l) => l.length > 0);
    return lines.length >= 2; // header + at least one row
  } catch {
    return false;
  }
}

export interface ParseRqOptions {
  dataSource?: string | null;
}

export function parseRq(
  queryFile: string,
  opts: ParseRqOptions = {},
): ParsedSource {
  const absQuery = resolve(queryFile);
  const dataSource = opts.dataSource ?? null;
  const cache = rqCachePath(absQuery, dataSource);
  const cacheDir = dirname(cache);

  if (!rqCacheFresh(cache, absQuery, dataSource)) {
    if (!existsSync(cacheDir)) mkdirSync(cacheDir, { recursive: true });
    const tmp = `${cache}.tmp`;
    try {
      executeRq(absQuery, dataSource, tmp);
      if (csvLooksNonEmpty(tmp)) {
        renameSync(tmp, cache);
      } else {
        try {
          rmSync(tmp, { force: true });
        } catch {
          /* ignore */
        }
      }
    } catch (e) {
      try {
        rmSync(tmp, { force: true });
      } catch {
        /* ignore */
      }
      if (!existsSync(cache)) throw e;
      // Keep stale cache on failure.
    }
  }

  const entries = existsSync(cache)
    ? parseSeparatedString(readFileSync(cache, "utf-8"), ",").entries
    : [];
  return { entries, prefixes: harvestPrefixes(absQuery) };
}
