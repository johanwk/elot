// src/tests/db/golden.test.ts
//
// Slice 3a (Step 2.2.5): byte-identical golden round-trip tests
// against checked-in fixtures.  Both this TS test and the matching
// Elisp test (test/elot-db-golden-test.el) ingest the same source
// fixtures via the language-aware writer, then produce a canonical
// JSON dump (see src/db/goldenDump.ts).  The dumps must be
// byte-identical to the checked-in goldens under
// test/fixtures/golden/.
//
// Bootstrap / regenerate:
//   ELOT_GOLDEN_REGEN=1 npx tsx src/tests/db/golden.test.ts
// then run the matching Elisp test with its own regen flag and
// diff the resulting *.golden.json to verify writer parity.
//
// CSV and JSON are exercised here (always on; no external deps).
// TTL is exercised as a ROBOT-gated extra (skipped without ROBOT).

import { mkdtempSync, readFileSync, writeFileSync, existsSync } from "fs";
import { tmpdir } from "os";
import { join, resolve } from "path";

import { ElotDb } from "../../db/sqljs.js";
import { canonicalDump } from "../../db/goldenDump.js";
import { parseSource } from "../../parsers/index.js";
import { robotAvailable } from "../../parsers/robot.js";

const REGEN = process.env.ELOT_GOLDEN_REGEN === "1";

const REPO_ROOT = resolve(__dirname, "..", "..", "..", "..", "..");
const FIX_DIR = join(REPO_ROOT, "test", "fixtures");
const GOLDEN_DIR = join(FIX_DIR, "golden");

interface Case {
  name: string;
  input: string;
  type: "csv" | "json" | "tsv" | "ttl";
  source: string;
  golden: string;
  gated?: () => boolean; // returns true if the case should run
}

const CASES: Case[] = [
  {
    name: "labels.csv",
    input: join(FIX_DIR, "labels.csv"),
    type: "csv",
    source: "labels",
    golden: join(GOLDEN_DIR, "labels-csv.golden.json"),
  },
  {
    name: "labels-nested.json",
    input: join(FIX_DIR, "labels-nested.json"),
    type: "json",
    source: "labels",
    golden: join(GOLDEN_DIR, "labels-nested-json.golden.json"),
  },
  {
    name: "labels-multilang.ttl (ROBOT)",
    input: join(FIX_DIR, "labels-multilang.ttl"),
    type: "ttl",
    source: "labels",
    golden: join(GOLDEN_DIR, "labels-multilang-ttl.golden.json"),
    gated: () => robotAvailable(),
  },
];

let pass = 0;
let fail = 0;
let skipped = 0;

async function runCase(c: Case): Promise<void> {
  if (c.gated && !c.gated()) {
    console.log(`SKIP ${c.name} (precondition not met)`);
    skipped++;
    return;
  }
  const dir = mkdtempSync(join(tmpdir(), "elot-golden-"));
  const dbPath = join(dir, "elot.sqlite");
  const db = await ElotDb.open(dbPath);
  try {
    const parsed = parseSource(c.input, c.type, { dataSource: null });
    db.updateSource(c.source, "", c.type, parsed.entries, 0);
    if (parsed.prefixes) {
      for (const [p, e] of parsed.prefixes) db.addPrefix(c.source, "", p, e);
    }
    const got = canonicalDump(db);

    if (REGEN) {
      writeFileSync(c.golden, got);
      console.log(`REGEN ${c.name} -> ${c.golden}`);
      pass++;
      return;
    }

    if (!existsSync(c.golden)) {
      console.log(
        `FAIL ${c.name}: golden missing (${c.golden}); ` +
          `run with ELOT_GOLDEN_REGEN=1 to create it`,
      );
      fail++;
      return;
    }
    const want = readFileSync(c.golden, "utf-8");
    if (got === want) {
      pass++;
    } else {
      // First differing byte (or shorter string).
      const min = Math.min(got.length, want.length);
      let diffAt = -1;
      for (let i = 0; i < min; i++) {
        if (got.charCodeAt(i) !== want.charCodeAt(i)) {
          diffAt = i;
          break;
        }
      }
      if (diffAt === -1 && got.length !== want.length) diffAt = min;
      const ctx = (s: string, i: number): string =>
        JSON.stringify(s.slice(Math.max(0, i - 20), i + 20));
      console.log(`FAIL ${c.name}: dump != golden`);
      console.log(`  golden bytes: ${want.length}  got bytes: ${got.length}`);
      console.log(`  first diff @ index ${diffAt}`);
      console.log(`  golden ctx: ${ctx(want, diffAt)}`);
      console.log(`  got    ctx: ${ctx(got, diffAt)}`);
      fail++;
    }
  } finally {
    db.close();
  }
}

(async (): Promise<void> => {
  for (const c of CASES) await runCase(c);
  const total = pass + fail;
  console.log(
    `golden tests: ${pass} passed, ${fail} failed${
      skipped ? `, ${skipped} skipped` : ""
    } (of ${total})`,
  );
  if (fail > 0) process.exit(1);
})();
