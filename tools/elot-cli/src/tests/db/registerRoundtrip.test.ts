// src/tests/db/registerRoundtrip.test.ts
//
// Step 2.2.3 round-trip: a CSV / TSV / JSON fixture is parsed, fed
// to ElotDb.updateSource(), saved, reopened, and read back via the
// label APIs to confirm the full ingest->read path honours the Step
// 1.16 language conventions.

import { mkdtempSync, rmSync, writeFileSync } from "fs";
import { tmpdir } from "os";
import { join } from "path";
import { ElotDb } from "../../db/sqljs.js";
import { parseSource } from "../../parsers/index.js";

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
function eq<T>(a: T, b: T, msg: string): void {
  if (JSON.stringify(a) !== JSON.stringify(b)) {
    throw new Error(`${msg}\n  got:      ${JSON.stringify(a)}\n  expected: ${JSON.stringify(b)}`);
  }
}

async function run(): Promise<void> {
  const tmp = mkdtempSync(join(tmpdir(), "elot-rt-"));
  const dbPath = join(tmp, "elot.sqlite");

  await t("CSV register+read: lang column + label@TAG", async () => {
    const csv = join(tmp, "labels.csv");
    writeFileSync(
      csv,
      "id,label,lang,label@ko,definition\n" +
        "ex:A,Widget,en,\uC704\uC824,\"A widget.\"\n" +
        "ex:B,Thing,,,\"A thing.\"\n",
      "utf-8",
    );
    const parsed = parseSource(csv, "csv");
    const d = await ElotDb.open(dbPath);
    try {
      const n = d.updateSource("labels", "", "csv", parsed.entries);
      eq(n, 2, "two entities");
      d.addPrefix("labels", "", "ex", "http://example.org/");
      d.save(dbPath);
    } finally {
      d.close();
    }
    const d2 = await ElotDb.open(dbPath);
    try {
      const active = [{ source: "labels", dataSource: "" }];
      // Default prefs: untagged > en > alpha.  A has no untagged row
      // (lang column populated both variants with tags), so en wins.
      eq(d2.getLabel("ex:A", active), "Widget", "A en (default prefs)");
      eq(
        d2.getLabel("ex:A", active, ["ko"]),
        "\uC704\uC824",
        "A ko with ko-preferred",
      );
      eq(d2.getLabel("ex:B", active), "Thing", "B untagged wins");
      // label@TAG row visible as a variant.
      const variants = d2.labelVariants("ex:A", active)!;
      eq(variants.length, 2, "A has two label variants");
    } finally {
      d2.close();
    }
  });

  await t("JSON register+read: nested lang + label@TAG", async () => {
    const json = join(tmp, "nested.json");
    writeFileSync(
      json,
      JSON.stringify({
        "ex:X": {
          label: "Widget",
          lang: "en",
          "label@ko": "\uC704\uC824",
          definition: "A widget.",
        },
      }),
      "utf-8",
    );
    const parsed = parseSource(json, "json");
    const d = await ElotDb.open(dbPath);
    try {
      d.updateSource("nested", "", "json", parsed.entries);
      d.save(dbPath);
    } finally {
      d.close();
    }
    const d2 = await ElotDb.open(dbPath);
    try {
      const active = [{ source: "nested", dataSource: "" }];
      eq(d2.getLabel("ex:X", active), "Widget", "X en default");
      eq(
        d2.getLabel("ex:X", active, ["ko"]),
        "\uC704\uC824",
        "X ko preferred",
      );
      eq(
        d2.getAttr("ex:X", "definition", active),
        "A widget.",
        "definition read back",
      );
    } finally {
      d2.close();
    }
  });

  await t("TSV register+read: backward-compat untagged", async () => {
    const tsv = join(tmp, "plain.tsv");
    writeFileSync(tsv, "id\tlabel\nex:Z\tZed\n", "utf-8");
    const parsed = parseSource(tsv, "tsv");
    const d = await ElotDb.open(dbPath);
    try {
      d.updateSource("plain", "", "tsv", parsed.entries);
      d.save(dbPath);
    } finally {
      d.close();
    }
    const d2 = await ElotDb.open(dbPath);
    try {
      eq(
        d2.getLabel("ex:Z", [{ source: "plain", dataSource: "" }]),
        "Zed",
        "Z untagged",
      );
    } finally {
      d2.close();
    }
  });

  try {
    rmSync(tmp, { recursive: true, force: true });
  } catch {
    /* ignore */
  }
  console.log(`registerRoundtrip tests: ${passed} passed, ${failed} failed`);
  process.exit(failed > 0 ? 1 : 0);
}

run().catch((e) => {
  console.error(e);
  process.exit(1);
});
