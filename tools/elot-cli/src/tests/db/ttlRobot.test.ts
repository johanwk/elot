// src/tests/db/ttlRobot.test.ts
//
// Step 2.2.4 - end-to-end TTL ingest via ROBOT.  Mirrors
// test/elot-sources-ttl-lang-test.el: multi-language TTL is parsed
// through ROBOT, fed to ElotDb.updateSource(), and read back via
// getLabel() with language preferences.
//
// ROBOT-gated: if robotAvailable() returns false, tests print
// "skipped" and exit 0.  Set $ELOT_ROBOT_JAR or install `robot`
// on PATH to exercise the full path.

import { copyFileSync, mkdtempSync, rmSync } from "fs";
import { tmpdir } from "os";
import { join, resolve } from "path";
import { ElotDb } from "../../db/sqljs.js";
import { parseTtl } from "../../parsers/ttl.js";
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
function eq<T>(a: T, b: T, msg: string): void {
  if (JSON.stringify(a) !== JSON.stringify(b)) {
    throw new Error(
      `${msg}\n  got:      ${JSON.stringify(a)}\n  expected: ${JSON.stringify(b)}`,
    );
  }
}

async function run(): Promise<void> {
  if (!robotAvailable()) {
    console.log(
      "ttlRobot tests: skipped (ROBOT not available; set $ELOT_ROBOT_JAR or install `robot` on PATH)",
    );
    process.exit(0);
  }

  const tmp = mkdtempSync(join(tmpdir(), "elot-ttl-"));
  // Reuse the Elisp-side fixture so TTL parity is checked against
  // the same bytes both sides ingest.
  const fixture = resolve(
    __dirname,
    "..",
    "..",
    "..",
    "..",
    "..",
    "test",
    "fixtures",
    "labels-multilang.ttl",
  );
  const ttl = join(tmp, "labels-multilang.ttl");
  copyFileSync(fixture, ttl);
  const dbPath = join(tmp, "elot.sqlite");

  await t("TTL ingest preserves language tags", () => {
    const parsed = parseTtl(ttl);
    eq(
      parsed.prefixes,
      [
        ["rdfs", "http://www.w3.org/2000/01/rdf-schema#"],
        ["ex", "http://example.org/ex/"],
      ],
      "prefixes harvested",
    );
    // Widget has three rdfs:label variants (en, ko, untagged).  ROBOT
    // emits one CSV row per triple, so the parser returns three separate
    // EntityTriple entries sharing the same id; updateSource() coalesces
    // them.  Aggregate across all of them here.
    const widgetEntries = parsed.entries.filter(
      (e) => e.id === "http://example.org/ex/Widget",
    );
    if (widgetEntries.length === 0)
      throw new Error("Widget entity not found");
    const labels = widgetEntries.flatMap((e) =>
      (e.attrs ?? []).filter(([p]) => p === "rdfs:label"),
    );
    eq(labels.length, 3, "three rdfs:label rows for Widget");
    const tags = labels
      .map(([, v]) => (typeof v === "string" ? "" : v.lang))
      .sort();
    eq(tags, ["", "en", "ko"], "tags en/ko/untagged");
  });

  await t("TTL ingest -> DB -> getLabel respects prefs", async () => {
    const parsed = parseTtl(ttl);
    const d = await ElotDb.open(dbPath);
    try {
      d.updateSource("ttl", "", "ttl", parsed.entries);
      if (parsed.prefixes) {
        for (const [p, e] of parsed.prefixes) d.addPrefix("ttl", "", p, e);
      }
      d.save(dbPath);
    } finally {
      d.close();
    }
    const d2 = await ElotDb.open(dbPath);
    try {
      const active = [{ source: "ttl", dataSource: "" }];
      const id = "http://example.org/ex/Widget";
      // Default prefs: untagged first.
      eq(
        d2.getLabel(id, active),
        "Widget-untagged",
        "default prefs -> untagged",
      );
      eq(d2.getLabel(id, active, ["ko"]), "\uC704\uC82F", "ko prefs");
      eq(d2.getLabel(id, active, ["en", "ko"]), "Widget", "en prefs");
    } finally {
      d2.close();
    }
  });

  try {
    rmSync(tmp, { recursive: true, force: true });
  } catch {
    /* ignore */
  }
  console.log(`ttlRobot tests: ${passed} passed, ${failed} failed`);
  process.exit(failed > 0 ? 1 : 0);
}

run().catch((e) => {
  console.error(e);
  process.exit(1);
});
