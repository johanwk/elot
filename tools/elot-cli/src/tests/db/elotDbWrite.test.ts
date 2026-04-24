// src/tests/db/elotDbWrite.test.ts
//
// Step 2.2.2: writer tests for ElotDb.  Mirrors test/elot-db-crud-test.el
// as closely as possible, plus save()/reopen round-trip coverage.
//
// Run with:  npx tsx src/tests/db/elotDbWrite.test.ts

import { mkdtempSync, rmSync, existsSync } from "fs";
import { tmpdir } from "os";
import { join } from "path";
import {
  ElotDb,
  ActiveSource,
  EntityTriple,
} from "../../db/sqljs.js";

let passed = 0;
let failed = 0;

function check(name: string, cond: boolean, detail?: string): void {
  if (cond) passed++;
  else {
    failed++;
    console.error(`FAIL: ${name}${detail ? " -- " + detail : ""}`);
  }
}

function eq<T>(name: string, actual: T, expected: T): void {
  check(
    name,
    JSON.stringify(actual) === JSON.stringify(expected),
    `expected ${JSON.stringify(expected)}, got ${JSON.stringify(actual)}`,
  );
}

function tmp(): { path: string; cleanup: () => void } {
  const dir = mkdtempSync(join(tmpdir(), "elot-db-write-"));
  const path = join(dir, "elot.sqlite");
  return { path, cleanup: () => rmSync(dir, { recursive: true, force: true }) };
}

async function main() {
  const S1: ActiveSource = { source: "s1", dataSource: "" };
  const A: ActiveSource = { source: "A", dataSource: "" };
  const B: ActiveSource = { source: "B", dataSource: "" };

  // ── test_elot_db_update_source_roundtrip ─────────────────────
  {
    const t = tmp();
    try {
      const db = await ElotDb.open(t.path);
      const data: EntityTriple[] = [
        {
          id: "e1",
          label: "Entity One",
          attrs: [
            ["rdf:type", "owl:Class"],
            ["skos:definition", "the one"],
          ],
        },
        { id: "e2", label: "Entity Two", attrs: [["rdf:type", "owl:Class"]] },
      ];
      const n = db.updateSource("s1", "", "org", data, 123.0);
      eq("update_source returns count", n, 2);
      check("source exists", db.sourceExistsP("s1"));
      eq("entity count", db.sourceEntityCount("s1"), 2);
      eq("get label e1", db.getLabel("e1", [S1]), "Entity One");
      eq(
        "get attr skos:definition",
        db.getAttr("e1", "skos:definition", [S1]),
        "the one",
      );
      // Save + reopen + re-read.
      db.save(t.path);
      db.close();
      const db2 = await ElotDb.open(t.path);
      eq("reopen: count", db2.sourceEntityCount("s1"), 2);
      eq(
        "reopen: get label",
        db2.getLabel("e2", [S1]),
        "Entity Two",
      );
      db2.close();
    } finally {
      t.cleanup();
    }
  }

  // ── replaces on re-insert ─────────────────────────────────────
  {
    const t = tmp();
    try {
      const db = await ElotDb.open(t.path);
      db.updateSource("s1", "", "org", [
        { id: "e1", label: "Old One", attrs: [["rdf:type", "owl:Class"]] },
        { id: "e2", label: "Old Two", attrs: [["rdf:type", "owl:Class"]] },
      ]);
      db.updateSource("s1", "", "org", [
        { id: "e3", label: "New Three", attrs: [["rdf:type", "owl:Thing"]] },
      ]);
      eq("after replace: count", db.sourceEntityCount("s1"), 1);
      eq("e1 gone", db.getLabel("e1", [S1]), null);
      eq("e3 present", db.getLabel("e3", [S1]), "New Three");
      db.close();
    } finally {
      t.cleanup();
    }
  }

  // ── kind written; :kind not leaked to attributes ──────────────
  {
    const t = tmp();
    try {
      const db = await ElotDb.open(t.path);
      db.updateSource("s1", "", "org", [
        {
          id: "http://example.org/x",
          label: "X",
          kind: "uri",
          attrs: [["rdf:type", "owl:Class"]],
        },
        { id: "ex:y", label: "Y", kind: "curie" },
      ]);
      const kinds = db.raw
        .exec("SELECT id, kind FROM entities ORDER BY id")[0]
        ?.values.map((r) => [r[0], r[1]]);
      eq("kinds written", kinds, [
        ["ex:y", "curie"],
        ["http://example.org/x", "uri"],
      ]);
      const attrCount = db.raw.exec(
        "SELECT COUNT(*) FROM attributes WHERE source='s1'",
      )[0]?.values[0][0];
      eq("attribute count (kind not leaked)", attrCount, 1);
      db.close();
    } finally {
      t.cleanup();
    }
  }

  // ── (source, data_source) pairs independent ──────────────────
  {
    const t = tmp();
    try {
      const db = await ElotDb.open(t.path);
      db.updateSource("q.rq", "a.ttl", "rq", [
        { id: "e1", label: "A", attrs: [["p", "v"]] },
      ]);
      db.updateSource("q.rq", "b.ttl", "rq", [
        { id: "e2", label: "B", attrs: [["p", "v"]] },
      ]);
      check("a.ttl exists", db.sourceExistsP("q.rq", "a.ttl"));
      check("b.ttl exists", db.sourceExistsP("q.rq", "b.ttl"));
      eq("two sources", db.listSources().length, 2);
      db.close();
    } finally {
      t.cleanup();
    }
  }

  // ── removeSource cascades ────────────────────────────────────
  {
    const t = tmp();
    try {
      const db = await ElotDb.open(t.path);
      db.updateSource("s1", "", "org", [
        { id: "e1", label: "E1", attrs: [["rdf:type", "owl:Class"]] },
      ]);
      db.addPrefix("s1", "", "ex", "http://example.org/");
      check("remove: ok", db.removeSource("s1"));
      check("after remove: no source", !db.sourceExistsP("s1"));
      const counts = [
        db.raw.exec("SELECT COUNT(*) FROM entities")[0]?.values[0][0],
        db.raw.exec("SELECT COUNT(*) FROM attributes")[0]?.values[0][0],
        db.raw.exec("SELECT COUNT(*) FROM prefixes")[0]?.values[0][0],
      ];
      eq("cascade wipes all children", counts, [0, 0, 0]);
      check("remove missing returns false", !db.removeSource("nope"));
      db.close();
    } finally {
      t.cleanup();
    }
  }

  // ── needs-update-p ───────────────────────────────────────────
  {
    const t = tmp();
    try {
      const db = await ElotDb.open(t.path);
      check("unknown source needs update", db.sourceNeedsUpdateP("f", 1));
      db.updateSource("f", "", "csv", [{ id: "e1", label: "E1" }], 1e12);
      check(
        "registered with future mtime: no update",
        !db.sourceNeedsUpdateP("f", 1),
      );
      db.updateSource("f", "", "csv", [{ id: "e1", label: "E1" }], 0.0);
      check(
        "registered with past mtime: stale",
        db.sourceNeedsUpdateP("f", 100),
      );
      db.close();
    } finally {
      t.cleanup();
    }
  }

  // ── getLabel priority ────────────────────────────────────────
  {
    const t = tmp();
    try {
      const db = await ElotDb.open(t.path);
      db.updateSource("A", "", "csv", [{ id: "e1", label: "Label from A" }]);
      db.updateSource("B", "", "csv", [{ id: "e1", label: "Label from B" }]);
      eq("A before B", db.getLabel("e1", [A, B]), "Label from A");
      eq("B before A", db.getLabel("e1", [B, A]), "Label from B");
      eq("inactive -> null", db.getLabel("e1", [{ source: "C" }]), null);
      db.close();
    } finally {
      t.cleanup();
    }
  }

  // ── multi-row rdfs:label coalescing + language pick ──────────
  {
    const t = tmp();
    try {
      const db = await ElotDb.open(t.path);
      // Simulate ROBOT TTL multi-row output: two rows for same id.
      db.updateSource(
        "T",
        "",
        "ttl",
        [
          {
            id: "ex:x",
            label: null,
            attrs: [["rdfs:label", { value: "Hello", lang: "en" }]],
          },
          {
            id: "ex:x",
            label: null,
            attrs: [["rdfs:label", { value: "Hallo", lang: "de" }]],
          },
        ],
        0.0,
        ["en"],
      );
      eq(
        "entities.label picked by lang prefs",
        db.getLabel("ex:x", [{ source: "T" }], ["en"]),
        "Hello",
      );
      eq(
        "de wins when prefs=de",
        db.getLabel("ex:x", [{ source: "T" }], ["de"]),
        "Hallo",
      );
      const variants = db.labelVariants("ex:x", [{ source: "T" }]);
      eq("two label variants", variants?.length, 2);
      db.close();
    } finally {
      t.cleanup();
    }
  }

  // ── addPrefix / removePrefix ─────────────────────────────────
  {
    const t = tmp();
    try {
      const db = await ElotDb.open(t.path);
      db.updateSource("s1", "", "csv", [{ id: "e1", label: "E" }]);
      db.addPrefix("s1", "", "ex", "http://example.org/");
      eq(
        "expandCurie via source prefix",
        db.expandCurie("ex:foo", [S1]),
        "http://example.org/foo",
      );
      check("removePrefix removes", db.removePrefix("s1", "", "ex"));
      eq("no expansion after remove", db.expandCurie("ex:foo", [S1]), null);
      check(
        "removePrefix missing returns false",
        !db.removePrefix("s1", "", "ex"),
      );
      db.close();
    } finally {
      t.cleanup();
    }
  }

  // ── save() persists mutations ────────────────────────────────
  {
    const t = tmp();
    try {
      let db = await ElotDb.open(t.path);
      db.updateSource("s1", "", "csv", [{ id: "e1", label: "Saved" }]);
      db.addPrefix("s1", "", "ex", "http://example.org/");
      db.save(t.path);
      db.close();
      check("file written", existsSync(t.path));
      db = await ElotDb.open(t.path);
      eq(
        "label persisted",
        db.getLabel("e1", [S1]),
        "Saved",
      );
      eq(
        "prefix persisted",
        db.expandCurie("ex:x", [S1]),
        "http://example.org/x",
      );
      db.close();
    } finally {
      t.cleanup();
    }
  }

  // ── rollback on failure ──────────────────────────────────────
  {
    const t = tmp();
    try {
      const db = await ElotDb.open(t.path);
      db.updateSource("s1", "", "csv", [{ id: "e1", label: "keep" }]);
      try {
        // Force mid-transaction failure via an INSERT that breaks
        // the sources UNIQUE by inserting through raw while the
        // transaction is open.  We trigger this by passing a data
        // array whose 2nd element throws in the loop via a Proxy
        // id getter.
        const bad = [
          { id: "x", label: "ok" },
          new Proxy({} as EntityTriple, {
            get(_t, prop) {
              if (prop === "id") throw new Error("boom");
              return undefined;
            },
          }),
        ];
        db.updateSource("s2", "", "csv", bad as EntityTriple[]);
      } catch {
        /* expected */
      }
      // After rollback, s1 unaffected and s2 absent.
      eq("s1 survives rollback", db.getLabel("e1", [S1]), "keep");
      check("s2 not created (rollback)", !db.sourceExistsP("s2"));
      db.close();
    } finally {
      t.cleanup();
    }
  }

  console.log(
    `elotDbWrite tests: ${passed} passed, ${failed} failed`,
  );
  if (failed > 0) process.exit(1);
}

main().catch((e) => {
  console.error(e);
  process.exit(1);
});
