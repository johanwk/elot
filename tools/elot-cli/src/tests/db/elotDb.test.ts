// src/tests/db/elotDb.test.ts
//
// Step 2.2.1: read-path tests for the ElotDb class.  The writer
// lands in 2.2.2; these tests seed the DB directly via raw SQL
// (mirroring what elot-db-update-source would emit) so that the
// read surface can be exercised in isolation.
//
// Run with:  npx tsx src/tests/db/elotDb.test.ts

import { mkdtempSync, rmSync, writeFileSync, existsSync } from "fs";
import { tmpdir } from "os";
import { join } from "path";
import initSqlJs from "sql.js";
import { ElotDb, locateSchemaSql } from "../../db/sqljs.js";
import { readFileSync } from "fs";

let passed = 0;
let failed = 0;

function assertEqual<T>(actual: T, expected: T, msg: string): void {
  const ok = JSON.stringify(actual) === JSON.stringify(expected);
  if (ok) passed++;
  else {
    failed++;
    console.error(`FAIL: ${msg}`);
    console.error(`  expected: ${JSON.stringify(expected)}`);
    console.error(`  actual:   ${JSON.stringify(actual)}`);
  }
}

async function withFreshDb(
  seedSql: string[],
): Promise<{ db: ElotDb; path: string; cleanup: () => void }> {
  const dir = mkdtempSync(join(tmpdir(), "elot-db-"));
  const path = join(dir, "elot.sqlite");
  // Create fresh DB via ElotDb (applies schema + seeds version).
  let db = await ElotDb.open(null);
  for (const stmt of seedSql) db.raw.run(stmt);
  // Persist to a real file, then reopen via ElotDb.open(path) to
  // exercise the v3-assertion code path.
  const bytes = db.raw.export();
  writeFileSync(path, Buffer.from(bytes));
  db.close();
  db = await ElotDb.open(path);
  return {
    db,
    path,
    cleanup: () => {
      db.close();
      rmSync(dir, { recursive: true, force: true });
    },
  };
}

function sq(s: string): string {
  return s.replace(/'/g, "''");
}

function seedSource(
  source: string,
  dataSource: string,
  type: string,
): string[] {
  return [
    `INSERT INTO sources (source, data_source, type, last_modified, last_updated)
     VALUES ('${sq(source)}', '${sq(dataSource)}', '${sq(type)}', 1.0, 1.0)`,
  ];
}

function seedEntity(
  id: string,
  label: string | null,
  source: string,
  dataSource: string,
  kind = "unknown",
): string {
  const lbl = label === null ? "NULL" : `'${sq(label)}'`;
  return `INSERT INTO entities (id, label, source, data_source, kind)
          VALUES ('${sq(id)}', ${lbl}, '${sq(source)}', '${sq(dataSource)}', '${sq(kind)}')`;
}

function seedAttr(
  id: string,
  source: string,
  dataSource: string,
  prop: string,
  value: string,
  lang = "",
): string {
  return `INSERT INTO attributes (id, source, data_source, prop, value, lang)
          VALUES ('${sq(id)}', '${sq(source)}', '${sq(dataSource)}',
                  '${sq(prop)}', '${sq(value)}', '${sq(lang)}')`;
}

function seedPrefix(
  source: string,
  dataSource: string,
  prefix: string,
  expansion: string,
): string {
  return `INSERT INTO prefixes (source, data_source, prefix, expansion)
          VALUES ('${sq(source)}', '${sq(dataSource)}',
                  '${sq(prefix)}', '${sq(expansion)}')`;
}

async function run(): Promise<void> {
  // ── schema.sql locates ─────────────────────────────────────────
  {
    const p = locateSchemaSql();
    assertEqual(existsSync(p), true, "schema.sql locatable");
    const content = readFileSync(p, "utf-8");
    assertEqual(
      /CREATE TABLE IF NOT EXISTS schema_version/.test(content),
      true,
      "schema.sql looks like the v3 DDL",
    );
  }

  // ── fresh DB seeds schema_version = 3 ──────────────────────────
  {
    const db = await ElotDb.open(null);
    const r = db.raw.exec("SELECT version FROM schema_version");
    assertEqual(r[0].values[0][0], 3, "fresh DB has schema_version = 3");
    db.close();
  }

  // ── refuses to open pre-v3 DBs ─────────────────────────────────
  {
    const SQL = await initSqlJs();
    const raw = new SQL.Database();
    // v2-style: version = 2, no global_prefixes / lang column.
    raw.run(`CREATE TABLE schema_version (version INTEGER PRIMARY KEY)`);
    raw.run(`INSERT INTO schema_version (version) VALUES (2)`);
    const dir = mkdtempSync(join(tmpdir(), "elot-db-v2-"));
    const path = join(dir, "v2.sqlite");
    writeFileSync(path, Buffer.from(raw.export()));
    raw.close();
    let threw = false;
    try {
      await ElotDb.open(path);
    } catch (e) {
      threw = /schema version mismatch/.test((e as Error).message);
    }
    assertEqual(threw, true, "pre-v3 DB refused with mismatch message");
    rmSync(dir, { recursive: true, force: true });
  }

  // ── list/sources + existsP + entityCount ──────────────────────
  {
    const { db, cleanup } = await withFreshDb([
      ...seedSource("A", "", "csv"),
      seedEntity("e1", "E one", "A", "", "unknown"),
      seedEntity("e2", "E two", "A", "", "unknown"),
    ]);
    try {
      const ss = db.listSources();
      assertEqual(ss.length, 1, "one source listed");
      assertEqual(ss[0].source, "A", "source name");
      assertEqual(ss[0].type, "csv", "source type");
      assertEqual(db.sourceExistsP("A"), true, "A exists");
      assertEqual(db.sourceExistsP("B"), false, "B absent");
      assertEqual(db.sourceEntityCount("A"), 2, "2 entities in A");
      assertEqual(
        db.sourceNeedsUpdateP("A", 2.0),
        true,
        "newer mtime -> needs update",
      );
      assertEqual(
        db.sourceNeedsUpdateP("A", 0.5),
        false,
        "older mtime -> no update",
      );
      assertEqual(
        db.sourceNeedsUpdateP("B", 0.0),
        true,
        "unknown source -> needs update",
      );
    } finally {
      cleanup();
    }
  }

  // ── getLabel: rdfs:label rows beat entities.label, prefs apply ─
  {
    const { db, cleanup } = await withFreshDb([
      ...seedSource("A", "", "csv"),
      seedEntity("e1", "default-label", "A", ""),
      seedAttr("e1", "A", "", "rdfs:label", "English label", "en"),
      seedAttr("e1", "A", "", "rdfs:label", "Korean label", "ko"),
      seedEntity("e2", "plain-label", "A", ""),
    ]);
    try {
      const act = [{ source: "A" }];
      assertEqual(
        db.getLabel("e1", act),
        "English label",
        "default prefs pick en rdfs:label",
      );
      assertEqual(
        db.getLabel("e1", act, ["ko"]),
        "Korean label",
        "ko pref picks ko",
      );
      assertEqual(
        db.getLabel("e2", act),
        "plain-label",
        "fallback to entities.label",
      );
      assertEqual(db.getLabel("missing", act), null, "missing id -> null");
    } finally {
      cleanup();
    }
  }

  // ── getAttr + getAllAttrs ─────────────────────────────────────
  {
    const { db, cleanup } = await withFreshDb([
      ...seedSource("A", "", "csv"),
      seedEntity("e1", "E one", "A", ""),
      seedAttr("e1", "A", "", "rdfs:label", "E one", "en"),
      seedAttr("e1", "A", "", "rdfs:label", "E uno", "es"),
      seedAttr("e1", "A", "", "skos:definition", "English", "en"),
      seedAttr("e1", "A", "", "skos:definition", "Korean", "ko"),
    ]);
    try {
      const act = [{ source: "A" }];
      assertEqual(
        db.getAttr("e1", "skos:definition", act),
        "English",
        "default prefs -> en",
      );
      assertEqual(
        db.getAttr("e1", "skos:definition", act, ["ko"]),
        "Korean",
        "ko pref -> ko",
      );
      const all = db.getAllAttrs("e1", act);
      assertEqual(all !== null, true, "getAllAttrs returns non-null");
      assertEqual(all!.sourceOrigin.source, "A", "source origin");
      // entries contains the picked (prop, value) pairs.
      const m = new Map(all!.entries);
      assertEqual(m.get("rdfs:label"), "E one", "picked en label");
      assertEqual(m.get("skos:definition"), "English", "picked en def");
      const allKo = db.getAllAttrs("e1", act, ["ko", "en"]);
      const mKo = new Map(allKo!.entries);
      assertEqual(mKo.get("skos:definition"), "Korean", "ko -> Korean def");
      // No Korean rdfs:label -> alphabetical fallback picks en (en<es).
      assertEqual(mKo.get("rdfs:label"), "E one", "en<es alphabetical");
    } finally {
      cleanup();
    }
  }

  // ── prefixes + expand/contract ────────────────────────────────
  {
    const { db, cleanup } = await withFreshDb([
      ...seedSource("A", "", "ttl"),
      seedPrefix("A", "", "ex", "http://example.org/"),
      seedPrefix("A", "", "long", "http://example.org/long/"),
      `INSERT OR REPLACE INTO global_prefixes (prefix, expansion)
         VALUES ('rdfs', 'http://www.w3.org/2000/01/rdf-schema#')`,
    ]);
    try {
      const act = [{ source: "A" }];
      assertEqual(
        db.expandCurie("ex:Foo", act),
        "http://example.org/Foo",
        "expand ex:",
      );
      assertEqual(
        db.expandCurie("rdfs:label", act),
        "http://www.w3.org/2000/01/rdf-schema#label",
        "global expansion",
      );
      assertEqual(db.expandCurie("nope:x", act), null, "unknown prefix -> null");
      // contractUri: longer prefix wins first.
      const curies = db.contractUri(
        "http://example.org/long/Thing",
        act,
      );
      assertEqual(curies[0], "long:Thing", "longest prefix first");
      assertEqual(curies.includes("ex:long/Thing"), true, "shorter present");
      assertEqual(
        db
          .listPrefixes("A")
          .map((p) => p.prefix)
          .sort(),
        ["ex", "long"],
        "listPrefixes scoped",
      );
    } finally {
      cleanup();
    }
  }

  // ── getLabelAny: id / CURIE-expand / URI-contract ─────────────
  {
    const { db, cleanup } = await withFreshDb([
      ...seedSource("A", "", "ttl"),
      seedPrefix("A", "", "ex", "http://example.org/"),
      seedEntity("http://example.org/Foo", "FooLabel", "A", ""),
      seedEntity("ex:Bar", "BarLabel", "A", ""),
    ]);
    try {
      const act = [{ source: "A" }];
      // Direct hit on URI id.
      assertEqual(
        db.getLabelAny("http://example.org/Foo", act),
        "FooLabel",
        "direct URI hit",
      );
      // CURIE expands to URI that is stored.
      assertEqual(
        db.getLabelAny("ex:Foo", act),
        "FooLabel",
        "CURIE expands to stored URI",
      );
      // Direct hit on CURIE id.
      assertEqual(
        db.getLabelAny("ex:Bar", act),
        "BarLabel",
        "direct CURIE hit",
      );
      // URI contracts to CURIE id stored.
      assertEqual(
        db.getLabelAny("http://example.org/Bar", act),
        "BarLabel",
        "URI contracts to stored CURIE",
      );
      assertEqual(
        db.getLabelAny("ex:missing", act),
        null,
        "unknown token -> null",
      );
    } finally {
      cleanup();
    }
  }

  // ── allActiveIds (with CURIE augmentation) ────────────────────
  {
    const { db, cleanup } = await withFreshDb([
      ...seedSource("A", "", "ttl"),
      seedPrefix("A", "", "ex", "http://example.org/"),
      seedEntity("http://example.org/Foo", "Foo", "A", ""),
      seedEntity("http://example.org/Bar", "Bar", "A", ""),
    ]);
    try {
      const act = [{ source: "A" }];
      const ids = db.allActiveIds(act, false);
      assertEqual(ids.length, 2, "2 ids without CURIEs");
      const both = db.allActiveIds(act, true);
      assertEqual(both.includes("ex:Foo"), true, "CURIE ex:Foo added");
      assertEqual(both.includes("ex:Bar"), true, "CURIE ex:Bar added");
      assertEqual(
        both.slice(0, 2).every((s) => s.startsWith("http://")),
        true,
        "URIs come first",
      );
    } finally {
      cleanup();
    }
  }

  // ── allActiveLabels / idsForLabel / labelVariants ─────────────
  {
    const { db, cleanup } = await withFreshDb([
      ...seedSource("A", "", "csv"),
      seedEntity("e1", "default-one", "A", ""),
      seedAttr("e1", "A", "", "rdfs:label", "English one", "en"),
      seedAttr("e1", "A", "", "rdfs:label", "Korean one", "ko"),
      seedEntity("e2", "default-two", "A", ""),
      seedAttr("e2", "A", "", "rdfs:label", "English two", "en"),
      seedAttr("e2", "A", "", "rdfs:label", "Korean two", "ko"),
      seedEntity("e3", "entity-three", "A", ""),
      // Two ids sharing a label (Step 1.15 shape).
      seedEntity("e4", "shared", "A", ""),
      seedEntity("e5", "shared", "A", ""),
    ]);
    try {
      const act = [{ source: "A" }];
      const ht = db.allActiveLabels(act);
      assertEqual(ht.get("English one"), ["e1"], "en one");
      assertEqual(ht.get("English two"), ["e2"], "en two");
      assertEqual(ht.get("entity-three"), ["e3"], "fallback entities.label");
      assertEqual(ht.get("Korean one"), undefined, "ko hidden by default prefs");
      assertEqual(
        (ht.get("shared") ?? []).sort(),
        ["e4", "e5"],
        "multi-id label",
      );
      const htKo = db.allActiveLabels(act, ["ko"]);
      assertEqual(htKo.get("Korean one"), ["e1"], "ko prefs -> ko labels");
      assertEqual(htKo.get("English one"), undefined, "en hidden under ko");

      assertEqual(
        (db.idsForLabel("shared", act) ?? []).sort(),
        ["e4", "e5"],
        "idsForLabel multi",
      );
      assertEqual(
        db.idsForLabel("nonexistent", act),
        null,
        "idsForLabel miss",
      );
      const variants = db.labelVariants("e1", act);
      assertEqual(variants?.length, 2, "2 lang variants for e1");
      assertEqual(
        variants?.map((v) => v.lang).sort(),
        ["en", "ko"],
        "variant langs",
      );
      assertEqual(
        db.labelVariants("e3", act),
        null,
        "no rdfs:label rows -> null",
      );
    } finally {
      cleanup();
    }
  }

  // ── multi-source priority: first-source-wins ──────────────────
  {
    const { db, cleanup } = await withFreshDb([
      ...seedSource("HI", "", "csv"),
      ...seedSource("LO", "", "csv"),
      seedEntity("e1", "from-hi", "HI", ""),
      seedEntity("e1", "from-lo", "LO", ""),
      seedEntity("e2", null, "HI", ""), // not in HI but cascade safe
      seedEntity("e2", "from-lo-only", "LO", ""),
    ]);
    try {
      const act = [{ source: "HI" }, { source: "LO" }];
      assertEqual(
        db.getLabel("e1", act),
        "from-hi",
        "HI wins over LO",
      );
      // e2 has no row in HI with non-null label; getLabel returns HI's null.
      // Our impl returns the entities.label column unmodified, which is null.
      // Confirm fallback to LO would not kick in (first-source-wins).
      assertEqual(
        db.getLabel("e2", act),
        null,
        "first-source null -> null (no fallthrough)",
      );
    } finally {
      cleanup();
    }
  }

  console.log(`elotDb tests: ${passed} passed, ${failed} failed`);
  if (failed > 0) process.exit(1);
}

run().catch((e) => {
  console.error(e);
  process.exit(1);
});
