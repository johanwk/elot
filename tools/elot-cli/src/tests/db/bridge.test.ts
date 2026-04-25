// src/tests/db/bridge.test.ts
//
// Step 2.3.1: ElotDbBridge -- lazy open, watch, debounced reload.

import { mkdtempSync, writeFileSync, unlinkSync, copyFileSync } from "fs";
import { tmpdir } from "os";
import { join } from "path";
import { ElotDbBridge } from "../../db/bridge.js";
import { ElotDb } from "../../db/sqljs.js";

let passed = 0;
let failed = 0;

async function test(name: string, fn: () => void | Promise<void>): Promise<void> {
  try {
    await fn();
    passed++;
  } catch (err) {
    failed++;
    console.error(`FAIL ${name}:`, err instanceof Error ? err.stack ?? err.message : err);
  }
}

function eq<T>(a: T, e: T, msg = ""): void {
  if (a !== e) throw new Error(`${msg}\n  got:      ${String(a)}\n  expected: ${String(e)}`);
}

function tru(v: unknown, msg = ""): void {
  if (!v) throw new Error(msg);
}

async function sleep(ms: number): Promise<void> {
  return new Promise((r) => setTimeout(r, ms));
}

async function makeSeededDb(path: string, sourceName: string): Promise<void> {
  const db = await ElotDb.open(null);
  db.updateSource(
    sourceName,
    "",
    "csv",
    [{ id: "ex:Widget", label: "Widget", attrs: [["rdfs:label", "Widget"]] }],
    0,
  );
  db.save(path);
  db.close();
}

async function run(): Promise<void> {
  const dir = mkdtempSync(join(tmpdir(), "elot-bridge-"));
  const dbPath = join(dir, "elot.sqlite");

  // ── lazy open ─────────────────────────────────────────────
  await test("get() returns null when path does not exist", async () => {
    const b = new ElotDbBridge({ resolvePath: () => dbPath });
    const db = await b.get();
    eq(db, null, "no file -> null");
    eq(b.isOpen, false, "isOpen false");
    b.dispose();
  });

  await test("get() opens the DB when the file exists", async () => {
    await makeSeededDb(dbPath, "labels");
    const b = new ElotDbBridge({ resolvePath: () => dbPath });
    const db = await b.get();
    tru(db !== null, "db opened");
    eq(b.isOpen, true, "isOpen true");
    eq(db!.listSources().length, 1, "one source");
    eq(db!.listSources()[0].source, "labels", "source name");
    b.dispose();
  });

  await test("get() is idempotent (same handle on repeated calls)", async () => {
    const b = new ElotDbBridge({ resolvePath: () => dbPath });
    const a = await b.get();
    const c = await b.get();
    eq(a, c, "same handle");
    b.dispose();
  });

  // ── path change ──────────────────────────────────────────
  await test("reload() picks up a new path from resolvePath", async () => {
    const dbPath2 = join(dir, "elot2.sqlite");
    await makeSeededDb(dbPath2, "second");
    let which = dbPath;
    const b = new ElotDbBridge({ resolvePath: () => which });
    let db = await b.get();
    eq(db!.listSources()[0].source, "labels");
    which = dbPath2;
    db = await b.reload();
    eq(db!.listSources()[0].source, "second", "reloaded with new path");
    b.dispose();
  });

  // ── listener ──────────────────────────────────────────────
  await test("onDidChange fires on (re)load and on missing-file load", async () => {
    let n = 0;
    const b = new ElotDbBridge({ resolvePath: () => dbPath });
    const off = b.onDidChange(() => {
      n++;
    });
    await b.get();
    eq(n, 1, "fires on first load");
    await b.reload();
    eq(n, 2, "fires on explicit reload");
    off();
    await b.reload();
    eq(n, 2, "no fire after disposer");
    b.dispose();
  });

  // ── watcher / debounce ──────────────────────────────────
  await test("file write triggers debounced reload", async () => {
    const watched = join(dir, "elot-watch.sqlite");
    await makeSeededDb(watched, "v1");
    let n = 0;
    const b = new ElotDbBridge({
      resolvePath: () => watched,
      debounceMs: 30,
    });
    b.onDidChange(() => {
      n++;
    });
    const db = await b.get();
    eq(db!.listSources()[0].source, "v1");
    eq(n, 1);
    // Replace contents.
    const tmp = join(dir, "v2.sqlite");
    await makeSeededDb(tmp, "v2");
    copyFileSync(tmp, watched);
    // Wait for debounce + reload.
    for (let i = 0; i < 50 && n < 2; i++) await sleep(20);
    tru(n >= 2, `expected reload notification (n=${n})`);
    const after = await b.get();
    eq(after!.listSources()[0].source, "v2", "watcher reloaded contents");
    b.dispose();
  });

  // ── dispose hygiene ──────────────────────────────────────
  await test("after dispose, get() returns null and listeners do not fire", async () => {
    const p = join(dir, "elot-dispose.sqlite");
    await makeSeededDb(p, "x");
    const b = new ElotDbBridge({ resolvePath: () => p });
    let n = 0;
    b.onDidChange(() => {
      n++;
    });
    await b.get();
    eq(n, 1);
    b.dispose();
    const after = await b.get();
    eq(after, null, "null after dispose");
    eq(n, 1, "no further notifications");
  });

  // ── error tolerance ──────────────────────────────────────
  await test("corrupt DB file -> onError, get() returns null, no throw", async () => {
    const corrupt = join(dir, "corrupt.sqlite");
    writeFileSync(corrupt, "this is not a sqlite database");
    const errors: unknown[] = [];
    const b = new ElotDbBridge({
      resolvePath: () => corrupt,
      onError: (e) => errors.push(e),
    });
    const db = await b.get();
    eq(db, null, "null on bad file");
    tru(errors.length >= 1, "onError called");
    b.dispose();
    unlinkSync(corrupt);
  });
}

run().then(() => {
  console.log(`bridge tests: ${passed} passed, ${failed} failed`);
  if (failed > 0) process.exit(1);
});
