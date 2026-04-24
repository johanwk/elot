// src/tests/db/cliE2e.test.ts
//
// Step 2.2.5 - end-to-end black-box tests for `elot-cli db`.
// Spawns dbCli.ts via tsx as a subprocess and exercises the full
// commander surface (init / register / list / lookup / attr /
// remove / refresh) plus the new --format=json|tsv|table polish.
//
// Run with:  npx tsx src/tests/db/cliE2e.test.ts

import { spawnSync, SpawnSyncReturns } from "child_process";
import { mkdtempSync, rmSync, writeFileSync } from "fs";
import { tmpdir } from "os";
import { join, resolve } from "path";

const CLI = resolve(__dirname, "..", "..", "dbCli.ts");
const NODE = process.execPath;
// Use the same tsx loader the npm scripts use.
const TSX_ARGS = ["--import", "tsx"];

let passed = 0;
let failed = 0;

function t(name: string, fn: () => void | Promise<void>): Promise<void> {
  return Promise.resolve(fn()).then(
    () => {
      passed++;
    },
    (e) => {
      failed++;
      console.error(`FAIL ${name}:`, e instanceof Error ? e.message : e);
    },
  );
}

function eq<T>(actual: T, expected: T, msg: string): void {
  const a = JSON.stringify(actual);
  const e = JSON.stringify(expected);
  if (a !== e) {
    throw new Error(`${msg}\n  got:      ${a}\n  expected: ${e}`);
  }
}

function assert(cond: unknown, msg: string): asserts cond {
  if (!cond) throw new Error(msg);
}

interface CliResult {
  status: number;
  stdout: string;
  stderr: string;
}

function runCli(args: string[]): CliResult {
  const r: SpawnSyncReturns<string> = spawnSync(
    NODE,
    [...TSX_ARGS, CLI, ...args],
    { encoding: "utf-8" },
  );
  return {
    status: r.status ?? -1,
    stdout: r.stdout ?? "",
    stderr: r.stderr ?? "",
  };
}

const work = mkdtempSync(join(tmpdir(), "elot-cli-e2e-"));
const dbPath = join(work, "elot.sqlite");

async function run(): Promise<void> {
  // ─── init: fresh DB ───────────────────────────────────────────
  await t("init creates a new DB", () => {
    const r = runCli(["db", "init", "--db", dbPath]);
    eq(r.status, 0, `init exit: ${r.stderr}`);
    assert(r.stdout.startsWith("created:"), `expected 'created:' in stdout, got: ${r.stdout}`);
  });

  await t("init on existing DB prints 'ok'", () => {
    const r = runCli(["db", "init", "--db", dbPath]);
    eq(r.status, 0, `init re-run exit: ${r.stderr}`);
    assert(r.stdout.startsWith("ok:"), `expected 'ok:' got: ${r.stdout}`);
  });

  // ─── register CSV ─────────────────────────────────────────────
  const csvPath = join(work, "labels.csv");
  writeFileSync(
    csvPath,
    "id,label,lang\nex:widget,Widget,en\nex:widget,Dings,de\nex:gadget,Gadget,en\n",
    "utf-8",
  );

  await t("register CSV reports entity count", () => {
    const r = runCli([
      "db",
      "register",
      csvPath,
      "--db",
      dbPath,
      "--source",
      "demo",
      "--type",
      "csv",
    ]);
    eq(r.status, 0, `register exit: ${r.stderr}`);
    assert(
      /registered: demo \(2 entities, type=csv\)/.test(r.stdout),
      `unexpected register output: ${r.stdout}`,
    );
  });

  await t("register: missing file -> exit 2 with friendly error", () => {
    const r = runCli([
      "db",
      "register",
      join(work, "no-such.csv"),
      "--db",
      dbPath,
      "--type",
      "csv",
    ]);
    eq(r.status, 2, "expected exit 2");
    assert(/file not found/.test(r.stderr), `stderr: ${r.stderr}`);
  });

  // ─── list ─────────────────────────────────────────────────────
  await t("list (tsv default) shows demo source", () => {
    const r = runCli(["db", "list", "--db", dbPath]);
    eq(r.status, 0, "list exit");
    const lines = r.stdout.trim().split(/\r?\n/);
    eq(lines.length, 1, "exactly one source row");
    const cols = lines[0].split("\t");
    eq(cols[0], "demo", "source name");
    eq(cols[2], "csv", "type column");
  });

  await t("list --format=json yields parseable array", () => {
    const r = runCli(["db", "list", "--db", dbPath, "--format", "json"]);
    eq(r.status, 0, "list json exit");
    const arr = JSON.parse(r.stdout);
    assert(Array.isArray(arr), "json output is array");
    eq(arr.length, 1, "one source");
    eq(arr[0].source, "demo", "source name");
    eq(arr[0].type, "csv", "type");
  });

  await t("list --format=table renders header + dash row", () => {
    const r = runCli(["db", "list", "--db", dbPath, "--format", "table"]);
    eq(r.status, 0, "list table exit");
    const lines = r.stdout.trim().split(/\r?\n/);
    assert(lines.length >= 3, "header + dashes + at least one row");
    assert(/^source\b/.test(lines[0]), "header starts with 'source'");
    assert(/^-+/.test(lines[1]), "second line is dashes");
    assert(/\bdemo\b/.test(lines[2]), "data row has 'demo'");
  });

  await t("list --prefixes (with no prefixes) prints nothing in tsv", () => {
    const r = runCli(["db", "list", "--db", dbPath, "--prefixes"]);
    eq(r.status, 0, "list prefixes exit");
    eq(r.stdout, "", "no prefix rows");
  });

  // ─── lookup ────────────────────────────────────────────────────
  await t("lookup Widget -> ex:widget (tsv)", () => {
    const r = runCli(["db", "lookup", "Widget", "--db", dbPath]);
    eq(r.status, 0, "lookup exit");
    eq(r.stdout.trim(), "ex:widget", "id");
  });

  await t("lookup --format=json carries label and ids", () => {
    const r = runCli([
      "db",
      "lookup",
      "Widget",
      "--db",
      dbPath,
      "--format",
      "json",
    ]);
    eq(r.status, 0, "lookup json exit");
    const obj = JSON.parse(r.stdout);
    eq(obj.label, "Widget", "label echoed");
    eq(obj.ids, ["ex:widget"], "ids array");
  });

  await t("lookup miss -> exit 1, empty stdout (tsv)", () => {
    const r = runCli(["db", "lookup", "Nonesuch", "--db", dbPath]);
    eq(r.status, 1, "miss exit code");
    eq(r.stdout, "", "no output on miss");
  });

  await t("lookup miss --format=json -> exit 1, ids: []", () => {
    const r = runCli([
      "db",
      "lookup",
      "Nonesuch",
      "--db",
      dbPath,
      "--format",
      "json",
    ]);
    eq(r.status, 1, "miss json exit code");
    const obj = JSON.parse(r.stdout);
    eq(obj.ids, [], "empty ids array");
  });

  // ─── attr ──────────────────────────────────────────────────────
  await t("attr ex:widget rdfs:label -> Widget (en wins prefs)", () => {
    const r = runCli([
      "db",
      "attr",
      "ex:widget",
      "rdfs:label",
      "--db",
      dbPath,
    ]);
    eq(r.status, 0, "attr exit");
    eq(r.stdout.trim(), "Widget", "value");
  });

  await t("attr --format=json includes id+prop+value", () => {
    const r = runCli([
      "db",
      "attr",
      "ex:widget",
      "rdfs:label",
      "--db",
      dbPath,
      "--format",
      "json",
    ]);
    eq(r.status, 0, "attr json exit");
    const obj = JSON.parse(r.stdout);
    eq(obj.id, "ex:widget", "id");
    eq(obj.prop, "rdfs:label", "prop");
    eq(obj.value, "Widget", "value");
  });

  await t("attr (no prop) lists all attrs in json", () => {
    const r = runCli([
      "db",
      "attr",
      "ex:widget",
      "--db",
      dbPath,
      "--format",
      "json",
    ]);
    eq(r.status, 0, "all-attrs json exit");
    const obj = JSON.parse(r.stdout);
    eq(obj.id, "ex:widget", "id");
    assert(Array.isArray(obj.attrs), "attrs is array");
    const labelEntry = obj.attrs.find(
      (a: { prop: string }) => a.prop === "rdfs:label",
    );
    assert(labelEntry, "rdfs:label present");
    eq(labelEntry.value, "Widget", "rdfs:label value");
    assert(obj.origin && obj.origin.source === "demo", "origin populated");
  });

  await t("attr miss -> exit 1", () => {
    const r = runCli([
      "db",
      "attr",
      "ex:nonesuch",
      "rdfs:label",
      "--db",
      dbPath,
    ]);
    eq(r.status, 1, "miss exit");
  });

  // ─── refresh ──────────────────────────────────────────────────
  await t("refresh re-reads CSV after edit", () => {
    writeFileSync(
      csvPath,
      "id,label,lang\nex:widget,Widget,en\nex:gadget,Gadget,en\nex:thingy,Thingy,en\n",
      "utf-8",
    );
    const r = runCli([
      "db",
      "refresh",
      "demo",
      "--file",
      csvPath,
      "--db",
      dbPath,
      "--type",
      "csv",
    ]);
    eq(r.status, 0, `refresh exit: ${r.stderr}`);
    assert(/refreshed: demo \(3 entities\)/.test(r.stdout), `output: ${r.stdout}`);
    // And the new label is queryable.
    const r2 = runCli(["db", "lookup", "Thingy", "--db", dbPath]);
    eq(r2.status, 0, "lookup new label exit");
    eq(r2.stdout.trim(), "ex:thingy", "new id resolved");
  });

  // ─── remove ───────────────────────────────────────────────────
  await t("remove demo source -> list empty", () => {
    const r = runCli(["db", "remove", "demo", "--db", dbPath]);
    eq(r.status, 0, "remove exit");
    assert(/removed: demo/.test(r.stdout), `removed output: ${r.stdout}`);
    const r2 = runCli(["db", "list", "--db", dbPath, "--format", "json"]);
    eq(r2.status, 0, "list after remove exit");
    eq(JSON.parse(r2.stdout), [], "no sources left");
  });

  await t("remove unknown source -> exit 1", () => {
    const r = runCli(["db", "remove", "ghost", "--db", dbPath]);
    eq(r.status, 1, "remove unknown exit");
    assert(/not-found/.test(r.stdout), `output: ${r.stdout}`);
  });

  // ─── --format validation ──────────────────────────────────────
  await t("--format bogus -> commander error (exit != 0)", () => {
    const r = runCli(["db", "list", "--db", dbPath, "--format", "yaml"]);
    assert(r.status !== 0, `expected non-zero, got ${r.status}`);
    assert(
      /must be one of/.test(r.stderr) || /invalid/i.test(r.stderr),
      `stderr should mention validation: ${r.stderr}`,
    );
  });
}

run().then(() => {
  rmSync(work, { recursive: true, force: true });
  console.log(`cliE2e tests: ${passed} passed, ${failed} failed`);
  if (failed > 0) process.exit(1);
});
