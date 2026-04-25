// src/tests/views/sourceItems.test.ts
//
// Step 2.3.7a: pure tests for the tree-item builders.

import {
  buildDbSourceItems,
  buildActiveSourceItems,
} from "../../views/sourceItems.js";
import type { SourceRow } from "../../db/sqljs.js";
import type { SourceKey } from "../../sourceCommands/reorder.js";

let pass = 0;
let fail = 0;

function t(name: string, fn: () => void): void {
  try {
    fn();
    pass++;
  } catch (e) {
    fail++;
    console.error(`FAIL ${name}: ${(e as Error).message}`);
  }
}

function eq<T>(a: T, b: T, msg = ""): void {
  const aj = JSON.stringify(a);
  const bj = JSON.stringify(b);
  if (aj !== bj) throw new Error(`${msg}\n  got:      ${aj}\n  expected: ${bj}`);
}

function row(s: string, ds = "", type: string | null = "csv", lu: number | null = null): SourceRow {
  return {
    source: s,
    dataSource: ds,
    type,
    lastModified: null,
    lastUpdated: lu,
  };
}

const sk = (s: string, ds = ""): SourceKey => ({ source: s, dataSource: ds });

// ---- buildDbSourceItems ----

t("db: empty input -> empty output", () => {
  eq(buildDbSourceItems([], []), []);
});

t("db: marks active vs inactive via contextValue", () => {
  const items = buildDbSourceItems(
    [row("bfo"), row("ro"), row("fma")],
    [sk("bfo"), sk("ro")],
  );
  eq(items.map((i) => i.contextValue), [
    "elotDbSource.active",
    "elotDbSource.active",
    "elotDbSource.inactive",
  ]);
  eq(items.map((i) => i.isActive), [true, true, false]);
});

t("db: description contains type, count, active marker", () => {
  const counts = new Map<string, number>([["bfo\u0000", 985]]);
  const items = buildDbSourceItems([row("bfo", "", "ttl")], [sk("bfo")], counts);
  eq(items.length, 1);
  if (!items[0].description.includes("ttl"))
    throw new Error(`missing type in description: ${items[0].description}`);
  if (!items[0].description.includes("985 ids"))
    throw new Error(`missing count: ${items[0].description}`);
  if (!items[0].description.startsWith("*"))
    throw new Error(`active should lead with *: ${items[0].description}`);
});

t("db: tooltip includes data_source path", () => {
  const items = buildDbSourceItems(
    [row("ro", "/path/to/ro.ttl", "ttl")],
    [],
  );
  if (!items[0].tooltip.includes("/path/to/ro.ttl"))
    throw new Error(items[0].tooltip);
});

t("db: count formatting (k suffix)", () => {
  const counts = new Map<string, number>([
    ["a\u0000", 42],
    ["b\u0000", 1234],
    ["c\u0000", 12345],
  ]);
  const items = buildDbSourceItems(
    [row("a"), row("b"), row("c")],
    [],
    counts,
  );
  eq(items.find((i) => i.label === "a")!.description.includes("42 ids"), true);
  eq(items.find((i) => i.label === "b")!.description.includes("1.2k ids"), true);
  eq(items.find((i) => i.label === "c")!.description.includes("12k ids"), true);
});

t("db: dataSource-sensitive active match", () => {
  // same source name, different dataSource -> different keys
  const items = buildDbSourceItems(
    [row("ro", "v1.ttl"), row("ro", "v2.ttl")],
    [sk("ro", "v2.ttl")],
  );
  const v1 = items.find((i) => i.key.dataSource === "v1.ttl")!;
  const v2 = items.find((i) => i.key.dataSource === "v2.ttl")!;
  eq(v1.isActive, false);
  eq(v2.isActive, true);
});

t("db: tooltip relative-time formatting", () => {
  // last_updated 30s ago vs ancient
  const now = 1_000_000;
  const items = buildDbSourceItems(
    [row("a", "", "csv", now - 30), row("b", "", "csv", 0)],
    [],
    undefined,
    now,
  );
  if (!items[0].tooltip.includes("30s ago"))
    throw new Error(items[0].tooltip);
  if (!items[1].tooltip.includes("never"))
    throw new Error(items[1].tooltip);
});

// ---- buildActiveSourceItems ----

t("active: empty -> empty", () => {
  eq(buildActiveSourceItems([]), []);
});

t("active: single item is 'only', no move buttons", () => {
  const items = buildActiveSourceItems([sk("bfo")]);
  eq(items.length, 1);
  eq(items[0].contextValue, "elotActiveSource.only");
  eq(items[0].canMoveUp, false);
  eq(items[0].canMoveDown, false);
  eq(items[0].prefix, "1.");
});

t("active: first/middle/last contextValues", () => {
  const items = buildActiveSourceItems([sk("a"), sk("b"), sk("c"), sk("d")]);
  eq(items.map((i) => i.contextValue), [
    "elotActiveSource.first",
    "elotActiveSource.middle",
    "elotActiveSource.middle",
    "elotActiveSource.last",
  ]);
  eq(items.map((i) => [i.canMoveUp, i.canMoveDown]), [
    [false, true],
    [true, true],
    [true, true],
    [true, false],
  ]);
});

t("active: priority numbering 1..N", () => {
  const items = buildActiveSourceItems([sk("a"), sk("b"), sk("c")]);
  eq(items.map((i) => i.prefix), ["1.", "2.", "3."]);
});

t("active: missing-from-DB warning when dbSources supplied", () => {
  const items = buildActiveSourceItems(
    [sk("bfo"), sk("ghost")],
    [row("bfo", "", "csv")],
  );
  if (!items[1].description.includes("not in DB"))
    throw new Error(items[1].description);
  if (!items[1].tooltip.includes("not currently registered"))
    throw new Error(items[1].tooltip);
  // present source: no warning
  if (items[0].description.includes("not in DB"))
    throw new Error("false-positive missing flag");
});

t("active: dataSource shows in description", () => {
  const items = buildActiveSourceItems([sk("ro", "/o/ro.ttl")]);
  if (!items[0].description.includes("/o/ro.ttl"))
    throw new Error(items[0].description);
});

console.log(`sourceItems tests: ${pass} passed, ${fail} failed`);
if (fail > 0) process.exit(1);
