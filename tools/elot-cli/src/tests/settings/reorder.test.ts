// Step 2.3.4: pure helpers for active-source set arithmetic + reorder.

import {
  SourceKey,
  keyEquals,
  indexOfKey,
  moveUp,
  moveDown,
  moveTop,
  moveBottom,
  appendUnique,
  removeByKeys,
  inactiveOf,
} from "../../sourceCommands/reorder.js";

let pass = 0;
let fail = 0;

function t(name: string, fn: () => void) {
  try {
    fn();
    pass++;
  } catch (e) {
    fail++;
    console.error(`FAIL ${name}:`, e);
  }
}

function eq<T>(a: T, b: T, msg = "") {
  if (JSON.stringify(a) !== JSON.stringify(b))
    throw new Error(`${msg}\n  got:      ${JSON.stringify(a)}\n  expected: ${JSON.stringify(b)}`);
}

const k = (s: string, d = ""): SourceKey => ({ source: s, dataSource: d });

t("keyEquals: same source + dataSource", () => {
  eq(keyEquals(k("a", "x"), k("a", "x")), true);
  eq(keyEquals(k("a", ""), { source: "a" } as SourceKey), true);
  eq(keyEquals(k("a", "x"), k("a", "y")), false);
  eq(keyEquals(k("a"), k("b")), false);
});

t("indexOfKey", () => {
  const arr = [k("a"), k("b", "x"), k("c")];
  eq(indexOfKey(arr, k("a")), 0);
  eq(indexOfKey(arr, k("b", "x")), 1);
  eq(indexOfKey(arr, k("b")), -1);
  eq(indexOfKey(arr, k("z")), -1);
});

t("moveUp basic + boundary", () => {
  const arr = [k("a"), k("b"), k("c")];
  eq(moveUp(arr, 0), arr, "no-op at top");
  eq(moveUp(arr, 1), [k("b"), k("a"), k("c")]);
  eq(moveUp(arr, 2), [k("a"), k("c"), k("b")]);
  eq(moveUp(arr, 99), arr, "out of range");
});

t("moveDown basic + boundary", () => {
  const arr = [k("a"), k("b"), k("c")];
  eq(moveDown(arr, 2), arr, "no-op at bottom");
  eq(moveDown(arr, 0), [k("b"), k("a"), k("c")]);
  eq(moveDown(arr, 1), [k("a"), k("c"), k("b")]);
});

t("moveTop / moveBottom", () => {
  const arr = [k("a"), k("b"), k("c"), k("d")];
  eq(moveTop(arr, 2), [k("c"), k("a"), k("b"), k("d")]);
  eq(moveTop(arr, 0), arr, "top-of-top is no-op");
  eq(moveBottom(arr, 1), [k("a"), k("c"), k("d"), k("b")]);
  eq(moveBottom(arr, 3), arr, "bottom-of-bottom is no-op");
});

t("immutability: helpers don't mutate input", () => {
  const arr = [k("a"), k("b"), k("c")];
  const snapshot = JSON.stringify(arr);
  moveUp(arr, 1);
  moveDown(arr, 1);
  moveTop(arr, 2);
  moveBottom(arr, 0);
  eq(JSON.stringify(arr), snapshot);
});

t("appendUnique: skip already-present, preserve order", () => {
  const arr = [k("a"), k("b")];
  eq(appendUnique(arr, [k("c"), k("a"), k("b", "x")]), [
    k("a"),
    k("b"),
    k("c"),
    k("b", "x"),
  ]);
});

t("removeByKeys", () => {
  const arr = [k("a"), k("b", "x"), k("b", ""), k("c")];
  eq(removeByKeys(arr, [k("b", "x"), k("c")]), [k("a"), k("b", "")]);
});

t("inactiveOf: set difference preserving `all` order", () => {
  const all = [k("a"), k("b"), k("c"), k("d")];
  const active = [k("c"), k("a")];
  eq(inactiveOf(all, active), [k("b"), k("d")]);
});

t("inactiveOf: dataSource-sensitive", () => {
  const all = [k("a", ""), k("a", "x")];
  const active = [k("a", "")];
  eq(inactiveOf(all, active), [k("a", "x")]);
});

console.log(`reorder tests: ${pass} passed, ${fail} failed`);
if (fail) process.exit(1);
