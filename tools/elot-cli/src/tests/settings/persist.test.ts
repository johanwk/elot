// Step 2.3.7b: pure helpers for buildPersistPlan / describePersistResult.

import {
  buildPersistPlan,
  describePersistResult,
  type PersistPlan,
} from "../../sourceCommands/persist.js";

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
    throw new Error(
      `${msg}\n  got:      ${JSON.stringify(a)}\n  expected: ${JSON.stringify(b)}`,
    );
}

t("buildPersistPlan: empty array -> count 0, scope preserved", () => {
  const p = buildPersistPlan([], "workspace");
  eq(p.scope, "workspace");
  eq(p.count, 0);
  eq(p.value, []);
});

t("buildPersistPlan: undefined input -> empty plan", () => {
  const p = buildPersistPlan(undefined, "user");
  eq(p, { scope: "user", value: [], count: 0 } as PersistPlan);
});

t("buildPersistPlan: string entries -> canonical {source,dataSource:''} shape", () => {
  const p = buildPersistPlan(["bfo", "ro"], "user");
  eq(p.value, [
    { source: "bfo", dataSource: "" },
    { source: "ro", dataSource: "" },
  ]);
  eq(p.count, 2);
});

t("buildPersistPlan: object entries pass through; missing dataSource defaults to ''", () => {
  const p = buildPersistPlan(
    [{ source: "a", dataSource: "/x.ttl" }, { source: "b" }],
    "workspace",
  );
  eq(p.value, [
    { source: "a", dataSource: "/x.ttl" },
    { source: "b", dataSource: "" },
  ]);
});

t("buildPersistPlan: malformed entries dropped", () => {
  const p = buildPersistPlan(
    [null, "", { source: "" }, { dataSource: "x" }, "ok", { source: "good" }],
    "workspace",
  );
  eq(p.value, [
    { source: "ok", dataSource: "" },
    { source: "good", dataSource: "" },
  ]);
});

t("buildPersistPlan: order preserved", () => {
  const p = buildPersistPlan(["c", "a", "b"], "workspace");
  eq(
    p.value.map((v) => v.source),
    ["c", "a", "b"],
  );
});

t("describePersistResult: zero", () => {
  eq(
    describePersistResult({ scope: "workspace", value: [], count: 0 }),
    "ELOT: cleared active label sources at workspace scope.",
  );
  eq(
    describePersistResult({ scope: "user", value: [], count: 0 }),
    "ELOT: cleared active label sources at user scope.",
  );
});

t("describePersistResult: singular", () => {
  eq(
    describePersistResult({
      scope: "workspace",
      value: [{ source: "a", dataSource: "" }],
      count: 1,
    }),
    "ELOT: persisted 1 active label source to workspace settings.",
  );
});

t("describePersistResult: plural", () => {
  eq(
    describePersistResult({
      scope: "user",
      value: [
        { source: "a", dataSource: "" },
        { source: "b", dataSource: "" },
        { source: "c", dataSource: "" },
      ],
      count: 3,
    }),
    "ELOT: persisted 3 active label sources to user settings.",
  );
});

console.log(`persist tests: ${pass} passed, ${fail} failed`);
if (fail > 0) process.exit(1);
