// src/tests/settings/activeSources.test.ts
//
// Step 2.3.1: settings normalisation for the VS Code bridge.
//
// Note: "active sources" is a *user-side* selection/ordering layer over
// whatever sources happen to be in the DB.  The DB itself has no notion
// of "active" -- it just stores rows under a `source` label.  These tests
// exercise the settings.json sanitisers, not anything DB-related, hence
// their location under tests/settings/ rather than tests/db/.

import {
  normalizeActiveSources,
  normalizePreferredLanguages,
  getEffectiveLanguagePrefs,
} from "../../activeSources.js";
import { UNTAGGED } from "../../db/langPicker.js";

let passed = 0;
let failed = 0;

function test(name: string, fn: () => void): void {
  try {
    fn();
    passed++;
  } catch (err) {
    failed++;
    console.error(`FAIL ${name}:`, err instanceof Error ? err.message : err);
  }
}

function eqJson(a: unknown, e: unknown, msg = ""): void {
  const aj = JSON.stringify(a);
  const ej = JSON.stringify(e);
  if (aj !== ej) throw new Error(`${msg}\n  got:      ${aj}\n  expected: ${ej}`);
}

// ── activeLabelSources ─────────────────────────────────────────

test("activeLabelSources: undefined/null/non-array -> []", () => {
  eqJson(normalizeActiveSources(undefined), []);
  eqJson(normalizeActiveSources(null), []);
  eqJson(normalizeActiveSources("not-an-array"), []);
  eqJson(normalizeActiveSources(42), []);
});

test("activeLabelSources: bare strings get empty dataSource", () => {
  eqJson(normalizeActiveSources(["foo", "bar"]), [
    { source: "foo", dataSource: "" },
    { source: "bar", dataSource: "" },
  ]);
});

test("activeLabelSources: object form preserved", () => {
  eqJson(
    normalizeActiveSources([
      { source: "foo", dataSource: "/tmp/foo.csv" },
      { source: "bar" },
    ]),
    [
      { source: "foo", dataSource: "/tmp/foo.csv" },
      { source: "bar", dataSource: "" },
    ],
  );
});

test("activeLabelSources: drops invalid entries", () => {
  eqJson(
    normalizeActiveSources([
      "ok",
      "",
      null,
      42,
      { source: "" },
      { source: 42 },
      { dataSource: "no-source" },
      { source: "good", dataSource: "x" },
    ]),
    [
      { source: "ok", dataSource: "" },
      { source: "good", dataSource: "x" },
    ],
  );
});

test("activeLabelSources: order preserved", () => {
  const raw = ["a", { source: "b" }, "c", { source: "d", dataSource: "ds" }];
  eqJson(normalizeActiveSources(raw), [
    { source: "a", dataSource: "" },
    { source: "b", dataSource: "" },
    { source: "c", dataSource: "" },
    { source: "d", dataSource: "ds" },
  ]);
});

// ── preferredLanguages ─────────────────────────────────────────

test("preferredLanguages: undefined/null/non-array -> []", () => {
  eqJson(normalizePreferredLanguages(undefined), []);
  eqJson(normalizePreferredLanguages(null), []);
  eqJson(normalizePreferredLanguages("en"), []);
});

test("preferredLanguages: keeps strings in order, drops empties/non-strings", () => {
  eqJson(
    normalizePreferredLanguages(["en", "", "ko", null, 42, "no"]),
    ["en", "ko", "no"],
  );
});

// ── getEffectiveLanguagePrefs ──────────────────────────────────

test("getEffectiveLanguagePrefs: empty -> default policy", () => {
  eqJson(getEffectiveLanguagePrefs([]), [UNTAGGED, "en"]);
  eqJson(getEffectiveLanguagePrefs(undefined), [UNTAGGED, "en"]);
});

test("getEffectiveLanguagePrefs: non-empty -> appends untagged", () => {
  eqJson(getEffectiveLanguagePrefs(["ko"]), ["ko", UNTAGGED]);
  eqJson(getEffectiveLanguagePrefs(["no", "en"]), ["no", "en", UNTAGGED]);
});

console.log(`activeSources tests: ${passed} passed, ${failed} failed`);
if (failed > 0) process.exit(1);
