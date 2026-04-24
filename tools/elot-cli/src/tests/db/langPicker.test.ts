// src/tests/db/langPicker.test.ts
//
// Pure-helper tests for the language picker, ported from
// test/elot-lang-prefs-test.el.  Run with:
//   npx tsx src/tests/db/langPicker.test.ts

import {
  effectiveLanguagePrefs,
  looksLikeCurieP,
  looksLikeUriP,
  pickValueByLang,
  selectByLanguage,
  UNTAGGED,
} from "../../db/langPicker.js";

let passed = 0;
let failed = 0;

function assertEqual<T>(actual: T, expected: T, msg: string): void {
  const ok = JSON.stringify(actual) === JSON.stringify(expected);
  if (ok) {
    passed++;
  } else {
    failed++;
    console.error(`FAIL: ${msg}`);
    console.error(`  expected: ${JSON.stringify(expected)}`);
    console.error(`  actual:   ${JSON.stringify(actual)}`);
  }
}

function test(name: string, fn: () => void): void {
  try {
    fn();
  } catch (e) {
    failed++;
    console.error(`THROW in ${name}: ${(e as Error).message}`);
  }
}

// ─── selectByLanguage / pickValueByLang ──────────────────────────

test("untagged wins by default", () => {
  assertEqual(
    selectByLanguage([
      { value: "hello", lang: "en" },
      { value: "hallo", lang: "de" },
      { value: "plain", lang: null },
    ]),
    { value: "plain", lang: null },
    "untagged beats en/de with default prefs",
  );
  assertEqual(
    selectByLanguage([
      { value: "hello", lang: "en" },
      { value: "plain", lang: "" },
    ]),
    { value: "plain", lang: "" },
    "empty-string lang counts as untagged",
  );
});

test("en is default second choice", () => {
  assertEqual(
    selectByLanguage([
      { value: "hallo", lang: "de" },
      { value: "bonjour", lang: "fr" },
      { value: "hello", lang: "en" },
    ]),
    { value: "hello", lang: "en" },
    "en wins over de/fr",
  );
  assertEqual(
    selectByLanguage([
      { value: "hallo", lang: "de" },
      { value: "hello", lang: "EN" },
    ]),
    { value: "hello", lang: "EN" },
    "case-insensitive match",
  );
});

test("alphabetical fallback", () => {
  assertEqual(
    selectByLanguage([
      { value: "annyeong", lang: "ko" },
      { value: "hallo", lang: "de" },
      { value: "bonjour", lang: "fr" },
    ]),
    { value: "hallo", lang: "de" },
    "de wins alphabetically over fr/ko",
  );
});

test("explicit prefs override default", () => {
  const rows = [
    { value: "hallo", lang: "de" },
    { value: "hello", lang: "en" },
    { value: "plain", lang: null },
    { value: "annyeong", lang: "ko" },
  ];
  assertEqual(
    selectByLanguage(rows, ["ko"]),
    { value: "annyeong", lang: "ko" },
    "ko pref picks ko row",
  );
  assertEqual(
    selectByLanguage(rows, ["de", "en"]),
    { value: "hallo", lang: "de" },
    "de,en prefs pick de",
  );
});

test("UNTAGGED sentinel position", () => {
  const rows = [
    { value: "hallo", lang: "de" },
    { value: "hello", lang: "en" },
    { value: "plain", lang: null },
  ];
  assertEqual(
    selectByLanguage(rows, ["ko", UNTAGGED]),
    { value: "plain", lang: null },
    "ko miss -> :untagged wins",
  );
  assertEqual(
    selectByLanguage(rows, ["en"]),
    { value: "hello", lang: "en" },
    "appended untagged loses to explicit en",
  );
  assertEqual(
    selectByLanguage(
      [
        { value: "plain", lang: null },
        { value: "hello", lang: "en" },
      ],
      ["en", UNTAGGED],
    ),
    { value: "hello", lang: "en" },
    "en before :untagged wins",
  );
});

test("effectiveLanguagePrefs normalisation", () => {
  assertEqual(
    effectiveLanguagePrefs(null),
    [UNTAGGED, "en"],
    "null -> default policy",
  );
  assertEqual(
    effectiveLanguagePrefs([]),
    [UNTAGGED, "en"],
    "empty -> default policy",
  );
  assertEqual(
    effectiveLanguagePrefs(["ko"]),
    ["ko", UNTAGGED],
    "tail :untagged appended",
  );
  assertEqual(
    effectiveLanguagePrefs([UNTAGGED, "en"]),
    [UNTAGGED, "en"],
    "explicit :untagged preserved",
  );
});

test("empty/singleton input", () => {
  assertEqual(selectByLanguage([]), null, "empty -> null");
  assertEqual(pickValueByLang([]), null, "pickValueByLang empty -> null");
  assertEqual(
    selectByLanguage([{ value: "only", lang: "xx" }]),
    { value: "only", lang: "xx" },
    "singleton -> self",
  );
  assertEqual(
    selectByLanguage([{ value: "only", lang: null }]),
    { value: "only", lang: null },
    "singleton untagged -> self",
  );
});

// ─── looksLikeUriP / looksLikeCurieP ─────────────────────────────

test("looksLikeUriP / looksLikeCurieP", () => {
  assertEqual(looksLikeUriP("http://example.org/foo"), true, "http URI");
  assertEqual(looksLikeUriP("ex:foo"), false, "CURIE is not URI");
  assertEqual(looksLikeUriP("plain"), false, "no colon is not URI");
  assertEqual(looksLikeCurieP("ex:foo"), true, "CURIE detected");
  assertEqual(looksLikeCurieP("http://x/y"), false, "URI not CURIE");
  assertEqual(looksLikeCurieP("plain"), false, "no colon not CURIE");
  assertEqual(looksLikeCurieP(":default"), true, "default prefix CURIE");
});

// ─── summary ─────────────────────────────────────────────────────

console.log(`langPicker tests: ${passed} passed, ${failed} failed`);
if (failed > 0) process.exit(1);
