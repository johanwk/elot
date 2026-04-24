// src/tests/db/parsers.test.ts
//
// Step 2.2.3 parser tests.  Covers CSV / TSV / JSON (nested and flat)
// including the Step 1.16.6 language conventions and the backward-
// compatible baseline.  Mirrors test/elot-sources-lang-test.el.

import { mkdtempSync, rmSync, writeFileSync } from "fs";
import { tmpdir } from "os";
import { join } from "path";
import { parseCsv, parseTsv, splitCsvLine } from "../../parsers/csvTsv.js";
import { parseJson } from "../../parsers/json.js";
import { parseSource, detectTypeFromExtension } from "../../parsers/index.js";
import { AttrValue, EntityTriple } from "../../db/sqljs.js";

let passed = 0;
let failed = 0;

function t(name: string, fn: () => void): void {
  try {
    fn();
    passed++;
  } catch (e) {
    failed++;
    console.error(`FAIL ${name}:`, e);
  }
}
function assert(cond: unknown, msg: string): void {
  if (!cond) throw new Error(msg);
}
function eq<T>(a: T, b: T, msg: string): void {
  if (JSON.stringify(a) !== JSON.stringify(b)) {
    throw new Error(`${msg}\n  got:      ${JSON.stringify(a)}\n  expected: ${JSON.stringify(b)}`);
  }
}

function labelRows(
  attrs: Array<[string, AttrValue]> | undefined,
): AttrValue[] {
  return (attrs ?? [])
    .filter(([p]) => p === "rdfs:label")
    .map(([, v]) => v);
}
function attrPlain(
  attrs: Array<[string, AttrValue]> | undefined,
  prop: string,
): string | undefined {
  const hit = (attrs ?? []).find(([p]) => p === prop);
  if (!hit) return undefined;
  const v = hit[1];
  return typeof v === "string" ? v : v.value;
}

const tmp = mkdtempSync(join(tmpdir(), "elot-parsers-"));
function fixture(name: string, content: string): string {
  const p = join(tmp, name);
  writeFileSync(p, content, "utf-8");
  return p;
}

// ─── splitCsvLine -----------------------------------------------------

t("splitCsvLine: simple unquoted", () => {
  eq(splitCsvLine("a,b,c", ","), ["a", "b", "c"], "simple");
});
t("splitCsvLine: quoted with comma", () => {
  eq(
    splitCsvLine('"a,1","b",c', ","),
    ["a,1", "b", "c"],
    "quoted-comma",
  );
});
t("splitCsvLine: escaped doubled-quote", () => {
  eq(
    splitCsvLine('"he said ""hi""",x', ","),
    ['he said "hi"', "x"],
    "doubled-quote",
  );
});
t("splitCsvLine: trailing empty field", () => {
  eq(splitCsvLine("a,b,", ","), ["a", "b", ""], "trailing-empty");
});
t("splitCsvLine: empty string", () => {
  eq(splitCsvLine("", ","), [], "empty");
});
t("splitCsvLine: TSV", () => {
  eq(splitCsvLine("a\tb\tc", "\t"), ["a", "b", "c"], "tsv");
});

// ─── CSV: lang column, label@TAG, backward-compat -------------------

t("parseCsv: lang column recognised", () => {
  const f = fixture(
    "lang.csv",
    "id,label,lang,definition\n" +
      "ex:A,English one,en,\"The English one.\"\n" +
      "ex:B,\uD55C\uAD6D\uC5B4,ko,\n" +
      "ex:C,untagged thing,,\"no tag.\"\n",
  );
  const { entries } = parseCsv(f);
  eq(entries.length, 3, "three rows");
  const a = entries.find((e) => e.id === "ex:A")!;
  eq(a.label, "English one", "A label");
  eq(
    labelRows(a.attrs),
    [{ value: "English one", lang: "en" }],
    "A rdfs:label tagged",
  );
  assert(
    attrPlain(a.attrs, "lang") === undefined,
    "lang column consumed, not emitted",
  );
  eq(attrPlain(a.attrs, "definition"), "The English one.", "A def");
  const c = entries.find((e) => e.id === "ex:C")!;
  eq(
    labelRows(c.attrs),
    [{ value: "untagged thing", lang: "" }],
    "C empty-tag row present",
  );
});

t("parseCsv: label@TAG suffix columns", () => {
  const f = fixture(
    "at.csv",
    "id,label,label@ko,label@en-GB\n" +
      "ex:X,Widget,\uC704\uC824,Widget\n" +
      "ex:Y,Only English,,\n",
  );
  const { entries } = parseCsv(f);
  eq(entries.length, 2, "two rows");
  const x = entries.find((e) => e.id === "ex:X")!;
  eq(x.label, "Widget", "X label");
  const xLabels = labelRows(x.attrs);
  eq(xLabels.length, 2, "X has two rdfs:label rows");
  assert(
    xLabels.some(
      (r) =>
        typeof r !== "string" &&
        r.value === "\uC704\uC824" &&
        r.lang === "ko",
    ),
    "X ko row present",
  );
  assert(
    xLabels.some(
      (r) => typeof r !== "string" && r.value === "Widget" && r.lang === "en-GB",
    ),
    "X en-GB row present",
  );
  // label@TAG columns consumed, not emitted as plain attrs.
  assert(
    attrPlain(x.attrs, "label@ko") === undefined,
    "label@ko column consumed",
  );
  const y = entries.find((e) => e.id === "ex:Y")!;
  eq(labelRows(y.attrs), [], "Y has no rdfs:label rows");
  eq(y.label, "Only English", "Y label");
});

t("parseCsv: no lang conventions - backward compat", () => {
  const f = fixture(
    "plain.csv",
    "id,label,definition\nex:A,Widget,\"A generic widget.\"\n",
  );
  const { entries } = parseCsv(f);
  eq(entries.length, 1, "one row");
  const a = entries[0];
  eq(a.id, "ex:A", "id");
  eq(a.label, "Widget", "label");
  eq(labelRows(a.attrs), [], "no rdfs:label rows");
  eq(attrPlain(a.attrs, "definition"), "A generic widget.", "def");
});

t("parseCsv: skips blank lines and trims CR", () => {
  const f = fixture(
    "blank.csv",
    "id,label\r\n\r\nex:A,Widget\r\n\r\nex:B,Gadget\r\n",
  );
  const { entries } = parseCsv(f);
  eq(entries.length, 2, "two rows");
  eq(entries.map((e) => e.id), ["ex:A", "ex:B"], "ids");
});

// ─── TSV -------------------------------------------------------------

t("parseTsv: lang column honoured", () => {
  const f = fixture(
    "lang.tsv",
    "id\tlabel\tlang\nex:A\tEnglish one\ten\n",
  );
  const { entries } = parseTsv(f);
  eq(
    labelRows(entries[0].attrs),
    [{ value: "English one", lang: "en" }],
    "tsv lang",
  );
});

// ─── JSON -------------------------------------------------------------

t("parseJson: flat shape", () => {
  const f = fixture("flat.json", '{"ex:A":"Widget","ex:B":"Gadget"}');
  const { entries } = parseJson(f);
  eq(entries.length, 2, "two entries");
  const a = entries.find((e) => e.id === "ex:A")!;
  eq(a.label, "Widget", "flat label");
  eq(labelRows(a.attrs), [], "no rdfs:label rows in flat form");
});

t("parseJson: nested lang key", () => {
  const f = fixture(
    "nested.json",
    JSON.stringify({ "ex:A": { label: "English one", lang: "en" } }),
  );
  const { entries } = parseJson(f);
  const a = entries[0];
  eq(a.label, "English one", "label");
  eq(
    labelRows(a.attrs),
    [{ value: "English one", lang: "en" }],
    "rdfs:label tagged",
  );
});

t("parseJson: nested label@TAG keys", () => {
  const f = fixture(
    "at.json",
    JSON.stringify({
      "ex:X": { label: "Widget", "label@ko": "\uC704\uC824" },
    }),
  );
  const { entries } = parseJson(f);
  const x = entries[0];
  eq(x.label, "Widget", "label");
  assert(
    labelRows(x.attrs).some(
      (r) =>
        typeof r !== "string" && r.value === "\uC704\uC824" && r.lang === "ko",
    ),
    "ko row present",
  );
});

t("parseJson: nested other props become attributes", () => {
  const f = fixture(
    "props.json",
    JSON.stringify({
      "ex:A": { label: "Widget", definition: "A widget." },
    }),
  );
  const { entries } = parseJson(f);
  eq(attrPlain(entries[0].attrs, "definition"), "A widget.", "def");
});

t("parseJson: scalar non-string coerced to label", () => {
  const f = fixture("num.json", '{"ex:A":42}');
  const { entries } = parseJson(f);
  eq(entries[0].label, "42", "coerced");
});

t("parseJson: lang without label - no rdfs:label row emitted", () => {
  const f = fixture(
    "lang-no-label.json",
    JSON.stringify({ "ex:A": { lang: "en" } }),
  );
  const { entries } = parseJson(f);
  eq(labelRows(entries[0].attrs), [], "no rows");
  eq(entries[0].label, "", "empty label");
});

// ─── dispatcher -------------------------------------------------------

t("detectTypeFromExtension", () => {
  eq(detectTypeFromExtension("a.csv"), "csv", "csv");
  eq(detectTypeFromExtension("a.TSV"), "tsv", "tsv uppercase");
  eq(detectTypeFromExtension("a.json"), "json", "json");
  eq(detectTypeFromExtension("a.ttl"), "ttl", "ttl");
  eq(detectTypeFromExtension("a.rq"), "rq", "rq");
  eq(detectTypeFromExtension("a.org"), "org", "org");
  eq(detectTypeFromExtension("a.md"), null, "unknown");
});

t("parseSource dispatches CSV / TSV / JSON", () => {
  const c = fixture("d.csv", "id,label\nex:A,X\n");
  const s = fixture("d.tsv", "id\tlabel\nex:A\tX\n");
  const j = fixture("d.json", '{"ex:A":"X"}');
  eq(parseSource(c, "csv").entries.length, 1, "csv");
  eq(parseSource(s, "tsv").entries.length, 1, "tsv");
  eq(parseSource(j, "json").entries.length, 1, "json");
});

t("parseSource rejects unimplemented types", () => {
  let threw = false;
  try {
    parseSource("x.ttl", "ttl");
  } catch {
    threw = true;
  }
  assert(threw, "ttl should throw in 2.2.3");
});

// ─── teardown --------------------------------------------------------

try {
  rmSync(tmp, { recursive: true, force: true });
} catch {
  /* ignore */
}

// Silence unused-import warnings.
void (null as unknown as EntityTriple);

console.log(`parsers tests: ${passed} passed, ${failed} failed`);
process.exit(failed > 0 ? 1 : 0);
