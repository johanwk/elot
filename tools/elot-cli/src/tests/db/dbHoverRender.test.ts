// src/tests/db/dbHoverRender.test.ts
//
// Step 2.3.2: hover Markdown renderer.

import { renderHoverMarkdown } from "../../dbHover/render.js";

let passed = 0;
let failed = 0;

function test(name: string, fn: () => void): void {
  try {
    fn();
    passed++;
  } catch (err) {
    failed++;
    console.error(
      `FAIL ${name}:`,
      err instanceof Error ? err.stack ?? err.message : err,
    );
  }
}

function tru(v: unknown, msg = ""): void {
  if (!v) throw new Error(msg);
}

function contains(haystack: string, needle: string, msg = ""): void {
  if (!haystack.includes(needle)) {
    throw new Error(`${msg}\n  haystack: ${haystack}\n  missing:  ${needle}`);
  }
}

function notContains(haystack: string, needle: string, msg = ""): void {
  if (haystack.includes(needle)) {
    throw new Error(`${msg}\n  haystack: ${haystack}\n  found:    ${needle}`);
  }
}

// ── Bold label header ────────────────────────────────────────────
test("Bold label header when label is non-null", () => {
  const md = renderHoverMarkdown({
    token: "ex:Widget",
    label: "Widget",
    attrs: [],
  });
  contains(md, "**Widget**");
});

test("Falls back to token in header when label is null", () => {
  const md = renderHoverMarkdown({
    token: "ex:Widget",
    label: null,
    attrs: [],
  });
  contains(md, "**ex:Widget**");
});

// ── Token row ────────────────────────────────────────────────────
test("Bare http URI rendered as clickable link", () => {
  const md = renderHoverMarkdown({
    token: "http://example.org/Widget",
    label: "Widget",
    attrs: [],
  });
  contains(md, "(http://example.org/Widget)");
});

test("CURIE rendered as inline code", () => {
  const md = renderHoverMarkdown({
    token: "ex:Widget",
    label: "Widget",
    attrs: [],
  });
  contains(md, "`ex:Widget`");
});

// ── Featured properties ──────────────────────────────────────────
test("rdf:type rendered as Type with code-formatted value", () => {
  const md = renderHoverMarkdown({
    token: "ex:Widget",
    label: "Widget",
    attrs: [["rdf:type", "owl:Class"]],
  });
  contains(md, "*Type:*");
  contains(md, "`owl:Class`");
});

test("Multiple rdf:type values are comma-joined", () => {
  const md = renderHoverMarkdown({
    token: "ex:Widget",
    label: "Widget",
    attrs: [
      ["rdf:type", "owl:Class"],
      ["rdf:type", "owl:NamedIndividual"],
    ],
  });
  contains(md, "`owl:Class`");
  contains(md, "`owl:NamedIndividual`");
});

test("skos:definition rendered as prose", () => {
  const md = renderHoverMarkdown({
    token: "ex:Widget",
    label: "Widget",
    attrs: [["skos:definition", "A generic widget."]],
  });
  contains(md, "skos");
  contains(md, "A generic widget");
});

// ── Provenance ───────────────────────────────────────────────────
test("Provenance footer shows source", () => {
  const md = renderHoverMarkdown({
    token: "ex:Widget",
    label: "Widget",
    attrs: [],
    origin: { source: "labels", dataSource: "" },
  });
  contains(md, "[src: labels]");
});

test("Provenance footer includes data_source when set", () => {
  const md = renderHoverMarkdown({
    token: "ex:Widget",
    label: "Widget",
    attrs: [],
    origin: { source: "ttl", dataSource: "/path/file.ttl" },
  });
  contains(md, "ttl");
  contains(md, "/path/file");
});

test("No provenance footer when origin is null", () => {
  const md = renderHoverMarkdown({
    token: "ex:Widget",
    label: "Widget",
    attrs: [],
  });
  notContains(md, "[src:");
});

// ── Other props (below separator) ────────────────────────────────
test("Non-featured props go below a horizontal rule", () => {
  const md = renderHoverMarkdown({
    token: "ex:Widget",
    label: "Widget",
    attrs: [
      ["dc:creator", "Alice"],
      ["dc:date", "2024-01-01"],
    ],
  });
  contains(md, "---");
  contains(md, "Alice");
});

test("rdfs:label is hidden in attrs (already in header)", () => {
  const md = renderHoverMarkdown({
    token: "ex:Widget",
    label: "Widget",
    attrs: [["rdfs:label", "Widget"]],
  });
  notContains(md, "*rdfs");
});

// ── Escaping ─────────────────────────────────────────────────────
test("Markdown special chars in label are escaped", () => {
  const md = renderHoverMarkdown({
    token: "ex:Widget",
    label: "A *bold* claim",
    attrs: [],
  });
  contains(md, "\\*bold\\*");
});

console.log(`dbHoverRender tests: ${passed} passed, ${failed} failed`);
if (failed > 0) process.exit(1);
