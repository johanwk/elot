// src/tests/labelLookup/items.test.ts
//
// Step 2.4.1: pure unit tests for the label-lookup item builder.

import {
  buildLookupItems,
  canonicalId,
  insertTokenForId,
  langSuffixForSingleton,
  suffixForId,
  type DbLookupView,
  type LocalEntry,
} from "../../labelLookup/items.js";
import type { LangPref, LangRow } from "../../db/langPicker.js";

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

function eq<T>(a: T, e: T, msg = ""): void {
  if (a !== e)
    throw new Error(
      `${msg}\n  got:      ${String(a)}\n  expected: ${String(e)}`,
    );
}

function deepEq(a: unknown, e: unknown, msg = ""): void {
  const sa = JSON.stringify(a);
  const se = JSON.stringify(e);
  if (sa !== se)
    throw new Error(`${msg}\n  got:      ${sa}\n  expected: ${se}`);
}

// Build a fake DbLookupView from plain JS data.
interface FakeDb {
  idsByLabel: Map<string, string[]>;
  prefixes: Array<{ prefix: string; expansion: string }>;
  variants: Map<string, LangRow[]>;
  rdfType: Map<string, string>;
}
function fake(opts: Partial<FakeDb>): DbLookupView {
  const idsByLabel = opts.idsByLabel ?? new Map();
  const prefixes = (opts.prefixes ?? []).slice().sort(
    (a, b) => b.expansion.length - a.expansion.length,
  );
  const variants = opts.variants ?? new Map();
  const rdfType = opts.rdfType ?? new Map();
  return {
    idsByLabel,
    contractUri(uri: string): string | null {
      for (const p of prefixes) {
        if (uri.startsWith(p.expansion)) {
          return `${p.prefix}:${uri.slice(p.expansion.length)}`;
        }
      }
      return null;
    },
    labelVariants(id: string): LangRow[] | null {
      return variants.get(id) ?? null;
    },
    rdfTypeForId(id: string): string | null {
      return rdfType.get(id) ?? null;
    },
  };
}

// ─── canonicalId ──────────────────────────────────────────────

test("canonicalId leaves CURIE alone", () => {
  eq(canonicalId("ex:Widget"), "ex:Widget");
  eq(canonicalId(":local"), ":local");
});

test("canonicalId strips angle brackets from a single bracketed URI", () => {
  eq(canonicalId("<http://example.org/Widget>"), "http://example.org/Widget");
});

test("canonicalId leaves bare URI alone", () => {
  eq(canonicalId("http://example.org/Widget"), "http://example.org/Widget");
});

// ─── suffixForId ──────────────────────────────────────────────

test("suffixForId returns CURIE id verbatim", () => {
  eq(suffixForId("ex:Widget", null), "ex:Widget");
});

test("suffixForId contracts URI via DB", () => {
  const db = fake({
    prefixes: [{ prefix: "ex", expansion: "http://example.org/" }],
  });
  eq(suffixForId("http://example.org/Widget", db), "ex:Widget");
});

test("suffixForId falls back to URI tail when no contraction", () => {
  const db = fake({});
  eq(suffixForId("http://example.org/Widget", db), "Widget");
});

// ─── langSuffixForSingleton (Step 1.16.8) ─────────────────────

test("langSuffix empty when id has no variants", () => {
  const db = fake({});
  eq(langSuffixForSingleton("Widget", "ex:Widget", db), "");
});

test("langSuffix empty when single variant", () => {
  const db = fake({
    variants: new Map([
      ["ex:Widget", [{ value: "Widget", lang: "en" }]],
    ]),
  });
  eq(langSuffixForSingleton("Widget", "ex:Widget", db), "");
});

test("langSuffix returns @lang when label matches winner with tag", () => {
  const db = fake({
    variants: new Map([
      [
        "ex:Widget",
        [
          { value: "Widget", lang: "en" },
          { value: "Dings", lang: "de" },
        ],
      ],
    ]),
  });
  // Default prefs prefer untagged then en; en wins.
  eq(langSuffixForSingleton("Widget", "ex:Widget", db), "@en");
});

test("langSuffix empty when winner has no language tag", () => {
  const db = fake({
    variants: new Map([
      [
        "ex:Widget",
        [
          { value: "Widget", lang: "" },
          { value: "Dings", lang: "de" },
        ],
      ],
    ]),
  });
  eq(langSuffixForSingleton("Widget", "ex:Widget", db), "");
});

test("langSuffix empty when label != winner", () => {
  const db = fake({
    variants: new Map([
      [
        "ex:Widget",
        [
          { value: "Widget", lang: "en" },
          { value: "Dings", lang: "de" },
        ],
      ],
    ]),
  });
  // Asking about "Dings" but winner under default prefs is "Widget".
  eq(langSuffixForSingleton("Dings", "ex:Widget", db), "");
});

test("langSuffix honours explicit prefs", () => {
  const db = fake({
    variants: new Map([
      [
        "ex:Widget",
        [
          { value: "Widget", lang: "en" },
          { value: "Dings", lang: "de" },
        ],
      ],
    ]),
  });
  const prefs: LangPref[] = ["de"];
  eq(langSuffixForSingleton("Dings", "ex:Widget", db, prefs), "@de");
});

// ─── insertTokenForId ────────────────────────────────────────

test("insertTokenForId returns CURIE as-is", () => {
  eq(insertTokenForId("ex:Widget", null), "ex:Widget");
});

test("insertTokenForId contracts URI via DB", () => {
  const db = fake({
    prefixes: [{ prefix: "ex", expansion: "http://example.org/" }],
  });
  eq(insertTokenForId("http://example.org/Widget", db), "ex:Widget");
});

test("insertTokenForId prefers local contract", () => {
  const db = fake({
    prefixes: [{ prefix: "db", expansion: "http://example.org/" }],
  });
  const local = (uri: string): string | null =>
    uri === "http://example.org/Widget" ? "buf:Widget" : null;
  eq(insertTokenForId("http://example.org/Widget", db, local), "buf:Widget");
});

test("insertTokenForId returns angle-bracketed IRI when no prefix known", () => {
  const db = fake({});
  eq(
    insertTokenForId("http://example.org/Widget", db),
    "<http://example.org/Widget>",
  );
});

test("insertTokenForId strips brackets first then re-wraps if needed", () => {
  const db = fake({});
  eq(
    insertTokenForId("<http://example.org/Widget>", db),
    "<http://example.org/Widget>",
  );
});

// ─── buildLookupItems: scope = local ─────────────────────────

test("local scope: uses only local entries, ignores DB", () => {
  const local: LocalEntry[] = [
    { id: "ex:Widget", label: "Widget", rdfType: "owl:Class" },
    { id: "ex:Gadget", label: "Gadget" },
  ];
  const db = fake({
    idsByLabel: new Map([["External", ["ex:External"]]]),
  });
  const items = buildLookupItems({ scope: "local", local, db });
  eq(items.length, 2);
  eq(items[0].label, "Widget");
  eq(items[0].id, "ex:Widget");
  eq(items[0].rdfType, "owl:Class");
  eq(items[0].origin, "local");
  eq(items[1].label, "Gadget");
  eq(items[1].rdfType, "");
});

// ─── buildLookupItems: scope = external ──────────────────────

test("external scope: uses only DB entries, ignores local", () => {
  const local: LocalEntry[] = [
    { id: "ex:LocalOnly", label: "LocalOnly" },
  ];
  const db = fake({
    idsByLabel: new Map([
      ["Widget", ["ex:Widget"]],
      ["Gadget", ["ex:Gadget"]],
    ]),
    rdfType: new Map([["ex:Widget", "owl:Class"]]),
  });
  const items = buildLookupItems({ scope: "external", local, db });
  eq(items.length, 2);
  deepEq(
    items.map((i) => [i.label, i.id, i.origin]),
    [
      ["Widget", "ex:Widget", "external"],
      ["Gadget", "ex:Gadget", "external"],
    ],
  );
  eq(items[0].rdfType, "owl:Class");
});

// ─── buildLookupItems: scope = both, dedup ──────────────────

test("both scope: local wins on id collision; origin = both", () => {
  const local: LocalEntry[] = [
    { id: "ex:Widget", label: "Widget (local)", rdfType: "owl:Class" },
  ];
  const db = fake({
    idsByLabel: new Map([
      ["Widget (db)", ["ex:Widget"]], // colliding id -> dedup
      ["Gadget", ["ex:Gadget"]], // unique
    ]),
  });
  const items = buildLookupItems({ scope: "both", local, db });
  eq(items.length, 2);
  // First is the local cell, with origin upgraded to "both" because
  // the DB also knew the id.
  eq(items[0].label, "Widget (local)");
  eq(items[0].origin, "both");
  eq(items[1].label, "Gadget");
  eq(items[1].origin, "external");
});

test("both scope: angle-bracket vs bare URI dedup", () => {
  const local: LocalEntry[] = [
    {
      id: "<http://example.org/Widget>",
      label: "Widget",
      rdfType: "owl:Class",
    },
  ];
  const db = fake({
    idsByLabel: new Map([["WidgetDB", ["http://example.org/Widget"]]]),
  });
  const items = buildLookupItems({ scope: "both", local, db });
  eq(items.length, 1);
  eq(items[0].label, "Widget"); // local wins
  eq(items[0].origin, "both");
  eq(items[0].id, "http://example.org/Widget"); // canonical
});

// ─── buildLookupItems: collision suffix ─────────────────────

test("colliding labels get CURIE/tail suffix in detail", () => {
  const db = fake({
    idsByLabel: new Map([
      ["Widget", ["ex:Widget", "ex2:Widget"]],
    ]),
    prefixes: [
      { prefix: "ex", expansion: "http://example.org/" },
      { prefix: "ex2", expansion: "http://example2.org/" },
    ],
  });
  const items = buildLookupItems({ scope: "external", local: [], db });
  eq(items.length, 2);
  eq(items[0].detail, "ex:Widget");
  eq(items[1].detail, "ex2:Widget");
});

test("colliding labels with URI ids: tail fallback when no prefix", () => {
  const db = fake({
    idsByLabel: new Map([
      [
        "Widget",
        [
          "http://a.example.org/Widget",
          "http://b.example.org/Widget",
        ],
      ],
    ]),
  });
  const items = buildLookupItems({ scope: "external", local: [], db });
  eq(items.length, 2);
  eq(items[0].detail, "Widget"); // tail, from a.example.org
  eq(items[1].detail, "Widget"); // tail, from b.example.org
});

// ─── buildLookupItems: lang-only singleton suffix (Step 1.16.8) ──

test("singleton with multiple lang variants gets @LANG suffix", () => {
  const db = fake({
    idsByLabel: new Map([["Widget", ["ex:Widget"]]]),
    variants: new Map([
      [
        "ex:Widget",
        [
          { value: "Widget", lang: "en" },
          { value: "Dings", lang: "de" },
        ],
      ],
    ]),
  });
  const items = buildLookupItems({ scope: "external", local: [], db });
  eq(items.length, 1);
  eq(items[0].detail, "@en");
});

test("colliding-label suffix overrides @LANG suffix", () => {
  // Two ids with the same label and one of them has multi-variants;
  // disambig suffix should be the CURIE, not @lang.
  const db = fake({
    idsByLabel: new Map([["Widget", ["ex:A", "ex:B"]]]),
    variants: new Map([
      [
        "ex:A",
        [
          { value: "Widget", lang: "en" },
          { value: "Dings", lang: "de" },
        ],
      ],
    ]),
  });
  const items = buildLookupItems({ scope: "external", local: [], db });
  eq(items.length, 2);
  eq(items[0].detail, "ex:A");
  eq(items[1].detail, "ex:B");
});

// ─── buildLookupItems: origin marker in scope=both ──────────

test("origin marker [external] only appears in scope=both", () => {
  const db = fake({
    idsByLabel: new Map([["Widget", ["ex:Widget"]]]),
  });
  const both = buildLookupItems({ scope: "both", local: [], db });
  eq(both[0].detail, "[external]");

  const ext = buildLookupItems({ scope: "external", local: [], db });
  eq(ext[0].detail, "");
});

test("origin marker absent for pure-local in scope=both", () => {
  const local: LocalEntry[] = [{ id: "ex:Widget", label: "Widget" }];
  const both = buildLookupItems({ scope: "both", local, db: null });
  eq(both[0].detail, "");
  eq(both[0].origin, "local");
});

// ─── exit ────────────────────────────────────────────────────

console.log(`labelLookupItems tests: ${passed} passed, ${failed} failed`);
process.exit(failed === 0 ? 0 : 1);
