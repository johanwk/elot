// src/tests/db/orgParser.test.ts
//
// End-to-end test for parsers/org.ts: reuses the existing
// parseOrgWasm + buildSlurp pipeline and shapes the result into
// the ParsedSource form consumed by ElotDb.updateSource().

import { resolve, join } from "path";
import { mkdtempSync, rmSync } from "fs";
import { tmpdir } from "os";

import { parseOrgSource } from "../../parsers/org.js";
import { parseSource, detectTypeFromExtension } from "../../parsers/index.js";
import { ElotDb } from "../../db/sqljs.js";

const examplesDir = resolve(__dirname, "../../../examples");

let passed = 0;
let failed = 0;

function t(name: string, fn: () => void | Promise<void>): Promise<void> {
  return Promise.resolve()
    .then(() => fn())
    .then(
      () => {
        passed++;
      },
      (e) => {
        failed++;
        console.error(`FAIL ${name}: ${(e as Error).message}`);
        if ((e as Error).stack)
          console.error((e as Error).stack!.split("\n").slice(1, 4).join("\n"));
      },
    );
}

function eq<T>(got: T, expected: T, msg: string) {
  if (got !== expected) {
    throw new Error(
      `${msg}\n  got:      ${JSON.stringify(got)}\n  expected: ${JSON.stringify(expected)}`,
    );
  }
}

function assert(cond: unknown, msg: string) {
  if (!cond) throw new Error(msg);
}

async function run() {
  const orgFile = resolve(examplesDir, "bfo-core.org");

  // 1. Dispatcher recognises .org.
  await t("detectTypeFromExtension('.org') -> 'org'", () => {
    eq(detectTypeFromExtension("foo.org"), "org", "detect");
  });

  // 2. Direct parseOrgSource on the BFO example.
  let parsed: ReturnType<typeof parseOrgSource>;
  await t("parseOrgSource(bfo-core.org) returns entries", () => {
    parsed = parseOrgSource(orgFile);
    assert(
      parsed.entries.length > 0,
      `expected non-empty entries, got ${parsed.entries.length}`,
    );
  });

  // 3. Known entity round-trip: obo:BFO_0000001 -> 'entity'.
  //    buildSlurp (Elisp parity) strips lang tags from headings, so
  //    the rdfs:label row is untagged plain text.
  await t("obo:BFO_0000001 has label 'entity' and rdfs:label row", () => {
    const e = parsed.entries.find((x) => x.id === "obo:BFO_0000001");
    assert(e !== undefined, "obo:BFO_0000001 entry missing");
    eq(e!.label, "entity", "denormalised label");
    const labels = (e!.attrs ?? []).filter(([p]) => p === "rdfs:label");
    assert(labels.length >= 1, `expected >=1 rdfs:label row, got ${labels.length}`);
    const first = labels[0][1];
    const val = typeof first === "string" ? first : (first as any).value;
    eq(val, "entity", "rdfs:label value");
  });

  // 3b. Regression: simple bracketed URI ids must be stored
  //     unbracketed so the two-pass resolver (CURIE-expand /
  //     URI-contract) round-trips correctly.  Composite ids of the
  //     form "<URI1> <URI2>" (ontology heading with versionIRI)
  //     must be left intact -- a naive outer-bracket strip would
  //     mangle them into "URI1> <URI2".
  await t("ids: simple URIs unbracketed; composite ids preserved", () => {
    // No id should look like the corrupt-mangled form.
    for (const e of parsed.entries) {
      assert(
        !/^[^<\s][^\s]*>\s+</.test(e.id),
        `mangled id detected: ${e.id}`,
      );
    }
    // No simple single-URI id should retain enclosing brackets.
    for (const e of parsed.entries) {
      if (e.id.startsWith("<") && e.id.endsWith(">") && !/\s/.test(e.id)) {
        throw new Error(`simple URI id still bracketed: ${e.id}`);
      }
    }
  });

  // 4. rdfType is emitted as an rdf:type attribute row.
  await t("obo:BFO_0000001 carries rdf:type owl:Class", () => {
    const e = parsed.entries.find((x) => x.id === "obo:BFO_0000001")!;
    const types = (e.attrs ?? []).filter(([p]) => p === "rdf:type");
    assert(types.length >= 1, "no rdf:type row");
    eq(types[0][1] as string, "owl:Class", "rdf:type value");
  });

  // 5. Dispatcher wiring: parseSource(file, 'org') matches direct call.
  await t("parseSource(file, 'org') equals parseOrgSource(file)", () => {
    const viaDispatcher = parseSource(orgFile, "org");
    eq(viaDispatcher.entries.length, parsed.entries.length, "entry count");
  });

  // 6. End-to-end through ElotDb: updateSource -> save -> reopen -> getLabel.
  const tmp = mkdtempSync(join(tmpdir(), "elot-org-test-"));
  const dbPath = join(tmp, "elot.sqlite");
  try {
    await t("parseOrgSource -> updateSource -> getLabel round-trip", async () => {
      const d = await ElotDb.open(dbPath);
      try {
        const n = d.updateSource(
          "bfo-core",
          "",
          "org",
          parsed.entries,
          0,
        );
        if (parsed.prefixes) {
          for (const [p, e] of parsed.prefixes) {
            d.addPrefix("bfo-core", "", p, e);
          }
        }
        d.save(dbPath);
        assert(n > 0, `expected n>0, got ${n}`);
      } finally {
        d.close();
      }

      const d2 = await ElotDb.open(dbPath);
      try {
        const active = [{ source: "bfo-core", dataSource: "" }];
        const label = d2.getLabel("obo:BFO_0000001", active);
        eq(label, "entity", "getLabel reads back denormalised label");
      } finally {
        d2.close();
      }
    });
  } finally {
    rmSync(tmp, { recursive: true, force: true });
  }

  console.log(
    `orgParser tests: ${passed} passed, ${failed} failed`,
  );
  if (failed > 0) process.exitCode = 1;
}

void run();
