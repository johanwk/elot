// src/tests/buildSlurp.test.ts
//
// Tests for buildSlurp and buildLabelMap.

import { readFileSync } from "fs";
import { resolve, dirname } from "path";
import { fileURLToPath } from "url";
import { parseOrg } from "../parseOrgWasm.js";
import { buildSlurp, buildLabelMap } from "../buildSlurp.js";

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

const examplesDir = resolve(__dirname, "../../examples");

function assert(condition: boolean, msg: string) {
  if (!condition) {
    console.error(`FAIL: ${msg}`);
    process.exit(1);
  }
}

function main() {
  let passed = 0;

  // ── Test 1: buildSlurp on bfo-core.org produces entries ────────
  {
    const orgText = readFileSync(resolve(examplesDir, "bfo-core.org"), "utf-8");
    const root = parseOrg(orgText);
    const slurp = buildSlurp(root);

    assert(slurp.size > 0, "buildSlurp should return non-empty map");
    console.log(`  buildSlurp returned ${slurp.size} entries`);
    passed++;
  }

  // ── Test 2: known entity has correct label ─────────────────────
  {
    const orgText = readFileSync(resolve(examplesDir, "bfo-core.org"), "utf-8");
    const root = parseOrg(orgText);
    const slurp = buildSlurp(root);

    // "entity" is the label for obo:BFO_0000001 in BFO
    const entry = slurp.get("obo:BFO_0000001");
    assert(entry !== undefined, "obo:BFO_0000001 should be in slurp");
    assert(
      entry!.label === '"entity"@en',
      `obo:BFO_0000001 label should be '"entity"@en', got '${entry!.label}'`
    );
    assert(
      entry!.rdfType === "owl:Class",
      `obo:BFO_0000001 rdfType should be 'owl:Class', got '${entry!.rdfType}'`
    );
    console.log(`  obo:BFO_0000001 → label="${entry!.label}", rdfType=${entry!.rdfType}`);
    passed++;
  }

  // ── Test 3: buildLabelMap convenience wrapper ──────────────────
  {
    const orgText = readFileSync(resolve(examplesDir, "bfo-core.org"), "utf-8");
    const root = parseOrg(orgText);
    const labelMap = buildLabelMap(root);

    assert(labelMap.size > 0, "buildLabelMap should return non-empty map");
    assert(
      labelMap.get("obo:BFO_0000001") === '"entity"@en',
      `buildLabelMap("obo:BFO_0000001") should be '"entity"@en'`
    );
    console.log(`  buildLabelMap returned ${labelMap.size} entries`);
    passed++;
  }

  // ── Test 4: entities without explicit labels use URI ───────────
  {
    const orgText = readFileSync(resolve(examplesDir, "bfo-core.org"), "utf-8");
    const root = parseOrg(orgText);
    const slurp = buildSlurp(root);

    // Check that every entry has a non-empty label
    for (const [uri, entry] of slurp) {
      assert(
        entry.label.length > 0,
        `Entry ${uri} should have non-empty label`
      );
    }
    console.log(`  All ${slurp.size} entries have non-empty labels`);
    passed++;
  }

  // ── Test 5: dump a few entries for manual inspection ───────────
  {
    const orgText = readFileSync(resolve(examplesDir, "bfo-core.org"), "utf-8");
    const root = parseOrg(orgText);
    const slurp = buildSlurp(root);

    console.log("\n  Sample slurp entries:");
    let count = 0;
    for (const [uri, entry] of slurp) {
      if (count >= 5) break;
      const props = (entry.properties ?? []).map((p) => `${p.tag}=${p.value}`).join(", ");
      console.log(`    ${uri} → "${entry.label}" [${entry.rdfType ?? "?"}] ${props ? `{${props}}` : ""}`);
      count++;
    }
    passed++;
  }

  console.log(`\n${passed} passed, 0 failed`);
}

main();
