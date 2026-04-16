// src/tests/integration.test.ts
//
// Integration test for the full ELOT VS Code feature set (Steps 1–4).
//
// Tests that:
// 1. buildSlurp produces entries from bfo-core.org
// 2. Hover logic resolves CURIEs to label/type info
// 3. Label decoration CURIE regex + slurp lookup work end-to-end
// 4. Status bar / package.json configuration is correct
//
// Run: npx tsx src/tests/integration.test.ts

import { readFileSync } from "fs";
import { resolve } from "path";
import { buildSlurp, buildLabelMap } from "../buildSlurp.js";
import { parseOrg } from "../parseOrgWasm.js";
const bfoOrgPath = resolve(__dirname, "../../examples/bfo-core.org");
const pkgJsonPath = resolve(__dirname, "../../package.json");

// ─── Helpers ─────────────────────────────────────────────────────

let passed = 0;
let failed = 0;

function assert(label: string, condition: boolean, detail?: string) {
  if (condition) {
    console.log(`  ✓ ${label}`);
    passed++;
  } else {
    console.error(`  ✗ ${label}${detail ? " — " + detail : ""}`);
    failed++;
  }
}

// ─── Setup ───────────────────────────────────────────────────────

const orgText = readFileSync(bfoOrgPath, "utf-8");
const root = parseOrg(orgText);
const slurpMap = buildSlurp(root);
const labelMap = buildLabelMap(root);
const pkgJson = JSON.parse(readFileSync(pkgJsonPath, "utf-8"));

console.log(`\n  Loaded bfo-core.org: ${slurpMap.size} entities\n`);

// ─── 1. buildSlurp sanity ────────────────────────────────────────

assert("buildSlurp returns non-empty map", slurpMap.size > 0);
assert(
  "buildLabelMap returns same count",
  labelMap.size === slurpMap.size,
  `labelMap=${labelMap.size}, slurpMap=${slurpMap.size}`,
);

const bfo1 = slurpMap.get("obo:BFO_0000001");
assert("obo:BFO_0000001 is in slurp map", bfo1 !== undefined);
assert(
  'obo:BFO_0000001 label contains "entity"',
  bfo1?.label.includes("entity") ?? false,
  `label=${bfo1?.label}`,
);
assert(
  "obo:BFO_0000001 rdfType is owl:Class",
  bfo1?.rdfType === "owl:Class",
  `rdfType=${bfo1?.rdfType}`,
);

// ─── 2. Hover resolution ────────────────────────────────────────

// Simulate what the hover provider does: match a CURIE, look it up
const CURIE_RE = /(?:[a-zA-Z][-a-zA-Z0-9_.]*|):[-\w_./:]+/;

const testLines = [
  'SubClassOf: obo:BFO_0000001',
  '  - rdf:type :: owl:Class',
  '  - rdfs:label :: "entity"@en',
  '  obo:BFO_0000002 and obo:BFO_0000003',
];

for (const line of testLines) {
  const m = line.match(CURIE_RE);
  assert(`CURIE regex matches in "${line}"`, m !== null, `match=${m?.[0]}`);
}

// Check that hover would produce useful info for a known entity
const hoverEntry = slurpMap.get("obo:BFO_0000002");
assert("obo:BFO_0000002 found in slurp", hoverEntry !== undefined);
assert(
  "hover would show useful info (label ≠ URI)",
  hoverEntry !== undefined && hoverEntry.label !== hoverEntry.uri,
  `label=${hoverEntry?.label}`,
);

// Check that at least one entry has extra properties (for rich hover)
let richCount = 0;
let richExample = "";
for (const [uri, entry] of slurpMap) {
  if (entry.properties && entry.properties.length > 0) {
    richCount++;
    if (!richExample) {
      richExample = `${uri}: ${entry.properties.map((p) => p.tag).join(", ")}`;
    }
  }
}
assert(
  "At least one entry has extra properties",
  richCount > 0,
  `${richCount} entries, e.g. ${richExample}`,
);

// ─── 3. Label decoration CURIE matching ─────────────────────────

const CURIE_GLOBAL_RE = /(?:[a-zA-Z][-a-zA-Z0-9_.]*|):[-\w_./]+/g;

// Count how many CURIE occurrences in the Org text resolve to labels
let totalCuries = 0;
let decoratableCuries = 0;
const seenLabels = new Set<string>();

CURIE_GLOBAL_RE.lastIndex = 0;
let match: RegExpExecArray | null;
while ((match = CURIE_GLOBAL_RE.exec(orgText)) !== null) {
  totalCuries++;
  const entry = slurpMap.get(match[0]);
  if (entry && entry.label !== entry.uri) {
    decoratableCuries++;
    seenLabels.add(entry.label);
  }
}

console.log(
  `  CURIE occurrences: ${totalCuries} total, ${decoratableCuries} decoratable, ${seenLabels.size} unique labels`,
);
assert("Found CURIE occurrences in Org text", totalCuries > 0);
assert("Some CURIEs are decoratable", decoratableCuries > 0);
assert(
  "Multiple unique labels",
  seenLabels.size > 1,
  `${seenLabels.size} unique labels`,
);

// ─── 4. package.json correctness ────────────────────────────────

// Commands
const commands = pkgJson.contributes?.commands ?? [];
const cmdNames = commands.map((c: any) => c.command);
assert(
  "elot.tangle command registered",
  cmdNames.includes("elot.tangle"),
);
assert(
  "elot.toggleLabels command registered",
  cmdNames.includes("elot.toggleLabels"),
);

// Keybindings
const keybindings = pkgJson.contributes?.keybindings ?? [];
const f5Binding = keybindings.find(
  (kb: any) => kb.command === "elot.toggleLabels" && kb.key === "f5",
);
assert("F5 keybinding for toggleLabels exists", f5Binding !== undefined);
assert(
  "F5 keybinding scoped to .org files",
  f5Binding?.when?.includes(".org") ?? false,
  `when=${f5Binding?.when}`,
);

// Configuration
const config = pkgJson.contributes?.configuration;
assert("contributes.configuration exists", config !== undefined);
const props = config?.properties ?? {};
assert(
  "elot.labelDisplay.fontStyle setting exists",
  "elot.labelDisplay.fontStyle" in props,
);
assert(
  "elot.labelDisplay.hoverEnabled setting exists",
  "elot.labelDisplay.hoverEnabled" in props,
);
assert(
  "fontStyle default is italic",
  props["elot.labelDisplay.fontStyle"]?.default === "italic",
);
assert(
  "hoverEnabled default is true",
  props["elot.labelDisplay.hoverEnabled"]?.default === true,
);

// ─── Summary ─────────────────────────────────────────────────────

console.log(`\n${passed} passed, ${failed} failed\n`);
if (failed > 0) process.exit(1);
