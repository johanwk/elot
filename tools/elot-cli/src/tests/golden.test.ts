// src/tests/golden.test.ts
//
// Golden file comparison test.
// Parses examples/bfo-core.org and compares the generated OMN output
// against examples/bfo-core.omn.

import { readFileSync } from "fs";
import { resolve, dirname } from "path";
import { fileURLToPath } from "url";
import { parseOrg } from "../parseOrgWasm.js";
import { generateFullOmn } from "../generateOmn.js";

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

const examplesDir = resolve(__dirname, "../../examples");

function main() {
  const orgPath = resolve(examplesDir, "bfo-core.org");
  const omnPath = resolve(examplesDir, "bfo-core.omn");

  const orgText = readFileSync(orgPath, "utf-8");
  const expectedOmn = readFileSync(omnPath, "utf-8");

  // Parse Org via orgize WASM → ElotNode tree
  const root = parseOrg(orgText);

  // Generate OMN
  const actualOmn = generateFullOmn(root);

  // Compare line by line (ignoring trailing whitespace)
  const expectedLines = expectedOmn.split("\n").map((l: string) => l.trimEnd());
  const actualLines = actualOmn.split("\n").map((l) => l.trimEnd());

  let passed = 0;
  let failed = 0;
  const maxLines = Math.max(expectedLines.length, actualLines.length);

  // Find first difference
  let firstDiffLine = -1;
  for (let i = 0; i < maxLines; i++) {
    const exp = expectedLines[i] ?? "<EOF>";
    const act = actualLines[i] ?? "<EOF>";
    if (exp !== act) {
      firstDiffLine = i;
      break;
    }
  }

  if (firstDiffLine === -1) {
    console.log(`PASS Golden test: ${actualLines.length} lines match`);
    passed++;
  } else {
    console.log(`FAIL Golden test at line ${firstDiffLine + 1}:`);
    // Show context (5 lines before and after)
    const start = Math.max(0, firstDiffLine - 3);
    const end = Math.min(maxLines, firstDiffLine + 10);
    for (let i = start; i < end; i++) {
      const exp = expectedLines[i] ?? "<EOF>";
      const act = actualLines[i] ?? "<EOF>";
      const marker = i === firstDiffLine ? ">>>" : "   ";
      if (exp !== act) {
        console.log(`${marker} line ${i + 1}:`);
        console.log(`    EXPECTED: ${JSON.stringify(exp)}`);
        console.log(`    ACTUAL:   ${JSON.stringify(act)}`);
      } else {
        console.log(`   line ${i + 1}: ${JSON.stringify(exp)}`);
      }
    }
    failed++;

    // Count total differences
    let totalDiffs = 0;
    for (let i = 0; i < maxLines; i++) {
      if ((expectedLines[i] ?? "<EOF>") !== (actualLines[i] ?? "<EOF>")) {
        totalDiffs++;
      }
    }
    console.log(
      `\n  Total: ${totalDiffs} differing lines out of ${maxLines}`
    );
    console.log(`  Expected ${expectedLines.length} lines, got ${actualLines.length} lines`);
  }

  console.log(`\n${passed} passed, ${failed} failed out of ${passed + failed}`);
  if (failed > 0) process.exit(1);
}

main();
