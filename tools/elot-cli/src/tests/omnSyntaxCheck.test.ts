// src/tests/omnSyntaxCheck.test.ts
//
// Tests for OMN syntax checking via Peggy parser.
// Test cases are loaded from the shared file syntax/test-cases.json
// (same source used by the Elisp test runner syntax/test-grammar.el).
//
// Run:  npx tsx src/tests/omnSyntaxCheck.test.ts

import { parseWithRule } from "../omnSyntaxCheck.js";
import { readFileSync } from "fs";
import { resolve } from "path";

// --- Load shared test cases from JSON ---

const testCasesPath = resolve(__dirname, "../../../../syntax/test-cases.json");
const testData: {
  testGroups: Array<{
    startRule: string;
    positive: [string, string][];
    negative: [string, string][];
  }>;
} = JSON.parse(readFileSync(testCasesPath, "utf-8"));

// --- Test runner ---

let passed = 0;
let failed = 0;

function expectPass(desc: string, input: string, startRule: string) {
  const result = parseWithRule(input, startRule);
  if (result.ok) {
    console.log(`  PASS: ${desc}`);
    passed++;
  } else {
    console.log(`  FAIL: ${desc}`);
    console.log(`        input: ${input}`);
    console.log(`        error: ${result.message}`);
    failed++;
  }
}

function expectFail(desc: string, input: string, startRule: string) {
  const result = parseWithRule(input, startRule);
  if (!result.ok) {
    console.log(`  PASS: ${desc}`);
    passed++;
  } else {
    console.log(`  FAIL: ${desc}  (should not have parsed!)`);
    console.log(`        input: ${input}`);
    failed++;
  }
}

// --- Run all test groups ---

console.log("\n=== OMN Syntax Check Tests (Peggy) ===\n");

for (const group of testData.testGroups) {
  console.log(`--- ${group.startRule} ---`);

  for (const [desc, input] of group.positive) {
    expectPass(desc, input, group.startRule);
  }

  for (const [desc, input] of group.negative) {
    expectFail(desc, input, group.startRule);
  }

  console.log();
}

// --- Summary ---

console.log(`${passed} passed, ${failed} failed (of ${passed + failed} total)\n`);
if (failed > 0) process.exit(1);
