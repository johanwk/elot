// src/debug-hierarchy.ts
//
// Debugging utility to print the parsed ElotNode hierarchy of an Org file.
// Usage: npx tsx src/debug-hierarchy.ts <input.org>

import { readFileSync } from "fs";
import { resolve } from "path";
import { parseOrg } from "./parseOrgWasm.js";

// Handle EPIPE gracefully (e.g. when piped to `head`)
process.stdout.on("error", (err: NodeJS.ErrnoException) => {
  if (err.code === "EPIPE") process.exit(0);
  throw err;
});

function main() {
  const args = process.argv.slice(2);
  if (args.length === 0) {
    console.error("Usage: npx tsx src/debug-hierarchy.ts <input.org>");
    process.exit(1);
  }

  const inputPath = resolve(args[0]);
  const orgText = readFileSync(inputPath, "utf-8");

  // Parse Org via orgize WASM → ElotNode tree
  const root = parseOrg(orgText);

  // Print the hierarchy as JSON
  console.log(JSON.stringify(root, null, 2));
}

main();
