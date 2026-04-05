// src/cli.ts
//
// CLI entry point for elot-cli.
// Reads an Org file, parses it via orgize WASM, generates OMN,
// and writes to stdout or a file determined by the :header-args:omn:
// :tangle property.

import { readFileSync, writeFileSync } from "fs";
import { resolve, dirname } from "path";
import { parseOrg } from "./parseOrgWasm.js";
import { generateFullOmn } from "./generateOmn.js";

function main() {
  const args = process.argv.slice(2);

  if (args.length === 0) {
    console.error("Usage: elot-cli <input.org> [output.omn]");
    console.error("  If output is omitted, uses tangle target from Org file or stdout.");
    process.exit(1);
  }

  const inputPath = resolve(args[0]);
  const orgText = readFileSync(inputPath, "utf-8");

  // Parse Org via orgize WASM → ElotNode tree
  const root = parseOrg(orgText);

  // Generate OMN
  const omn = generateFullOmn(root);

  // Determine output
  if (args[1] && args[1] !== "-" && args[1] !== "/dev/stdout") {
    // Explicit output path (not stdout alias)
    const outputPath = resolve(args[1]);
    writeFileSync(outputPath, omn, "utf-8");
    console.error(`Written to ${outputPath}`);
  } else if (args[1] === "-" || args[1] === "/dev/stdout") {
    // Explicit stdout
    process.stdout.write(omn);
  } else {
    // Try to find tangle target from the first ontology node
    const firstOntology = (root.children ?? [])[0];
    const tangleTarget = firstOntology?.tangleTargetOmn;

    if (tangleTarget) {
      // Resolve relative to the input file's directory
      const outputPath = resolve(dirname(inputPath), tangleTarget);
      writeFileSync(outputPath, omn, "utf-8");
      console.error(`Written to ${outputPath}`);
    } else {
      // No tangle target — write to stdout
      process.stdout.write(omn);
    }
  }
}

main();
