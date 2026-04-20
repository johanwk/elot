// src/cli.ts
//
// CLI entry point for elot-cli.
// Reads an Org file, parses it via orgize WASM, generates OMN,
// and writes to stdout or a file determined by the :header-args:omn:
// :tangle property.
//
// With --html, exports to styled HTML via Pandoc instead.

import { readFileSync, writeFileSync } from "fs";
import { resolve, dirname } from "path";
import { parseOrg } from "./parseOrgWasm.js";
import { generateFullOmn } from "./generateOmn.js";
import { findPandoc, exportOrgToHtml } from "./exportHtml.js";

function printUsage() {
  console.error("Usage: elot-cli <input.org> [output]");
  console.error("       elot-cli --html <input.org> [output.html]");
  console.error("");
  console.error("Options:");
  console.error("  --html    Export to styled HTML via Pandoc (requires Pandoc on PATH)");
  console.error("");
  console.error("Without --html, generates OWL Manchester Syntax.");
  console.error("If output is omitted, uses tangle target from Org file (OMN) or stdout.");
  console.error("If output is '-', writes to stdout (OMN only).");
}

async function main() {
  const rawArgs = process.argv.slice(2);

  if (rawArgs.length === 0 || rawArgs.includes("--help") || rawArgs.includes("-h")) {
    printUsage();
    process.exit(rawArgs.length === 0 ? 1 : 0);
  }

  // Parse flags
  const htmlMode = rawArgs.includes("--html");
  const args = rawArgs.filter(a => a !== "--html");

  if (args.length === 0) {
    printUsage();
    process.exit(1);
  }

  const inputPath = resolve(args[0]);

  if (htmlMode) {
    // ── HTML export via Pandoc ──
    const pandocPath = findPandoc();
    if (!pandocPath) {
      console.error("Error: Pandoc not found on PATH.");
      console.error("Install Pandoc from https://pandoc.org/installing.html");
      process.exit(1);
    }

    const outputPath = args[1] ? resolve(args[1]) : undefined;
    try {
      const outPath = await exportOrgToHtml(inputPath, pandocPath, outputPath);
      console.error(`HTML written to ${outPath}`);
    } catch (err: any) {
      console.error(`HTML export failed: ${err.message}`);
      process.exit(1);
    }
  } else {
    // ── OMN export ──
    const orgText = readFileSync(inputPath, "utf-8");
    const root = parseOrg(orgText);
    const omn = generateFullOmn(root);

    if (args[1] && args[1] !== "-" && args[1] !== "/dev/stdout") {
      const outputPath = resolve(args[1]);
      writeFileSync(outputPath, omn, "utf-8");
      console.error(`Written to ${outputPath}`);
    } else if (args[1] === "-" || args[1] === "/dev/stdout") {
      process.stdout.write(omn);
    } else {
      const firstOntology = (root.children ?? [])[0];
      const tangleTarget = firstOntology?.tangleTargetOmn;

      if (tangleTarget) {
        const outputPath = resolve(dirname(inputPath), tangleTarget);
        writeFileSync(outputPath, omn, "utf-8");
        console.error(`Written to ${outputPath}`);
      } else {
        process.stdout.write(omn);
      }
    }
  }
}

main();
