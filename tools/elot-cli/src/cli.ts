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
import { Command } from "commander";
import { parseOrg } from "./parseOrgWasm.js";
import { generateFullOmn } from "./generateOmn.js";
import { findPandoc, exportOrgToHtml } from "./exportHtml.js";
import { buildDbCommand } from "./dbCli.js";

// Read version from package.json at build time (inlined by esbuild)
const VERSION = "0.3.4";

const program = new Command();

program
  .name("elot-cli")
  .description("Convert ELOT Org-mode ontology files to OWL Manchester Syntax or HTML")
  .version(VERSION, "-V, --version")
  .addCommand(buildDbCommand())
  .argument("[input.org]", "Input Org-mode ontology file")
  .argument("[output]", "Output file path (default: tangle target or stdout for OMN; input.html for HTML)")
  .option("--html", "Export to styled HTML via Pandoc (requires Pandoc on PATH)")
  .addHelpText("after", `
Examples:
  $ elot-cli ontology.org                  Generate OMN (tangle target or stdout)
  $ elot-cli ontology.org output.omn       Generate OMN to explicit file
  $ elot-cli ontology.org -                Generate OMN to stdout
  $ elot-cli --html ontology.org           Export to HTML (requires Pandoc)
  $ elot-cli --html ontology.org out.html  Export to HTML with explicit output`)
  .action(async (input: string | undefined, output: string | undefined, opts: { html?: boolean }) => {
    if (!input) {
      program.help();
      return;
    }
    const inputPath = resolve(input);

    if (opts.html) {
      // ── HTML export via Pandoc ──
      const pandocPath = findPandoc();
      if (!pandocPath) {
        console.error("Error: Pandoc not found on PATH.");
        console.error("Install Pandoc from https://pandoc.org/installing.html");
        process.exit(1);
      }

      const outputPath = output ? resolve(output) : undefined;
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

      if (output && output !== "-" && output !== "/dev/stdout") {
        const outputPath = resolve(output);
        writeFileSync(outputPath, omn, "utf-8");
        console.error(`Written to ${outputPath}`);
      } else if (output === "-" || output === "/dev/stdout") {
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
  });

program.parseAsync(process.argv);
