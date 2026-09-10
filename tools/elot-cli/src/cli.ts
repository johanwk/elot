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
import { collectAllLintErrors } from "./collectLintErrors.js";
import { elotNodeKind } from "./types.js";
import type { ElotNode } from "./types.js";

/**
 * Fail with a message on stderr and a nonzero exit status.
 *
 * The conversion pipeline has no notion of source positions (the WASM
 * parser does not return byte offsets), so messages name the file and
 * the offending heading rather than a line number.  See the briefing:
 * adding offsets to the Rust crate is Phase 4 work.
 */
function fail(message: string): never {
  console.error(`elot-cli: ${message}`);
  process.exit(1);
}

// Single source of truth is the "version" field of package.json.
//
// In the bundle, esbuild replaces __ELOT_VERSION__ with that value via
// `define` (see esbuild.mjs), and folds away the fallback branch.  When
// running from source (tsx src/cli.ts) the identifier is undefined, so
// we read package.json directly.  Either way the constant is never
// hand-maintained.
declare const __ELOT_VERSION__: string | undefined;

const VERSION =
  typeof __ELOT_VERSION__ !== "undefined"
    ? __ELOT_VERSION__
    : (require("../package.json").version as string);

const program = new Command();

program
  .name("elot-cli")
  .description("Convert ELOT Org-mode ontology files to OWL Manchester Syntax or HTML")
  .version(VERSION, "-V, --version")
  .addCommand(buildDbCommand())
  .argument("[input.org]", "Input Org-mode ontology file")
  .argument("[output]", "Output file path (default: tangle target or stdout for OMN; input.html for HTML)")
  .option("--html", "Export to styled HTML via Pandoc (requires Pandoc on PATH)")
  .option("--lint", "Report ELOT lint diagnostics; exit 1 if any error is found")
  .addHelpText("after", `
Examples:
  $ elot-cli ontology.org                  Generate OMN (tangle target or stdout)
  $ elot-cli ontology.org output.omn       Generate OMN to explicit file
  $ elot-cli ontology.org -                Generate OMN to stdout
  $ elot-cli --html ontology.org           Export to HTML (requires Pandoc)
  $ elot-cli --html ontology.org out.html  Export to HTML with explicit output
  $ elot-cli --lint ontology.org           Report lint diagnostics`)
  .action(async (input: string | undefined, output: string | undefined, opts: { html?: boolean; lint?: boolean }) => {
    if (!input) {
      program.help();
      return;
    }
    const inputPath = resolve(input);

    // --html and --lint do different jobs and produce different exit
    // semantics; combining them is always a mistake, so say so rather
    // than silently letting one win.
    if (opts.html && opts.lint) {
      fail("--html and --lint are mutually exclusive");
    }

    if (opts.lint) {
      // -- Lint only: no output file is produced --
      let orgText: string;
      try {
        orgText = readFileSync(inputPath, "utf-8");
      } catch (err: any) {
        fail(`cannot read ${inputPath}: ${err.message}`);
      }

      let root: ElotNode;
      try {
        root = parseOrg(orgText);
      } catch (err: any) {
        fail(`failed to parse ${inputPath}: ${err.message}`);
      }

      const diagnostics = collectAllLintErrors(root);
      for (const d of diagnostics) {
        // No byte offsets from the WASM parser, so a diagnostic is
        // located by its heading unless a checker supplied a line.
        const where = d.line !== undefined ? `:${d.line}` : "";
        const heading = d.node.title ? ` [${d.node.title}]` : "";
        console.log(`${inputPath}${where}: ${d.severity}:${heading} ${d.message}`);
      }

      const errors = diagnostics.filter((d) => d.severity === "error").length;
      const warnings = diagnostics.length - errors;
      console.error(`elot-cli: ${errors} error(s), ${warnings} warning(s)`);
      process.exit(errors > 0 ? 1 : 0);
    }

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
      let orgText: string;
      try {
        orgText = readFileSync(inputPath, "utf-8");
      } catch (err: any) {
        fail(`cannot read ${inputPath}: ${err.message}`);
      }

      let root: ElotNode;
      try {
        root = parseOrg(orgText);
      } catch (err: any) {
        fail(`failed to parse ${inputPath}: ${err.message}`);
      }

      // A file with no ontology heading is a user error, not an empty
      // document: silently emitting nothing hides a typo in the :ID:
      // property or a missing top-level heading.
      const ontologies = (root.children ?? []).filter(
        (n) => elotNodeKind(n) === "ontology"
      );
      if (ontologies.length === 0) {
        fail(
          `no ELOT ontology heading found in ${inputPath} ` +
            `(expected a top-level heading with an :ID: property ending in ` +
            `"-ontology-declaration" under it)`
        );
      }

      let omn: string;
      try {
        omn = generateFullOmn(root);
      } catch (err: any) {
        fail(`failed to generate OMN from ${inputPath}: ${err.message}`);
      }

      if (omn.trim() === "") {
        fail(`generated OMN for ${inputPath} is empty`);
      }

      const write = (outputPath: string) => {
        try {
          writeFileSync(outputPath, omn, "utf-8");
        } catch (err: any) {
          fail(`cannot write ${outputPath}: ${err.message}`);
        }
        console.error(`Written to ${outputPath}`);
      };

      if (output && output !== "-" && output !== "/dev/stdout") {
        write(resolve(output));
      } else if (output === "-" || output === "/dev/stdout") {
        process.stdout.write(omn);
      } else {
        const tangleTarget = ontologies[0]?.tangleTargetOmn;
        if (tangleTarget) {
          write(resolve(dirname(inputPath), tangleTarget));
        } else {
          process.stdout.write(omn);
        }
      }
    }
  });

program.parseAsync(process.argv).catch((err: any) => {
  console.error(`elot-cli: ${err?.message ?? err}`);
  process.exit(1);
});
