// esbuild.mjs
//
// Custom esbuild script that handles the orgize WASM glue code.
//
// Problem: wasm-pack (--target nodejs) generates glue JS that loads
// the .wasm file via fs.readFileSync(path.join(__dirname, '...bg.wasm')).
// esbuild's --loader:.wasm=file only handles direct .wasm imports, not
// this runtime pattern. So we need to:
//
// 1. Let esbuild bundle the glue JS normally (it's just JS)
// 2. Copy the .wasm file to dist/ so __dirname references resolve at runtime
//
// This script builds both extension.js and cli.js.

import * as esbuild from "esbuild";
import { copyFileSync, mkdirSync, existsSync, readdirSync, rmSync } from "fs";
import { createRequire } from "module";
import { dirname, join } from "path";

const require = createRequire(import.meta.url);

// Copy .wasm files from src/wasm/ to dist/
function copyWasmFiles() {
  const wasmDir = "src/wasm";
  const distDir = "dist";

  // Clean dist/ of stale files from previous builds
  if (existsSync(distDir)) {
    rmSync(distDir, { recursive: true });
  }
  mkdirSync(distDir, { recursive: true });

  if (!existsSync(wasmDir)) {
    console.error(`ERROR: ${wasmDir} does not exist. Run 'npm run build:wasm' first.`);
    process.exit(1);
  }

  const wasmFiles = readdirSync(wasmDir).filter((f) => f.endsWith(".wasm"));
  if (wasmFiles.length === 0) {
    console.error(`ERROR: No .wasm files in ${wasmDir}. Run 'npm run build:wasm' first.`);
    process.exit(1);
  }

  for (const f of wasmFiles) {
    copyFileSync(join(wasmDir, f), join(distDir, f));
    console.log(`Copied ${f} -> dist/${f}`);
  }

  // Copy sql.js's sql-wasm.wasm so ElotDb.locateFile() can find it
  // sibling-of-bundle at runtime.  Without this, the bundled extension
  // breaks once node_modules/ is stripped during .vsix packaging.
  // Copy elot-package/schema.sql so ElotDb.locateSchemaSql() can find
  // it sibling-of-bundle at runtime.  The repo-relative candidates
  // (../../../../elot-package/schema.sql etc.) don't exist inside the
  // packaged .vsix because elot-package/ lives outside tools/elot-cli/
  // and is therefore not included.
  try {
    const schemaSrc = join("..", "..", "elot-package", "schema.sql");
    if (!existsSync(schemaSrc)) {
      console.error(`ERROR: could not find schema.sql at ${schemaSrc}`);
      process.exit(1);
    }
    copyFileSync(schemaSrc, join(distDir, "schema.sql"));
    console.log(`Copied schema.sql -> dist/schema.sql`);
  } catch (err) {
    console.error(`ERROR: failed to copy schema.sql: ${err.message}`);
    process.exit(1);
  }

  try {
    const sqlJsEntry = require.resolve("sql.js");
    // sql.js entry sits in package root or in dist/; walk up to the
    // package, then dive into dist/.
    let pkgDir = dirname(sqlJsEntry);
    while (pkgDir && !existsSync(join(pkgDir, "package.json"))) {
      const parent = dirname(pkgDir);
      if (parent === pkgDir) break;
      pkgDir = parent;
    }
    const wasmSrc = join(pkgDir, "dist", "sql-wasm.wasm");
    if (!existsSync(wasmSrc)) {
      console.error(`ERROR: could not find sql-wasm.wasm at ${wasmSrc}`);
      process.exit(1);
    }
    copyFileSync(wasmSrc, join(distDir, "sql-wasm.wasm"));
    console.log(`Copied sql-wasm.wasm -> dist/sql-wasm.wasm`);
  } catch (err) {
    console.error(`ERROR: failed to locate/copy sql.js wasm: ${err.message}`);
    process.exit(1);
  }
}

// esbuild plugin: rewrite the wasm-pack glue's path.join(__dirname, '...bg.wasm')
// so it resolves correctly when bundled into dist/.
// The glue code does: path.join(__dirname, 'elot_orgize_bg.wasm')
// After bundling into dist/extension.js, __dirname IS dist/, and we've
// copied the .wasm file there, so this just works. No plugin needed!

const sharedOptions = {
  bundle: true,
  platform: "node",
  target: "node18",
  format: "cjs",
  sourcemap: false,
  // TypeScript with "module": "Node16" requires .js extensions in imports
  // (e.g. './parseOrgWasm.js'). Tell esbuild to try .ts first so it can
  // resolve these to the actual source files.
  resolveExtensions: [".ts", ".js", ".json"],
};

async function build() {
  copyWasmFiles();

  // Build the VS Code extension
  await esbuild.build({
    ...sharedOptions,
    entryPoints: ["src/extension.ts"],
    outfile: "dist/extension.js",
    external: ["vscode"],
  });
  console.log("Built dist/extension.js");

  // Build the CLI
  await esbuild.build({
    ...sharedOptions,
    entryPoints: ["src/cli.ts"],
    outfile: "dist/cli.js",
    // Make CLI executable by prepending shebang via banner
    banner: { js: "#!/usr/bin/env node" },
  });
  console.log("Built dist/cli.js");
}

build().catch((err) => {
  console.error(err);
  process.exit(1);
});
