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
import { join } from "path";

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
    console.log(`Copied ${f} → dist/${f}`);
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
