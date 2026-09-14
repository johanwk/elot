// Generate provenance and checksums for the four standalone CLI runtime files.
// Recommended: make upstream (rebuilds first). Running this script directly only
// inventories the existing dist/ files; it cannot establish their build origin.
// Uses Node built-ins plus Git and npm, including on Windows/Git Bash.
import { execFileSync, execSync } from "node:child_process";
import { createHash } from "node:crypto";
import { readFileSync, writeFileSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const root = dirname(fileURLToPath(import.meta.url));
const dist = join(root, "dist");
const files = ["cli.js", "elot_orgize_bg.wasm", "sql-wasm.wasm", "schema.sql"];
const options = { cwd: root, encoding: "utf8" };
const git = (...args) => execFileSync("git", args, options).trim();
const sha256 = (data) => createHash("sha256").update(data).digest("hex");

try {
  // Read every asset before writing either output; missing files are errors.
  const sums = files.map((name) => ({
    name,
    hash: sha256(readFileSync(join(dist, name))),
  }));
  const commit = git("rev-parse", "HEAD");
  const status = git("status", "--porcelain=v1", "--untracked-files=all");
  const lines = status ? status.split(/\r?\n/) : [];
  const untracked = lines.filter((line) => line.startsWith("??")).length;
  const tracked = lines.length - untracked;
  const version = execFileSync(process.execPath, [join(dist, "cli.js"), "--version"], options).trim();
  // A fixed shell command also works with npm.cmd on Windows.
  const npmVersion = execSync("npm --version", options).trim();
  const manifest = sums.map(({ name, hash }) => `${hash}  ${name}\n`).join("");
  const provenance = `#+title: ELOT CLI upstream provenance

* Source reference
- Repository: https://github.com/johanwk/elot
- Checkout HEAD at inventory time: ${commit}
- Bundled CLI reported version: ${version}
- Licence: GPL-3.0-or-later (retain the upstream licence notice separately).

* Inventory and environment
- Generated (UTC): ${new Date().toISOString()}
- Node.js: ${process.version}
- npm: ${npmVersion}
- Platform: ${process.platform} / ${process.arch}
- Recommended generation command: make -C tools/elot-cli upstream
- That target runs: npm run bundle, then node generate-upstream.mjs
- Tracked changed entries across the repository: ${tracked}
- Untracked entries across the repository: ${untracked}

This file inventories the current dist/ files and checkout. The Git commit
identifies committed source, while the checksums identify the exact artifacts.
Running the script directly does not rebuild or prove which source produced
existing artifacts. Use the make target, and do not edit source during the build.
Ignored files (including installed dependencies and generated inputs) are not
covered by the checkout counts. A clean status alone is not proof of a
reproducible build. This inventory does not certify that tests passed.

* SHA-256 runtime checksums
#+begin_example
${manifest}#+end_example

SHA256SUMS file SHA-256: ${sha256(manifest)}

* Verify in the destination repository
Copy the four runtime files, UPSTREAM.org, and SHA256SUMS together.
From their destination directory run:

#+begin_src sh
sha256sum -c SHA256SUMS
#+end_src

If cli.js is renamed to cli.cjs, change the filename in SHA256SUMS; its
artifact checksum stays the same, but the manifest's own checksum changes.
Preserve file bytes when copying. Git line-ending conversion can change hashes;
consider marking the vendored runtime files and SHA256SUMS as -text in the
destination repository's .gitattributes.
`;
  writeFileSync(join(dist, "SHA256SUMS"), manifest, "utf8");
  writeFileSync(join(dist, "UPSTREAM.org"), provenance, "utf8");
  console.log("Wrote dist/UPSTREAM.org and dist/SHA256SUMS");
  if (tracked || untracked) {
    console.warn(`Note: checkout has ${tracked} tracked changed and ${untracked} untracked entries; recorded in UPSTREAM.org.`);
  }
} catch (error) {
  console.error(`Cannot generate upstream inventory: ${error.message}`);
  process.exitCode = 1;
}
