// src/tests/electron/runTests.ts
//
// Step 2.3.5b - Electron test harness entrypoint.
//
// Launches a fresh VS Code instance with the bundled extension loaded,
// pointed at a pre-seeded fixture workspace, and runs the Mocha suite
// in `suite/index.ts`.
//
// Pre-conditions (the runner enforces them):
//   - The extension is already bundled: `dist/extension.js` exists.
//   - The fixture workspace has been prepared (see `prepareFixtures.ts`,
//     run as a separate tsx step before this script via the
//     `pretest:electron` npm hook).
//   - The Mocha suite has been compiled to `dist-test/`.

import * as path from "path";
import { existsSync } from "fs";
import { runTests } from "@vscode/test-electron";

async function main() {
  // __dirname at runtime is dist-test/ (rootDir was set to
  // src/tests/electron in tsconfig.test.json), so the extension root
  // is one level up.
  const extensionDevelopmentPath = path.resolve(__dirname, "..");

  const bundle = path.join(extensionDevelopmentPath, "dist", "extension.js");
  if (!existsSync(bundle)) {
    console.error(
      `ERROR: ${bundle} does not exist.  Run 'npm run bundle' before 'npm run test:electron'.`,
    );
    process.exit(2);
  }

  const extensionTestsPath = path.resolve(__dirname, "suite", "index.js");

  const workspacePath = path.join(
    extensionDevelopmentPath,
    "dist-test",
    "fixtures",
    "workspace",
  );
  if (!existsSync(workspacePath)) {
    console.error(
      `ERROR: fixture workspace ${workspacePath} not found.  Run 'npm run prepare:electron-fixtures' first.`,
    );
    process.exit(2);
  }

  // Allow reusing an already-installed VS Code binary instead of
  // downloading one.  Useful behind corporate TLS-intercepting
  // proxies where Node cannot verify update.code.visualstudio.com.
  // Set ELOT_VSCODE_EXECUTABLE to the GUI binary, e.g. on Windows:
  //   C:\Users\<you>\AppData\Local\Programs\Microsoft VS Code\Code.exe
  // (NOT the bin/code shim -- that's a CLI launcher.)
  const localExe = process.env.ELOT_VSCODE_EXECUTABLE;
  const baseOpts = {
    extensionDevelopmentPath,
    extensionTestsPath,
    launchArgs: [
      workspacePath,
      "--disable-extensions",
      "--disable-gpu",
    ],
  };

  let runOpts: Parameters<typeof runTests>[0];
  if (localExe && localExe.length > 0) {
    if (!existsSync(localExe)) {
      console.error(
        `ERROR: ELOT_VSCODE_EXECUTABLE points to '${localExe}' which does not exist.`,
      );
      process.exit(2);
    }
    console.log(`Using local VS Code binary: ${localExe}`);
    runOpts = { ...baseOpts, vscodeExecutablePath: localExe };
  } else {
    runOpts = { ...baseOpts, version: "stable" };
  }

  try {
    const exitCode = await runTests(runOpts);
    process.exit(exitCode);
  } catch (err) {
    console.error("Failed to run electron tests:", err);
    process.exit(1);
  }
}

main();
