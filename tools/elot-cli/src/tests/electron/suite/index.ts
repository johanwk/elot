// src/tests/electron/suite/index.ts
//
// Step 2.3.5b - Mocha entrypoint loaded by VS Code's test host.
//
// `runTests.ts` launches VS Code with `extensionTestsPath` pointing at
// the compiled version of this file.  VS Code calls `run()`, which
// configures Mocha and discovers all `*.test.js` files in this folder.

import * as path from "path";
import { glob } from "glob";

// `mocha` is a CommonJS module; the default import gives us the class.
// eslint-disable-next-line @typescript-eslint/no-var-requires
const Mocha = require("mocha");

export async function run(): Promise<void> {
  const mocha = new Mocha({
    ui: "tdd",
    color: true,
    timeout: 60000, // VS Code activation + DB load can take a few seconds.
  });

  const testsRoot = __dirname; // dist-test/suite/

  const files = await glob("**/*.test.js", { cwd: testsRoot });
  for (const f of files) {
    mocha.addFile(path.resolve(testsRoot, f));
  }

  return new Promise<void>((resolve, reject) => {
    try {
      mocha.run((failures: number) => {
        if (failures > 0) {
          reject(new Error(`${failures} test(s) failed`));
        } else {
          resolve();
        }
      });
    } catch (err) {
      reject(err);
    }
  });
}
