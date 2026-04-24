// src/tests/db/dbPaths.test.ts
//
// Step 2.2.2: unit tests for the DB path resolver.  Pure helpers;
// no filesystem access, so we exercise all three platforms and the
// $ELOT_DB_PATH override in a single run.
//
// Run with:  npx tsx src/tests/db/dbPaths.test.ts

import { homedir } from "os";
import { join } from "path";
import {
  resolveDefaultDbPath,
  vscodeGlobalStorageDir,
} from "../../dbPaths.js";

let passed = 0;
let failed = 0;

function eq<T>(name: string, actual: T, expected: T): void {
  if (JSON.stringify(actual) === JSON.stringify(expected)) {
    passed++;
  } else {
    failed++;
    console.error(
      `FAIL: ${name}\n  expected: ${JSON.stringify(expected)}\n  actual:   ${JSON.stringify(actual)}`,
    );
  }
}

function check(name: string, cond: boolean): void {
  if (cond) passed++;
  else {
    failed++;
    console.error(`FAIL: ${name}`);
  }
}

// Windows
eq(
  "win32 globalStorage with APPDATA",
  vscodeGlobalStorageDir({ APPDATA: "C:\\Users\\u\\AppData\\Roaming" }, "win32"),
  join(
    "C:\\Users\\u\\AppData\\Roaming",
    "Code",
    "User",
    "globalStorage",
    "johanwk.elot",
  ),
);

// macOS
const darwin = vscodeGlobalStorageDir({}, "darwin");
check(
  "darwin globalStorage path contains Application Support",
  darwin.includes("Application Support"),
);
check(
  "darwin globalStorage ends with johanwk.elot",
  darwin.endsWith(join("globalStorage", "johanwk.elot")),
);

// Linux
const linux = vscodeGlobalStorageDir({}, "linux");
check(
  "linux globalStorage includes .config/Code",
  linux.includes(join(".config", "Code")),
);

// ELOT_DB_PATH override
eq(
  "ELOT_DB_PATH overrides",
  resolveDefaultDbPath({ ELOT_DB_PATH: "/custom/elot.db" }, "linux"),
  "/custom/elot.db",
);

// No env -> computed default
const def = resolveDefaultDbPath({}, "linux");
check("default path nonempty", def.length > 0);
check("default starts at homedir", def.startsWith(homedir()));
check("default ends with elot.sqlite", def.endsWith("elot.sqlite"));

console.log(`dbPaths tests: ${passed} passed, ${failed} failed`);
if (failed > 0) process.exit(1);
