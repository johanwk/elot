// src/tests/db/dbResolve.test.ts
//
// Step 2.3.1: VS Code DB path resolution.

import { resolveExtensionDbPath, ELOT_DB_FILENAME } from "../../dbResolve.js";
import { join } from "path";

let passed = 0;
let failed = 0;

function test(name: string, fn: () => void): void {
  try {
    fn();
    passed++;
  } catch (err) {
    failed++;
    console.error(`FAIL ${name}:`, err instanceof Error ? err.message : err);
  }
}

function eq<T>(a: T, e: T, msg = ""): void {
  if (a !== e) throw new Error(`${msg}\n  got:      ${a}\n  expected: ${e}`);
}

const STORAGE = "/tmp/elot-storage";
const DEFAULT_PATH = join(STORAGE, ELOT_DB_FILENAME);

test("falls back to globalStorage when nothing is set", () => {
  eq(
    resolveExtensionDbPath({ globalStorageDir: STORAGE }),
    DEFAULT_PATH,
    "default fallback",
  );
});

test("env var overrides default", () => {
  eq(
    resolveExtensionDbPath({
      globalStorageDir: STORAGE,
      envPath: "/srv/elot.sqlite",
    }),
    "/srv/elot.sqlite",
    "env override",
  );
});

test("user setting overrides env", () => {
  eq(
    resolveExtensionDbPath({
      globalStorageDir: STORAGE,
      envPath: "/srv/elot.sqlite",
      userPath: "/u/elot.sqlite",
    }),
    "/u/elot.sqlite",
    "user > env",
  );
});

test("workspace setting overrides user", () => {
  eq(
    resolveExtensionDbPath({
      globalStorageDir: STORAGE,
      envPath: "/srv/elot.sqlite",
      userPath: "/u/elot.sqlite",
      workspacePath: "/w/elot.sqlite",
    }),
    "/w/elot.sqlite",
    "workspace > user > env",
  );
});

test("empty strings are ignored at every layer", () => {
  eq(
    resolveExtensionDbPath({
      globalStorageDir: STORAGE,
      envPath: "",
      userPath: "",
      workspacePath: "",
    }),
    DEFAULT_PATH,
    "empties fall through to default",
  );
});

test("null/undefined are ignored at every layer", () => {
  eq(
    resolveExtensionDbPath({
      globalStorageDir: STORAGE,
      envPath: null,
      userPath: undefined,
      workspacePath: null,
    }),
    DEFAULT_PATH,
    "null/undefined fall through",
  );
});

test("workspace empty falls through to user", () => {
  eq(
    resolveExtensionDbPath({
      globalStorageDir: STORAGE,
      workspacePath: "",
      userPath: "/u/elot.sqlite",
    }),
    "/u/elot.sqlite",
    "empty workspace -> user",
  );
});

console.log(`dbResolve tests: ${passed} passed, ${failed} failed`);
if (failed > 0) process.exit(1);
