// src/tests/settings/cliRunner.test.ts
//
// Step 2.3.6: pure tests for the CLI invocation builders.  No
// vscode, no spawn -- just argv-construction parity.

import {
  buildRegisterInvocation,
  buildRefreshInvocation,
  defaultSourceNameFromFile,
} from "../../cliRunner.js";

let pass = 0;
let fail = 0;

function t(name: string, fn: () => void): void {
  try {
    fn();
    pass++;
  } catch (e) {
    fail++;
    console.error(`FAIL ${name}: ${(e as Error).message}`);
  }
}

function eq<T>(a: T, b: T, msg = ""): void {
  const aj = JSON.stringify(a);
  const bj = JSON.stringify(b);
  if (aj !== bj) throw new Error(`${msg}\n  got:      ${aj}\n  expected: ${bj}`);
}

t("register: minimal", () => {
  const inv = buildRegisterInvocation("elot-cli", {
    file: "/a/b/labels.csv",
    source: "labels",
  });
  eq(inv.args, ["db", "register", "/a/b/labels.csv", "--source", "labels"]);
});

t("register: type + dataSource + dbPath", () => {
  const inv = buildRegisterInvocation("elot-cli", {
    file: "q.rq",
    source: "remote",
    type: "rq",
    dataSource: "https://example.org/sparql",
    dbPath: "/tmp/elot.sqlite",
  });
  eq(inv.args, [
    "db",
    "register",
    "q.rq",
    "--source",
    "remote",
    "--type",
    "rq",
    "--data-source",
    "https://example.org/sparql",
    "--db",
    "/tmp/elot.sqlite",
  ]);
});

t("register: empty/null dataSource & dbPath omitted", () => {
  const inv = buildRegisterInvocation("elot-cli", {
    file: "x.csv",
    source: "x",
    type: null,
    dataSource: null,
    dbPath: null,
  });
  eq(inv.args, ["db", "register", "x.csv", "--source", "x"]);
});

t("register: display quotes whitespace", () => {
  const inv = buildRegisterInvocation("elot-cli", {
    file: "/a b/c.csv",
    source: "my src",
  });
  // exact rendering not asserted, but must include quoted forms
  if (!inv.display.includes(`"/a b/c.csv"`)) {
    throw new Error(`display lacks quoted file: ${inv.display}`);
  }
  if (!inv.display.includes(`"my src"`)) {
    throw new Error(`display lacks quoted source: ${inv.display}`);
  }
});

t("refresh: minimal", () => {
  const inv = buildRefreshInvocation("elot-cli", { source: "bfo" });
  eq(inv.args, ["db", "refresh", "bfo"]);
});

t("refresh: with data-source + db", () => {
  const inv = buildRefreshInvocation("elot-cli", {
    source: "bfo",
    dataSource: "/o/bfo.ttl",
    dbPath: "/tmp/x.sqlite",
  });
  eq(inv.args, [
    "db",
    "refresh",
    "bfo",
    "--data-source",
    "/o/bfo.ttl",
    "--db",
    "/tmp/x.sqlite",
  ]);
});

t("defaultSourceNameFromFile: posix", () => {
  eq(defaultSourceNameFromFile("/a/b/labels.csv"), "labels");
});

t("defaultSourceNameFromFile: windows", () => {
  eq(defaultSourceNameFromFile("C:\\data\\some.query.rq"), "some.query");
});

t("defaultSourceNameFromFile: dotfile / no ext", () => {
  eq(defaultSourceNameFromFile("foo"), "foo");
  eq(defaultSourceNameFromFile(".hidden"), ".hidden");
});

console.log(`cliRunner tests: ${pass} passed, ${fail} failed`);
if (fail > 0) process.exit(1);
