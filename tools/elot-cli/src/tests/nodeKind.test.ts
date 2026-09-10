// src/tests/nodeKind.test.ts
//
// Executable statement of the ElotNode shape invariants documented on
// `ElotNodeKind` in src/types.ts.
//
// Every node in a parsed ELOT document must classify as exactly one kind,
// and the optional fields that kind promises must actually be present.
// This is what a Rust port turns into an enum with per-variant payloads
// (Phase 4), so a violation here is a bug that would block that port --
// and, more immediately, a sign that parseOrgWasm has changed shape under
// the generator and the lint checkers.

import { readFileSync } from "fs";
import { resolve } from "path";
import { parseOrg } from "../parseOrgWasm.js";
import { elotNodeKind, ElotNode, ElotNodeKind } from "../types.js";

let passed = 0;
let failed = 0;

function check(label: string, cond: boolean, detail?: string): void {
  if (cond) {
    passed++;
  } else {
    failed++;
    console.log(`  FAIL: ${label}${detail ? ` -- ${detail}` : ""}`);
  }
}

function walk(node: ElotNode, path: string, visit: (n: ElotNode, p: string) => void): void {
  visit(node, path);
  for (const child of node.children ?? []) {
    walk(child, `${path}/${child.title || "(untitled)"}`, visit);
  }
}

function main(): void {
  const orgPath = resolve(__dirname, "../../examples/bfo-core.org");
  const root = parseOrg(readFileSync(orgPath, "utf-8"));

  const counts: Record<ElotNodeKind, number> = {
    root: 0,
    ontology: 0,
    section: 0,
    entity: 0,
  };

  walk(root, "", (n, path) => {
    const kind = elotNodeKind(n);
    counts[kind]++;

    switch (kind) {
      case "root":
        // Only the synthetic level-0 node; it declares nothing itself.
        check("root has no uri", n.uri === undefined, path);
        check("root has no elotContextType", n.elotContextType === undefined, path);
        break;

      case "ontology":
        check("ontology has elotContextType", !!n.elotContextType, path);
        check("ontology has elotContextLocalname", !!n.elotContextLocalname, path);
        check("ontology is not level 0", n.level > 0, path);
        break;

      case "section":
        // A wrapper heading never declares an entity.
        // A wrapper heading may carry `rdfType` -- that is where the
        // ancestor context originates (e.g. ":ID: ...-classes" -> owl:Class),
        // and descendants inherit it.  What it never carries is a `uri`.
        check("section has no uri", n.uri === undefined, path);
        if (n.prefixes !== undefined) {
          check("prefixes only under prefixdefs", n.prefixdefs === true, path);
        }
        break;

      case "entity":
        check("entity has uri", !!n.uri, path);
        check("entity has rdfType", !!n.rdfType, path);
        check("entity has no prefixdefs", n.prefixdefs !== true, path);
        check("entity is not level 0", n.level > 0, path);
        break;
    }
  });

  // The fixture must actually exercise every variant, otherwise the
  // invariants above are vacuously true.
  check("saw exactly one root", counts.root === 1, `got ${counts.root}`);
  check("saw at least one ontology", counts.ontology >= 1, `got ${counts.ontology}`);
  check("saw at least one section", counts.section >= 1, `got ${counts.section}`);
  check("saw many entities", counts.entity > 10, `got ${counts.entity}`);

  console.log(
    `  kinds: root=${counts.root} ontology=${counts.ontology} ` +
      `section=${counts.section} entity=${counts.entity}`
  );
  console.log(`\nnodeKind tests: ${passed} passed, ${failed} failed`);
  process.exitCode = failed > 0 ? 1 : 0;
}

main();
