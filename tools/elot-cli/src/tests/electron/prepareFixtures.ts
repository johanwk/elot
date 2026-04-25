// src/tests/electron/prepareFixtures.ts
//
// Step 2.3.5b - Build the fixture workspace used by the electron tests.
//
// Run via tsx as a *separate* process before the electron test host
// starts (see package.json `pretest:electron`).  Keeping it out of the
// electron compile graph avoids forcing the rest of the source tree
// through the test tsconfig.
//
// Layout produced under <extensionRoot>/dist-test/fixtures/workspace/:
//
//   .vscode/settings.json   -- elot.dbPath + elot.activeLabelSources
//   sample.txt              -- a non-Org file with known CURIEs/IRIs
//   elot.sqlite             -- pre-seeded DB (one source, two entities)

import { mkdirSync, rmSync, writeFileSync, existsSync } from "fs";
import * as path from "path";
import { ElotDb, EntityTriple } from "../../db/sqljs.js";

const SAMPLE_TEXT = `\
ELOT electron-test fixture
==========================

This file references the same identifier in CURIE form and full-IRI form
to exercise the global label-display hover and decoration paths.

  CURIE form:    ex:Widget
  Full IRI:      <http://example.org/ex/Widget>
  CURIE (other): ex:Gadget
  Unknown id:    ex:NotInDB

End of fixture.
`;

async function main(): Promise<void> {
  // Invoked from the package root via `tsx src/tests/electron/prepareFixtures.ts`,
  // so cwd == the extension root.
  const extensionRoot = process.cwd();
  const fixturesRoot = path.join(extensionRoot, "dist-test", "fixtures");
  const workspacePath = path.join(fixturesRoot, "workspace");
  const vscodeDir = path.join(workspacePath, ".vscode");
  const dbPath = path.join(workspacePath, "elot.sqlite");
  const samplePath = path.join(workspacePath, "sample.txt");

  if (existsSync(workspacePath)) {
    rmSync(workspacePath, { recursive: true, force: true });
  }
  mkdirSync(vscodeDir, { recursive: true });

  const db = await ElotDb.open(dbPath);
  const entries: EntityTriple[] = [
    {
      id: "http://example.org/ex/Widget",
      label: "Widget",
      kind: "Class",
      attrs: [
        ["rdfs:label", { value: "Widget", lang: "en" }],
        ["rdf:type", { value: "owl:Class", lang: "" }],
        ["skos:definition", { value: "A test widget.", lang: "en" }],
      ],
    },
    {
      id: "http://example.org/ex/Gadget",
      label: "Gadget",
      kind: "Class",
      attrs: [
        ["rdfs:label", { value: "Gadget", lang: "en" }],
        ["rdf:type", { value: "owl:Class", lang: "" }],
      ],
    },
  ];
  db.updateSource("ex", "", "json", entries);
  db.addPrefix("ex", "", "ex", "http://example.org/ex/");
  db.save();

  const settings = {
    "elot.dbPath": dbPath,
    "elot.activeLabelSources": [{ source: "ex", dataSource: "" }],
    "elot.preferredLanguages": ["en"],
    "elot.globalLabelDisplay.hoverEnabled": true,
    "elot.globalLabelDisplay.includeLanguages": ["plaintext"],
  };
  writeFileSync(
    path.join(vscodeDir, "settings.json"),
    JSON.stringify(settings, null, 2),
    "utf-8",
  );

  writeFileSync(samplePath, SAMPLE_TEXT, "utf-8");

  // Print the absolute path on stdout so callers can capture it.
  console.log(workspacePath);
}

main().catch((err) => {
  console.error("prepareFixtures failed:", err);
  process.exit(1);
});
