// src/tests/exportHtmlIntegration.test.ts
//
// Step 6: Integration test for ELOT HTML export.
//
// - Tests preprocessOrgForLinks against a realistic ELOT Org file
//   (structure modelled on actual exported ontologies like BFO / IDO)
// - If Pandoc is available on PATH, also runs the full export pipeline
//   and checks the HTML output for expected features.

import { preprocessOrgForLinks, findPandoc, exportOrgToHtml } from "../exportHtml.js";
import { writeFileSync, readFileSync, unlinkSync, existsSync } from "fs";
import { join } from "path";
import * as os from "os";

function assert(condition: boolean, msg: string) {
  if (!condition) {
    console.error(`FAIL: ${msg}`);
    process.exit(1);
  }
}

// ── Realistic ELOT Org text ─────────────────────────────────────────
//
// Follows the structure of real exported ontologies:
// - Top-level heading = ontology context with :ID:, :ELOT-context-type:
// - ** Prefixes with :prefixdefs: yes
// - ** Classes with :resourcedefs: yes, :ID: ending in -class-hierarchy
// - *** entity headings: "label (curie)" or "curie" forms
// - Description lists with rdfs:label, rdfs:subClassOf, etc.
// - **** sub-class headings at deeper levels

const REALISTIC_ORG = `#+title: Test Ontology
#+subtitle: An OWL ontology
#+author: Test Author
#+date: 2025-01-01

* test-ont
:PROPERTIES:
:ID:       test-ont
:ELOT-context-type: ontology
:ELOT-context-localname: test-ont
:ELOT-default-prefix: test-ont
:header-args:omn: :tangle ./test.omn :noweb yes
:END:

** Prefixes
:PROPERTIES:
:prefixdefs: yes
:END:
The ontology document in OWL employs the namespace prefixes of table [[prefix-table]].

#+name: prefix-table
| prefix | uri |
|--------+-----|
| : | http://example.org/test-ont/ |
| ex: | http://example.org/ontology/ |
| owl: | http://www.w3.org/2002/07/owl# |
| rdfs: | http://www.w3.org/2000/01/rdf-schema# |
| xsd: | http://www.w3.org/2001/XMLSchema# |

** test-ont ontology (http://example.org/test-ont)
:PROPERTIES:
:ID:       test-ont-ontology-declaration
:custom_id: test-ont-ontology-declaration
:resourcedefs: yes
:END:

 - Ontology IRI :: ex:
 - rdfs:label :: Test Ontology

** Classes
:PROPERTIES:
:ID:       test-ont-class-hierarchy
:custom_id: test-ont-class-hierarchy
:resourcedefs: yes
:END:

*** Disjointness clauses                                          :nodeclare:
 - DisjointClasses :: ex:Vehicle, ex:Animal

*** Vehicle (ex:Vehicle)
 - rdfs:label :: "vehicle"@en
 - rdfs:comment :: A means of transport.
 - skos:example :: A car, a bicycle, a train.
**** Car (ex:Car)
 - rdfs:label :: "car"@en
 - rdfs:subClassOf :: ex:Vehicle
 - rdfs:comment :: A four-wheeled motor vehicle.
**** Bicycle (ex:Bicycle)
 - rdfs:label :: "bicycle"@en
 - rdfs:subClassOf :: ex:Vehicle

*** Animal (ex:Animal)
 - rdfs:label :: "animal"@en
 - rdfs:comment :: A living organism.
**** Dog (ex:Dog)
 - rdfs:label :: "dog"@en
 - rdfs:subClassOf :: ex:Animal

** Object properties
:PROPERTIES:
:ID:       test-ont-object-property-hierarchy
:custom_id: test-ont-object-property-hierarchy
:resourcedefs: yes
:END:

*** owns (ex:owns)
 - rdfs:label :: "owns"@en
 - rdfs:domain :: ex:Animal
 - rdfs:range :: ex:Vehicle

** Notes

This section discusses the relationship between ex:Animal and ex:Vehicle.
In particular, ex:Dog is a subclass of ex:Animal, and ex:Car is a subclass of ex:Vehicle.

An ex:Animal can ex:owns a ex:Vehicle.

Here is an example in Turtle:

#+begin_src ttl
ex:fido a ex:Dog .
ex:fido ex:owns ex:myCar .
ex:myCar a ex:Car .
#+end_src

And a fixed-width example:

: ex:Vehicle rdfs:subClassOf owl:Thing .

Already linked: [[#ex:Dog][Dog]] should not be double-linked.
`;

function main() {
  let passed = 0;

  // ── Test A: Full preprocessing on realistic ELOT Org ───────────────
  {
    const result = preprocessOrgForLinks(REALISTIC_ORG);
    const lines = result.split("\n");

    // A1: CUSTOM_ID injected for entities
    assert(
      result.includes(":CUSTOM_ID: ex:Vehicle"),
      "A1a: CUSTOM_ID for ex:Vehicle"
    );
    assert(
      result.includes(":CUSTOM_ID: ex:Car"),
      "A1b: CUSTOM_ID for ex:Car"
    );
    assert(
      result.includes(":CUSTOM_ID: ex:Dog"),
      "A1c: CUSTOM_ID for ex:Dog"
    );
    assert(
      result.includes(":CUSTOM_ID: ex:owns"),
      "A1d: CUSTOM_ID for ex:owns"
    );
    assert(
      result.includes(":CUSTOM_ID: ex:Bicycle"),
      "A1e: CUSTOM_ID for ex:Bicycle"
    );

    // A2: No CUSTOM_ID for :nodeclare: headings
    const disjLine = lines.find(l => l.includes("Disjointness clauses"));
    assert(
      disjLine !== undefined && !result.includes(":CUSTOM_ID: Disjointness"),
      "A2: No CUSTOM_ID for :nodeclare: heading"
    );

    // A3: Labels from rdfs:label used (not heading text)
    // rdfs:subClassOf :: ex:Vehicle → should become [[#ex:Vehicle][Vehicle]]
    const subLine = lines.find(l => l.includes("rdfs:subClassOf") && l.includes("Vehicle"));
    assert(
      subLine !== undefined && subLine.includes("[[#ex:Vehicle][Vehicle]]"),
      `A3a: rdfs:subClassOf value should use label 'Vehicle' (got: ${subLine})`
    );

    // rdfs:domain :: ex:Animal → [[#ex:Animal][animal]]
    const domainLine = lines.find(l => l.includes("rdfs:domain"));
    assert(
      domainLine !== undefined && domainLine.includes("[[#ex:Animal][Animal]]"),
      `A3b: rdfs:domain value should use label 'Animal' (got: ${domainLine})`
    );

    // rdfs:range :: ex:Vehicle → [[#ex:Vehicle][Vehicle]]
    const rangeLine = lines.find(l => l.includes("rdfs:range"));
    assert(
      rangeLine !== undefined && rangeLine.includes("[[#ex:Vehicle][Vehicle]]"),
      `A3c: rdfs:range value should use label 'Vehicle' (got: ${rangeLine})`
    );

    // A4: Body text linkification
    const bodyLine1 = lines.find(l => l.includes("discusses the relationship"));
    assert(
      bodyLine1 !== undefined &&
        bodyLine1.includes("[[#ex:Animal][Animal]]") &&
        bodyLine1.includes("[[#ex:Vehicle][Vehicle]]"),
      `A4a: Body text CURIEs should be linkified (got: ${bodyLine1})`
    );

    const bodyLine2 = lines.find(l => l.includes("In particular"));
    assert(
      bodyLine2 !== undefined &&
        bodyLine2.includes("[[#ex:Dog][Dog]]") &&
        bodyLine2.includes("[[#ex:Car][Car]]"),
      `A4b: More body text CURIEs linkified (got: ${bodyLine2})`
    );

    // A5: Src block content NOT linkified
    const srcLine = lines.find(l => l.includes("ex:fido a ex:Dog"));
    assert(
      srcLine !== undefined && !srcLine.includes("[[#"),
      "A5: CURIEs inside src block should not be linkified"
    );

    // A6: Fixed-width line NOT linkified
    const fwLine = lines.find(l => l.startsWith(": ex:Vehicle"));
    assert(
      fwLine !== undefined && !fwLine.includes("[[#"),
      "A6: CURIEs in fixed-width lines should not be linkified"
    );

    // A7: Already-linked CURIE not double-wrapped
    const alreadyLine = lines.find(l => l.includes("Already linked"));
    assert(
      alreadyLine !== undefined &&
        alreadyLine.includes("[[#ex:Dog][Dog]]") &&
        !alreadyLine.includes("[[#ex:Dog][Dog]]]"),
      "A7: Already-linked CURIEs should not be double-linked"
    );

    // A8: Bare prefix expansion
    const ontIriLine = lines.find(l => l.includes("Ontology IRI"));
    assert(
      ontIriLine !== undefined && ontIriLine.includes("http://example.org/ontology/"),
      `A8: Bare prefix 'ex:' should be expanded (got: ${ontIriLine})`
    );

    // A9: Description list CURIE tags ARE linkified
    // "- rdfs:domain :: ..." — the tag "rdfs:domain" should become a link
    assert(
      domainLine !== undefined && domainLine.includes("[[#rdfs:domain][rdfs:domain]]"),
      `A9: CURIE description list tags should be linkified (got: ${domainLine})`
    );

    // A10: Headings NOT linkified
    const vehicleHeading = lines.find(l => l.match(/^\*+\s.*Vehicle.*ex:Vehicle/));
    assert(
      vehicleHeading !== undefined && !vehicleHeading.includes("[[#"),
      "A10: Heading text should not be linkified"
    );

    // A11: DisjointClasses values linkified
    const disjValLine = lines.find(l => l.includes("DisjointClasses"));
    assert(
      disjValLine !== undefined &&
        disjValLine.includes("[[#ex:Vehicle][Vehicle]]") &&
        disjValLine.includes("[[#ex:Animal][Animal]]"),
      `A11: DisjointClasses values should be linkified (got: ${disjValLine})`
    );

    console.log("  Test A passed: full preprocessing on realistic ELOT Org (11 checks)");
    passed++;
  }

  // ── Test B: Pandoc end-to-end (if available) ───────────────────────
  {
    const pandocPath = findPandoc();
    if (!pandocPath) {
      console.log("  Test B skipped: Pandoc not found on PATH");
    } else {
      const tmpDir = os.tmpdir();
      const inputPath = join(tmpDir, `elot-integ-test-${Date.now()}.org`);
      const outputPath = inputPath.replace(/\.org$/, ".html");

      try {
        writeFileSync(inputPath, REALISTIC_ORG, "utf-8");

        // Run the full export pipeline synchronously via a promise
        const runExport = async () => {
          const result = await exportOrgToHtml(inputPath, pandocPath, outputPath);
          return result;
        };

        runExport().then((htmlPath) => {
          const html = readFileSync(htmlPath, "utf-8");

          // B1: HTML has id= attributes on headings
          assert(
            html.includes('id="ex:Vehicle"') || html.includes('id="exVehicle"'),
            "B1: Heading should have id attribute for ex:Vehicle"
          );

          // B2: Links are clickable (both from body text and from description tags)
          assert(
            html.includes('href="#ex:Vehicle"') || html.includes('href="#exVehicle"'),
            "B2: Body text should contain link to ex:Vehicle"
          );

          // B2b: Description list CURIE tags become links
          assert(
            html.includes('href="#rdfs:subClassOf"') || html.includes('href="#rdfssubClassOf"'),
            "B2b: CURIE description list tags should become links in HTML"
          );

          // B3: Labels appear in link text
          assert(
            html.includes(">Vehicle<") || html.includes(">Vehicle</a>"),
            "B3: Link text should show label 'Vehicle'"
          );

          // B4: TOC exists wrapped in table-of-contents div
          assert(
            html.includes('id="table-of-contents"'),
            "B4: TOC should be wrapped in div#table-of-contents"
          );

          // B5: ELOT CSS is loaded
          assert(
            html.includes("elot-style.css"),
            "B5: ELOT CSS should be loaded"
          );

          // B6: ELOT nav JS is loaded
          assert(
            html.includes("elot-nav.js"),
            "B6: ELOT nav JS should be loaded"
          );

          // B7: Title appears
          assert(
            html.includes("Test Ontology"),
            "B7: Title should appear in HTML"
          );

          // B8: Src block content is in <code> (not linkified)
          assert(
            html.includes("ex:fido") && !html.includes('href="#ex:fido"'),
            "B8: Src block content should not be linkified in HTML"
          );

          // B9: Prefix expanded in HTML output
          assert(
            html.includes("http://example.org/ontology/"),
            "B9: Bare prefix should be expanded in HTML"
          );

          console.log("  Test B passed: Pandoc end-to-end HTML export (9 checks)");

          // Cleanup
          try { unlinkSync(inputPath); } catch {}
          try { unlinkSync(htmlPath); } catch {}

          console.log(`\nAll integration tests passed.`);
        }).catch((err) => {
          console.error(`FAIL: Pandoc export error: ${err.message}`);
          try { unlinkSync(inputPath); } catch {}
          process.exit(1);
        });

        return; // async path handles the final output
      } catch (err: any) {
        console.error(`FAIL: ${err.message}`);
        try { unlinkSync(inputPath); } catch {}
        process.exit(1);
      }
    }

    if (!findPandoc()) {
      console.log(`\nAll ${passed} integration tests passed (Pandoc tests skipped).`);
    }
  }
}

main();
