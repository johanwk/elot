// src/tests/exportHtml.test.ts
//
// Tests for preprocessOrgForLinks (Step 1: slurp-based CURIE map).

import { preprocessOrgForLinks } from "../exportHtml.js";

function assert(condition: boolean, msg: string) {
  if (!condition) {
    console.error(`FAIL: ${msg}`);
    process.exit(1);
  }
}

function main() {
  let passed = 0;

  // ── Test 1: CUSTOM_ID injection and CURIE→label from slurp ──────
  {
    const orgText = `#+title: Test Ontology

* Ontology

** Prefix declarations

#+name: prefixes
| prefix | uri                          |
|--------+------------------------------|
| ex:    | http://example.org/ontology/ |

** Class hierarchy

*** ex:Person person
:PROPERTIES:
:rdf:type: owl:Class
:END:
- rdfs:label :: "person"@en

*** ex:Animal animal
- rdfs:label :: "animal"@en

** Object property hierarchy

*** ex:hasPet has pet
- rdfs:label :: "has pet"@en
- rdfs:domain :: ex:Person
- rdfs:range :: ex:Animal
`;

    const result = preprocessOrgForLinks(orgText);

    // Check that CUSTOM_ID was injected for ex:Person (already has drawer)
    assert(
      result.includes(":CUSTOM_ID: ex:Person"),
      "Should inject CUSTOM_ID for ex:Person"
    );

    // Check that CUSTOM_ID was injected for ex:Animal (no existing drawer)
    assert(
      result.includes(":CUSTOM_ID: ex:Animal"),
      "Should inject CUSTOM_ID for ex:Animal"
    );

    // Check that CUSTOM_ID was injected for ex:hasPet
    assert(
      result.includes(":CUSTOM_ID: ex:hasPet"),
      "Should inject CUSTOM_ID for ex:hasPet"
    );

    // Check that the slurp map found the right labels
    // (ex:Person in the domain value should get linkified with label)
    assert(
      result.includes("[[#ex:Person][person]]") || result.includes("[[#ex:Person]"),
      "ex:Person in description value should be linkified"
    );

    console.log("  Test 1 passed: CUSTOM_ID injection and slurp map");
    passed++;
  }

  // ── Test 2: No CURIEs → pass-through ─────────────────────────────
  {
    const orgText = `#+title: Plain doc

* Section one
Some text.
`;
    const result = preprocessOrgForLinks(orgText);
    assert(result === orgText, "No CURIEs should return text unchanged");
    console.log("  Test 2 passed: pass-through for non-ELOT files");
    passed++;
  }

  // ── Test 3: Existing CUSTOM_ID not duplicated ─────────────────────
  {
    const orgText = `#+title: Test

* Ontology

** Prefix declarations

#+name: prefixes
| prefix | uri                          |
|--------+------------------------------|
| ex:    | http://example.org/ontology/ |

** Class hierarchy

*** ex:Foo foo
:PROPERTIES:
:CUSTOM_ID: ex:Foo
:rdf:type: owl:Class
:END:
- rdfs:label :: "foo"@en
`;

    const result = preprocessOrgForLinks(orgText);
    // Count occurrences of CUSTOM_ID: ex:Foo
    const matches = result.match(/:CUSTOM_ID: ex:Foo/g);
    assert(
      matches !== null && matches.length === 1,
      `Should not duplicate CUSTOM_ID (found ${matches?.length ?? 0})`
    );
    console.log("  Test 3 passed: no duplicate CUSTOM_ID");
    passed++;
  }

  // ── Test 4: Block exclusion — CURIEs inside src/example blocks untouched ──
  {
    const orgText = `#+title: Test

* Ontology

** Prefix declarations

#+name: prefixes
| prefix | uri                          |
|--------+------------------------------|
| ex:    | http://example.org/ontology/ |

** Class hierarchy

*** ex:Foo foo
:PROPERTIES:
:rdf:type: owl:Class
:END:
- rdfs:label :: "foo"@en

*** ex:Bar bar
- rdfs:label :: "bar"@en

** Notes

Here we mention ex:Foo in body text.

#+begin_src ttl
ex:Foo a owl:Class .
#+end_src

#+begin_example
ex:Bar is just an example
#+end_example

: ex:Foo fixed-width line

After the blocks, ex:Bar appears again.
`;

    const result = preprocessOrgForLinks(orgText);
    const lines = result.split("\n");

    // Inside src block: ex:Foo should NOT be linkified
    const srcLine = lines.find(l => l.includes("ex:Foo a owl:Class"));
    assert(
      srcLine !== undefined && !srcLine.includes("[[#"),
      "CURIEs inside src block should not be linkified"
    );

    // Inside example block: ex:Bar should NOT be linkified
    const exLine = lines.find(l => l.includes("ex:Bar is just an example"));
    assert(
      exLine !== undefined && !exLine.includes("[[#"),
      "CURIEs inside example block should not be linkified"
    );

    // Fixed-width line: ex:Foo should NOT be linkified
    const fwLine = lines.find(l => l.startsWith(": ex:Foo"));
    assert(
      fwLine !== undefined && !fwLine.includes("[[#"),
      "CURIEs in fixed-width lines should not be linkified"
    );

    // Body text outside blocks: should be linkified (Step 3)
    const bodyLine = lines.find(l => l.includes("Here we mention"));
    assert(
      bodyLine !== undefined && bodyLine.includes("[[#ex:Foo][foo]]"),
      "CURIEs in plain body text should be linkified"
    );

    const afterLine = lines.find(l => l.includes("After the blocks"));
    assert(
      afterLine !== undefined && afterLine.includes("[[#ex:Bar][bar]]"),
      "CURIEs in body text after blocks should be linkified"
    );

    console.log("  Test 4 passed: block exclusion zones");
    passed++;
  }

  // ── Test 5: #+begin_src/#+end_src case insensitivity ──────────────
  {
    const orgText = `#+title: Test

* Ontology

** Prefix declarations

#+name: prefixes
| prefix | uri                          |
|--------+------------------------------|
| ex:    | http://example.org/ontology/ |

** Class hierarchy

*** ex:Baz baz
- rdfs:label :: "baz"@en

** Notes

#+BEGIN_SRC ttl
ex:Baz should stay
#+END_SRC

ex:Baz should be linked.
`;

    const result = preprocessOrgForLinks(orgText);
    const lines = result.split("\n");

    const insideLine = lines.find(l => l.includes("ex:Baz should stay"));
    assert(
      insideLine !== undefined && !insideLine.includes("[[#"),
      "Case-insensitive BEGIN_SRC should exclude content"
    );

    const outsideLine = lines.find(l => l.includes("should be linked"));
    assert(
      outsideLine !== undefined && outsideLine.includes("[[#ex:Baz][baz]]"),
      "Plain body text should be linkified"
    );

    console.log("  Test 5 passed: case-insensitive block markers");
    passed++;
  }

  // ── Test 6: Body text linkification — full coverage ─────────────
  {
    const orgText = `#+title: Test

* Ontology

** Prefix declarations

#+name: prefixes
| prefix | uri                          |
|--------+------------------------------|
| ex:    | http://example.org/ontology/ |

** Class hierarchy

*** ex:Alpha alpha
- rdfs:label :: "alpha"@en

*** ex:Beta beta
- rdfs:label :: "beta"@en
- rdfs:subClassOf :: ex:Alpha

** Notes

In prose, ex:Alpha and ex:Beta appear as links.

Already linked: [[#ex:Alpha][alpha]] should not be double-linked.

- description tag ex:Alpha :: value has ex:Beta here
`;

    const result = preprocessOrgForLinks(orgText);
    const lines = result.split("\n");

    // Plain body text linkified
    const proseLine = lines.find(l => l.startsWith("In prose"));
    assert(
      proseLine !== undefined &&
        proseLine.includes("[[#ex:Alpha][alpha]]") &&
        proseLine.includes("[[#ex:Beta][beta]]"),
      "Both CURIEs in prose should be linkified"
    );

    // Already-linked CURIE not double-wrapped
    const alreadyLine = lines.find(l => l.includes("Already linked"));
    assert(
      alreadyLine !== undefined &&
        !alreadyLine.includes("[[#ex:Alpha][alpha]]]"),
      "Already-linked CURIEs should not be double-linked"
    );
    // Verify the existing link is preserved
    assert(
      alreadyLine !== undefined && alreadyLine.includes("[[#ex:Alpha][alpha]]"),
      "Existing link should be preserved"
    );

    // Description list: CURIE tag should be linkified, non-CURIE tag not
    const dlLine = lines.find(l => l.includes("description tag"));
    assert(
      dlLine !== undefined,
      "Description list line should exist"
    );
    // The value part (after " :: ") should have ex:Beta linkified
    const dlVal = dlLine!.substring(dlLine!.indexOf(" :: ") + 4);
    assert(
      dlVal.includes("[[#ex:Beta][beta]]"),
      "Description list value should have CURIEs linkified"
    );

    // rdfs:subClassOf tag should be linkified.
    // rdfs:subClassOf is NOT declared in this test ontology, so the CURIE
    // itself is used as the link label (no label in curieToLabel).
    const subLine = lines.find(l => l.includes("rdfs:subClassOf"));
    assert(
      subLine !== undefined && subLine.includes("[[#rdfs:subClassOf][rdfs:subClassOf]]"),
      "Undeclared CURIE description list tag should be linkified with CURIE as label"
    );
    // rdfs:subClassOf value linkified
    assert(
      subLine !== undefined && subLine.includes("[[#ex:Alpha][alpha]]"),
      "rdfs:subClassOf value should be linkified"
    );

    console.log("  Test 6 passed: full body text linkification");
    passed++;
  }

  // ── Test 7: CURIEs in headings are NOT linkified ──────────────────
  {
    const orgText = `#+title: Test

* Ontology

** Prefix declarations

#+name: prefixes
| prefix | uri                          |
|--------+------------------------------|
| ex:    | http://example.org/ontology/ |

** Class hierarchy

*** ex:Gamma gamma
- rdfs:label :: "gamma"@en
`;

    const result = preprocessOrgForLinks(orgText);
    const headingLine = result.split("\n").find(l => l.startsWith("*** ex:Gamma"));
    assert(
      headingLine !== undefined && !headingLine.includes("[[#"),
      "Headings should not have CURIEs linkified"
    );

    console.log("  Test 7 passed: headings not linkified");
    passed++;
  }

  // ── Test 8: Bare prefix expansion in description-list values ─────
  {
    const orgText = `#+title: Test

* Ontology

** Prefix declarations
:PROPERTIES:
:prefixdefs: yes
:END:

#+name: prefixes
| prefix | uri                          |
|--------+------------------------------|
| ex:    | http://example.org/ontology/ |
| owl:   | http://www.w3.org/2002/07/owl# |

** Class hierarchy

*** ex:Thing thing
- rdfs:label :: "thing"@en
- Ontology IRI :: ex:
- Also uses :: owl:
- Not a prefix :: some text here
`;

    const result = preprocessOrgForLinks(orgText);
    const lines = result.split("\n");

    // "Ontology IRI :: ex:" should become "Ontology IRI :: http://example.org/ontology/"
    const iriLine = lines.find(l => l.includes("Ontology IRI"));
    assert(
      iriLine !== undefined && iriLine.includes("http://example.org/ontology/"),
      `Bare prefix 'ex:' should be expanded to full URI (got: ${iriLine})`
    );
    assert(
      iriLine !== undefined && !iriLine.includes("ex:"),
      "Bare prefix should be fully replaced"
    );

    // "Also uses :: owl:" should expand too
    const owlLine = lines.find(l => l.includes("Also uses"));
    assert(
      owlLine !== undefined && owlLine.includes("http://www.w3.org/2002/07/owl#"),
      `Bare prefix 'owl:' should be expanded (got: ${owlLine})`
    );

    // "Not a prefix :: some text here" should be unchanged
    const plainLine = lines.find(l => l.includes("Not a prefix"));
    assert(
      plainLine !== undefined && plainLine.includes("some text here"),
      "Non-prefix description values should be unchanged"
    );

    console.log("  Test 8 passed: bare prefix expansion");
    passed++;
  }

  console.log(`\nAll ${passed} exportHtml tests passed.`);
}

main();
