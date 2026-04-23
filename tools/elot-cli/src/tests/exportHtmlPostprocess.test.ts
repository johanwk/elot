// src/tests/exportHtmlPostprocess.test.ts
//
// Tests for HTML post-processing: outline divs, section numbers,
// deep heading conversion, org-dl classes, <p> unwrapping in <dd>.

import { wrapOutlineDivs } from "../exportHtml.js";

function assert(condition: boolean, msg: string) {
  if (!condition) {
    console.error(`FAIL: ${msg}`);
    process.exit(1);
  }
}

function main() {
  let passed = 0;

  // ── Test 1: Deep headings converted from <p class="heading"> to <hN> ──
  {
    // Simulate what exportOrgToHtml does before wrapOutlineDivs:
    // Pandoc produces <p class="heading"> for headings deeper than h6.
    // The exporter converts these to <h7>, <h8>, etc.
    // We test the regex used in exportOrgToHtml.
    const headingIdToLevel = new Map<string, number>([
      ["lis:Capability", 6],  // Org level 6 → h7 (shift by 1)
      ["lis:DeepThing", 7],   // Org level 7 → h8
    ]);

    let html = `<div id="content">
<p class="heading" data-number="1.4.3.1.1.1" id="lis:Capability"><span
class="header-section-number">1.4.3.1.1.1</span> Capability
(lis:Capability)</p>
<dl class="org-dl">
<dt>rdfs:isDefinedBy</dt>
<dd>lis-ont:core</dd>
</dl>
<p class="heading" data-number="1.4.3.1.1.1.1" id="lis:DeepThing"><span
class="header-section-number">1.4.3.1.1.1.1</span> DeepThing</p>
<p>Some content.</p>
</div>
<script src="elot-nav.js"></script>`;

    // Apply the same regex as exportOrgToHtml
    html = html.replace(
      /<p class="heading"[^>]*?\bid="([^"]+)"[^>]*>(.*?)<\/p>/gs,
      (_m, id, content) => {
        const orgLevel = headingIdToLevel.get(id);
        const hLevel = orgLevel ? orgLevel + 1 : 7;
        return `<h${hLevel} id="${id}">${content}</h${hLevel}>`;
      }
    );

    assert(
      html.includes('<h7 id="lis:Capability">'),
      "T1a: <p class='heading'> should become <h7> for Org level 6"
    );
    assert(
      !html.includes('<p class="heading"'),
      "T1b: No <p class='heading'> should remain"
    );
    assert(
      html.includes('<h8 id="lis:DeepThing">'),
      "T1c: <p class='heading'> should become <h8> for Org level 7"
    );
    assert(
      html.includes('</h7>') && html.includes('</h8>'),
      "T1d: Closing tags should match opening tags"
    );

    console.log("  Test 1 passed: deep headings converted from <p> to <hN>");
    passed++;
  }

  // ── Test 2: wrapOutlineDivs adds outline container divs ────────────
  {
    const html = `<!DOCTYPE html>
<html><head><title>Test</title></head>
<body>
<div id="content">
<h1 class="title">My Ontology</h1>
<h2 id="classes"><span class="header-section-number">1</span> Classes</h2>
<p>Intro text.</p>
<h3 id="ex:Vehicle"><span class="header-section-number">1.1</span> Vehicle (ex:Vehicle)</h3>
<dl class="org-dl">
<dt>rdfs:label</dt>
<dd>vehicle</dd>
</dl>
<h3 id="ex:Animal"><span class="header-section-number">1.2</span> Animal (ex:Animal)</h3>
<dl class="org-dl">
<dt>rdfs:label</dt>
<dd>animal</dd>
</dl>
</div>
<script src="elot-nav.js"></script>
</body></html>`;

    const result = wrapOutlineDivs(html);

    // Check outline-container divs
    assert(
      result.includes('id="outline-container-classes"') &&
        result.includes('class="outline-2"'),
      "T2a: h2 should be wrapped in outline-container div with class outline-2"
    );
    assert(
      result.includes('id="outline-container-ex:Vehicle"') &&
        result.includes('class="outline-3"'),
      "T2b: h3 should be wrapped in outline-container div with class outline-3"
    );

    // Check outline-text divs
    assert(
      result.includes('id="text-classes"') &&
        result.includes('class="outline-text-2"'),
      "T2c: h2 should have outline-text div"
    );
    assert(
      result.includes('id="text-ex:Vehicle"') &&
        result.includes('class="outline-text-3"'),
      "T2d: h3 should have outline-text div"
    );

    // Title should NOT be wrapped (no id attribute)
    assert(
      !result.includes('outline-container-title'),
      "T2e: Title heading (no id) should not be wrapped"
    );

    console.log("  Test 2 passed: outline container divs added");
    passed++;
  }

  // ── Test 3: Section number class renamed and trailing dot added ────
  {
    const html = `<!DOCTYPE html>
<html><body>
<div id="content">
<h2 id="sec1"><span class="header-section-number">1</span> First</h2>
<p>Content.</p>
<h3 id="sec1-1"><span class="header-section-number">1.1</span> Sub</h3>
<p>More.</p>
</div>
<script src="elot-nav.js"></script>
</body></html>`;

    const result = wrapOutlineDivs(html);

    assert(
      result.includes('class="section-number-2"') &&
        result.includes('>1.</span>'),
      "T3a: h2 section number should have class section-number-2 and trailing dot"
    );
    assert(
      result.includes('class="section-number-3"') &&
        result.includes('>1.1.</span>'),
      "T3b: h3 section number should have class section-number-3 and trailing dot"
    );
    assert(
      !result.includes('header-section-number'),
      "T3c: No header-section-number class should remain"
    );

    console.log("  Test 3 passed: section number classes fixed with trailing dot");
    passed++;
  }

  // ── Test 4: <dl> gets class="org-dl" ───────────────────────────────
  {
    const html = `<dl>
<dt>rdfs:label</dt>
<dd>foo</dd>
</dl>
<dl>
<dt>nested</dt>
<dd>bar</dd>
</dl>`;

    const result = html.replace(/<dl>/g, '<dl class="org-dl">');

    assert(
      !result.includes("<dl>"),
      "T4a: No bare <dl> should remain"
    );
    assert(
      (result.match(/<dl class="org-dl">/g) || []).length === 2,
      "T4b: Both <dl> elements should have org-dl class"
    );

    console.log("  Test 4 passed: <dl> gets class='org-dl'");
    passed++;
  }

  // ── Test 5: <p> unwrapped inside <dd> before sub-<dl> ─────────────
  {
    const html = `<dt>rdfs:seeAlso</dt>
<dd>
<p>om:Unit</p>
<dl class="org-dl">
<dt>iof-av:explanatoryNote</dt>
<dd>'unit' is a class.</dd>
</dl>
</dd>`;

    const result = html.replace(
      /<dd>\r?\n?<p>(.*?)<\/p>\r?\n?<dl/gs,
      (_m, content) => `<dd>\n${content}\n<dl`
    );

    assert(
      !result.includes("<p>om:Unit</p>"),
      "T5a: <p> wrapper should be removed from <dd> content before sub-<dl>"
    );
    assert(
      result.includes("om:Unit"),
      "T5b: Content should still be present"
    );
    assert(
      result.includes("<dd>\nom:Unit\n<dl"),
      "T5c: Content should be bare text before nested <dl>"
    );

    console.log("  Test 5 passed: <p> unwrapped inside <dd> before sub-<dl>");
    passed++;
  }

  // ── Test 6: Nested heading levels produce correct outline nesting ──
  {
    const html = `<!DOCTYPE html>
<html><body>
<div id="content">
<h2 id="classes"><span class="header-section-number">1</span> Classes</h2>
<p>Intro.</p>
<h3 id="ex:Vehicle"><span class="header-section-number">1.1</span> Vehicle</h3>
<dl class="org-dl"><dt>rdfs:label</dt><dd>vehicle</dd></dl>
<h4 id="ex:Car"><span class="header-section-number">1.1.1</span> Car</h4>
<dl class="org-dl"><dt>rdfs:label</dt><dd>car</dd></dl>
<h4 id="ex:Bicycle"><span class="header-section-number">1.1.2</span> Bicycle</h4>
<dl class="org-dl"><dt>rdfs:label</dt><dd>bicycle</dd></dl>
<h3 id="ex:Animal"><span class="header-section-number">1.2</span> Animal</h3>
<dl class="org-dl"><dt>rdfs:label</dt><dd>animal</dd></dl>
</div>
<script src="elot-nav.js"></script>
</body></html>`;

    const result = wrapOutlineDivs(html);

    // h4 Car should be inside outline-4 div
    assert(
      result.includes('id="outline-container-ex:Car"') &&
        result.includes('class="outline-4"'),
      "T6a: h4 Car should have outline-4 container"
    );
    assert(
      result.includes('id="outline-container-ex:Bicycle"') &&
        result.includes('class="outline-4"'),
      "T6b: h4 Bicycle should have outline-4 container"
    );

    // When Animal (h3) appears, Car and Bicycle (h4) divs should be closed
    // Animal should be in outline-3
    assert(
      result.includes('id="outline-container-ex:Animal"') &&
        result.includes('class="outline-3"'),
      "T6c: h3 Animal should have outline-3 container"
    );

    // Verify all outline divs are properly closed (equal number of opens/closes)
    const openDivs = (result.match(/<div\s/g) || []).length;
    const closeDivs = (result.match(/<\/div>/g) || []).length;
    assert(
      openDivs === closeDivs,
      `T6d: Open divs (${openDivs}) should equal close divs (${closeDivs})`
    );

    console.log("  Test 6 passed: nested heading levels produce correct outline nesting");
    passed++;
  }

  // ── Test 7: Non-standard h7+ headings get correct outline wrapping ─
  {
    const html = `<!DOCTYPE html>
<html><body>
<div id="content">
<h7 id="lis:Capability"><span class="header-section-number">1.4.3.1.1.1</span> Capability (lis:Capability)</h7>
<dl class="org-dl">
<dt>rdfs:isDefinedBy</dt>
<dd>lis-ont:core</dd>
</dl>
</div>
<script src="elot-nav.js"></script>
</body></html>`;

    const result = wrapOutlineDivs(html);

    assert(
      result.includes('id="outline-container-lis:Capability"') &&
        result.includes('class="outline-7"'),
      "T7a: h7 should have outline-7 container"
    );
    assert(
      result.includes('class="section-number-7"') &&
        result.includes('>1.4.3.1.1.1.</span>'),
      "T7b: Section number should have section-number-7 class and trailing dot"
    );
    assert(
      result.includes('id="text-lis:Capability"') &&
        result.includes('class="outline-text-7"'),
      "T7c: h7 should have outline-text-7 div"
    );

    console.log("  Test 7 passed: non-standard h7+ headings wrapped correctly");
    passed++;
  }

  // ── Test 8: TOC and title not wrapped in outline divs ──────────────
  {
    const html = `<!DOCTYPE html>
<html><body>
<div id="table-of-contents">
<h2>Table of Contents</h2>
<nav id="TOC"><ul><li>Classes</li></ul></nav>
</div>
<div id="content">
<h1 class="title">My Ontology</h1>
<p class="subtitle">An OWL ontology</p>
<h2 id="classes"><span class="header-section-number">1</span> Classes</h2>
<p>Body.</p>
</div>
<script src="elot-nav.js"></script>
</body></html>`;

    const result = wrapOutlineDivs(html);

    // TOC should be untouched
    assert(
      result.includes('<div id="table-of-contents">'),
      "T8a: TOC div should be preserved"
    );
    // Title should not be wrapped in outline div
    assert(
      !result.includes('outline-container-title'),
      "T8b: Title should not get outline container"
    );
    // Classes heading should be wrapped
    assert(
      result.includes('id="outline-container-classes"'),
      "T8c: Classes heading should get outline container"
    );

    console.log("  Test 8 passed: TOC and title not wrapped in outline divs");
    passed++;
  }

  console.log(`\nAll ${passed} post-processing tests passed.`);
}

main();
