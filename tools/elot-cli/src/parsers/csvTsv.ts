// src/parsers/csvTsv.ts
//
// CSV / TSV parsers.  Ports elot-source--parse-separated and the
// RFC 4180 line splitter from elot-package/elot-sources.el, including
// the Step 1.16.6 language conventions:
//
//   - a `lang` header column (index >= 2) attaches a row-level
//     BCP-47 tag to the primary rdfs:label.  When present, every
//     populated label is emitted as an rdfs:label attribute row
//     (including lang="") so the reader-side picker can see all
//     variants.
//   - `label@TAG` header suffixes (index >= 2) produce additional
//     rdfs:label attribute rows with LANG=TAG for each populated cell.
//
// Files with neither convention ingest exactly as before: columns 0/1
// become id/label and the remaining columns become plain attribute
// rows keyed by header name.  Emits a ParsedSource whose entries
// feed ElotDb.updateSource() unchanged.

import { readFileSync } from "fs";
import { EntityTriple, AttrValue } from "../db/sqljs.js";

export interface ParsedSource {
  entries: EntityTriple[];
  /** Optional prefix declarations harvested from the source. */
  prefixes?: Array<[string, string]>;
}

/**
 * Split LINE on SEP (single-character string) honouring double-quoted
 * fields with RFC 4180 doubled-quote escapes.  Surrounding quotes are
 * stripped from returned fields.  Trailing empty field after a final
 * separator is preserved.
 */
export function splitCsvLine(line: string, sep: string): string[] {
  const sepCh = sep.charCodeAt(0);
  const fields: string[] = [];
  const n = line.length;
  let i = 0;
  while (i < n) {
    const c = line.charCodeAt(i);
    if (c === 34 /* " */) {
      i++;
      let start = i;
      const bufParts: string[] = [];
      let done = false;
      while (!done && i < n) {
        const c2 = line.charCodeAt(i);
        if (c2 === 34 && i + 1 < n && line.charCodeAt(i + 1) === 34) {
          bufParts.push(line.slice(start, i));
          bufParts.push('"');
          i += 2;
          start = i;
        } else if (c2 === 34) {
          bufParts.push(line.slice(start, i));
          done = true;
          i++;
        } else {
          i++;
        }
      }
      // Consume an optional separator after the closing quote.
      if (i < n && line.charCodeAt(i) === sepCh) i++;
      fields.push(bufParts.join(""));
    } else {
      const start = i;
      while (i < n && line.charCodeAt(i) !== sepCh) i++;
      fields.push(line.slice(start, i));
      if (i < n && line.charCodeAt(i) === sepCh) i++;
    }
  }
  // Handle trailing empty field after a final separator.
  if (n > 0 && line.charCodeAt(n - 1) === sepCh) {
    fields.push("");
  }
  return fields;
}

/** Strip trailing CR / whitespace (mirrors Elisp string-trim-right). */
function trimRight(s: string): string {
  return s.replace(/[\s\uFEFF\xA0]+$/, "");
}

/** Strip a UTF-8 BOM from the start of S, if present. */
function stripBom(s: string): string {
  return s.length > 0 && s.charCodeAt(0) === 0xfeff ? s.slice(1) : s;
}

/**
 * Parse CONTENT as separated values with SEP.  The header row is the
 * first non-empty line.  Column 0 is id, column 1 is label, columns
 * 2+ are attributes or the special `lang` / `label@TAG` columns.
 */
export function parseSeparatedString(
  content: string,
  sep: string,
): ParsedSource {
  const lines = stripBom(content).split(/\r?\n/);
  let headers: string[] | null = null;
  let li = 0;
  while (li < lines.length && !headers) {
    const line = trimRight(lines[li]);
    if (line.length === 0) {
      li++;
      continue;
    }
    headers = splitCsvLine(line, sep);
    li++;
  }
  if (!headers) return { entries: [] };

  let idxLang: number | null = null;
  const labelAtCols: Array<{ idx: number; tag: string }> = [];
  headers.forEach((h, i) => {
    if (i < 2) return;
    if (h === "lang") {
      idxLang = i;
      return;
    }
    const m = /^label@(.+)$/.exec(h);
    if (m) labelAtCols.push({ idx: i, tag: m[1] });
  });

  const entries: EntityTriple[] = [];
  for (; li < lines.length; li++) {
    const line = trimRight(lines[li]);
    if (line.length === 0) continue;
    const fields = splitCsvLine(line, sep);
    const id = fields[0] ?? "";
    const label = fields[1] ?? "";
    if (!id) continue;
    const rowLang =
      idxLang != null ? fields[idxLang] ?? "" : null;
    const attrs: Array<[string, AttrValue]> = [];

    for (let i = 2; i < headers.length; i++) {
      if (idxLang != null && i === idxLang) continue;
      if (labelAtCols.some((c) => c.idx === i)) continue;
      const h = headers[i];
      const v = fields[i];
      if (v != null && v !== "") {
        attrs.push([h, v]);
      }
    }
    // Row-level lang attaches to the primary label.  When the CSV has
    // a `lang` column (Step 1.16.6 / 1.16.7), emit rdfs:label for every
    // populated label including lang="".
    if (idxLang != null && label !== "") {
      attrs.push([
        "rdfs:label",
        { value: label, lang: rowLang ?? "" },
      ]);
    }
    for (const cell of labelAtCols) {
      const v = fields[cell.idx];
      if (v != null && v !== "") {
        attrs.push(["rdfs:label", { value: v, lang: cell.tag }]);
      }
    }
    entries.push({ id, label, attrs });
  }
  return { entries };
}

export function parseCsv(file: string): ParsedSource {
  const text = readFileSync(file, "utf-8");
  return parseSeparatedString(text, ",");
}

export function parseTsv(file: string): ParsedSource {
  const text = readFileSync(file, "utf-8");
  return parseSeparatedString(text, "\t");
}
