// src/db/goldenDump.ts
//
// Slice 3a (Step 2.2.5): canonical JSON dump of an ElotDb's data
// tables, used for byte-identical golden round-trip tests against
// the Elisp writer.
//
// To avoid any byte-level differences between Node's JSON.stringify
// and Emacs' json-serialize, we use a tiny custom serializer with
// explicit, minimal escape rules.  Both this module and the matching
// Elisp dumper (test/elot-db-golden-gen.el) implement the same rules:
//
//   - Output is minified UTF-8, single line, trailing newline.
//   - Object keys are emitted alphabetically.
//   - Array elements are emitted in iteration order; the caller is
//     responsible for sorting (see SELECT ORDER BY clauses below).
//   - Strings: non-ASCII characters are emitted as raw UTF-8 bytes.
//     The following are escaped as \" \\ \n \r \t; code points
//     < 0x20 not in that set are emitted as \u00XX.  Forward slash
//     is NOT escaped (matches JSON.stringify default).
//   - null: literal "null".  No numbers, no booleans (we exclude
//     timestamp columns; all other DB values are strings or NULL).
//
// Top-level shape:
//   {"attributes":[...],"entities":[...],"prefixes":[...],"sources":[...]}
//
// Excluded: sources.last_modified / last_updated (timestamps);
// global_prefixes (seed data, identical on both sides).

import { ElotDb } from "./sqljs.js";

interface Row {
  [k: string]: string | null;
}

function escapeString(s: string): string {
  let out = '"';
  for (let i = 0; i < s.length; i++) {
    const c = s.charCodeAt(i);
    if (c === 0x22) out += '\\"';
    else if (c === 0x5c) out += "\\\\";
    else if (c === 0x0a) out += "\\n";
    else if (c === 0x0d) out += "\\r";
    else if (c === 0x09) out += "\\t";
    else if (c < 0x20) {
      out += "\\u" + c.toString(16).padStart(4, "0");
    } else {
      out += s[i];
    }
  }
  return out + '"';
}

function emitValue(v: string | null): string {
  return v === null ? "null" : escapeString(v);
}

function emitRow(row: Row): string {
  const keys = Object.keys(row).sort();
  const parts = keys.map((k) => `${escapeString(k)}:${emitValue(row[k])}`);
  return "{" + parts.join(",") + "}";
}

function emitArray(rows: readonly Row[]): string {
  return "[" + rows.map(emitRow).join(",") + "]";
}

function rowsFromSelect(
  db: ElotDb,
  sql: string,
  cols: readonly string[],
): Row[] {
  const stmt = db.raw.prepare(sql);
  const out: Row[] = [];
  try {
    while (stmt.step()) {
      const vals = stmt.get() as unknown[];
      const row: Row = {};
      cols.forEach((c, i) => {
        const v = vals[i];
        row[c] = v == null ? null : String(v);
      });
      out.push(row);
    }
  } finally {
    stmt.free();
  }
  return out;
}

/**
 * Produce the canonical golden dump for DB.  Returns a string ending
 * with a trailing newline.
 */
export function canonicalDump(db: ElotDb): string {
  const sources = rowsFromSelect(
    db,
    `SELECT source, data_source, type
       FROM sources
       ORDER BY source, data_source`,
    ["source", "data_source", "type"],
  );
  const entities = rowsFromSelect(
    db,
    `SELECT id, label, source, data_source, kind
       FROM entities
       ORDER BY id, source, data_source`,
    ["id", "label", "source", "data_source", "kind"],
  );
  const attributes = rowsFromSelect(
    db,
    `SELECT id, source, data_source, prop, value, lang
       FROM attributes
       ORDER BY id, source, data_source, prop, lang, value`,
    ["id", "source", "data_source", "prop", "value", "lang"],
  );
  const prefixes = rowsFromSelect(
    db,
    `SELECT source, data_source, prefix, expansion
       FROM prefixes
       ORDER BY source, data_source, prefix`,
    ["source", "data_source", "prefix", "expansion"],
  );
  // Top-level keys must be emitted alphabetically.
  const out =
    "{" +
    `"attributes":${emitArray(attributes)},` +
    `"entities":${emitArray(entities)},` +
    `"prefixes":${emitArray(prefixes)},` +
    `"sources":${emitArray(sources)}` +
    "}\n";
  return out;
}
