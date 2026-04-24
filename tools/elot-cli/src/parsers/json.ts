// src/parsers/json.ts
//
// JSON label-map parser.  Ports elot-source-parse-json from
// elot-package/elot-sources.el, including the Step 1.16.6 nested
// language conventions:
//
//   - flat shape:   { "id1": "label1", "id2": "label2", ... }
//   - nested shape: { "id1": { "label": "...", "lang": "...",
//                               "label@TAG": "...", "otherprop": "..." } }
//
// Nested fields:
//   - `label`    -> primary label (denormalised entities.label).
//   - `lang`     -> row-level tag attached to the primary label;
//                   emits an rdfs:label attribute row with that tag
//                   only when both label AND lang are non-empty
//                   (matches the Elisp semantics; files without a
//                   lang key ingest as untagged, unchanged).
//   - `label@TAG`-> additional rdfs:label attribute row per populated
//                   key, LANG=TAG.
//   - any other key becomes a plain attribute row keyed by that name.
//
// Values are coerced to strings (numbers -> String(), null -> "").

import { readFileSync } from "fs";
import { EntityTriple, AttrValue } from "../db/sqljs.js";
import { ParsedSource } from "./csvTsv.js";

function coerce(v: unknown): string {
  if (v == null) return "";
  if (typeof v === "string") return v;
  if (typeof v === "number" || typeof v === "boolean") return String(v);
  return String(v);
}

export function parseJson(file: string): ParsedSource {
  const raw = readFileSync(file, "utf-8");
  const data = JSON.parse(raw) as Record<string, unknown>;
  const entries: EntityTriple[] = [];

  for (const [id, val] of Object.entries(data)) {
    if (typeof val === "string") {
      entries.push({ id, label: val, attrs: [] });
      continue;
    }
    if (val != null && typeof val === "object" && !Array.isArray(val)) {
      let label: string | null = null;
      let labelLang: string | null = null;
      const attrs: Array<[string, AttrValue]> = [];
      for (const [k, v] of Object.entries(val as Record<string, unknown>)) {
        const vs = coerce(v);
        if (k === "label") {
          label = vs;
        } else if (k === "lang") {
          labelLang = vs;
        } else {
          const m = /^label@(.+)$/.exec(k);
          if (m) {
            if (vs !== "") {
              attrs.push(["rdfs:label", { value: vs, lang: m[1] }]);
            }
          } else {
            attrs.push([k, vs]);
          }
        }
      }
      // Row-level lang attaches to the primary label, but only when
      // both non-empty (Elisp parity).
      if (
        labelLang != null &&
        labelLang !== "" &&
        label != null &&
        label !== ""
      ) {
        attrs.push([
          "rdfs:label",
          { value: label, lang: labelLang },
        ]);
      }
      entries.push({ id, label: label ?? "", attrs });
      continue;
    }
    // Scalar non-string/non-object (number, bool, null): coerce to label.
    entries.push({ id, label: coerce(val), attrs: [] });
  }
  return { entries };
}
