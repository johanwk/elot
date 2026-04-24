// src/parsers/prefixes.ts
//
// Harvest @prefix / PREFIX declarations from a TTL or SPARQL file.
// Ports elot-source--harvest-prefixes from elot-package/elot-sources.el.
// First occurrence wins.  The empty prefix ("@prefix : <...>") is
// supported and stored with the empty string as key.

import { existsSync, readFileSync } from "fs";

const PREFIX_RE =
  /^[ \t]*(?:@prefix|PREFIX)[ \t]+([A-Za-z_][A-Za-z0-9_.-]*)?[ \t]*:[ \t]*<([^>]*)>/gim;

export function harvestPrefixes(file: string): Array<[string, string]> {
  if (!file || !existsSync(file)) return [];
  const text = readFileSync(file, "utf-8");
  const seen = new Set<string>();
  const out: Array<[string, string]> = [];
  for (const m of text.matchAll(PREFIX_RE)) {
    const prefix = m[1] ?? "";
    const expansion = m[2] ?? "";
    if (seen.has(prefix)) continue;
    seen.add(prefix);
    out.push([prefix, expansion]);
  }
  return out;
}
