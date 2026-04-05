// src/prefixes.ts
//
// Builds a Map for CURIE expansion from PrefixEntry arrays.

import type { PrefixEntry } from "./types.js";

/**
 * Build a prefix→URI Map suitable for CURIE expansion.
 */
export function buildPrefixMap(entries: PrefixEntry[]): Map<string, string> {
  const map = new Map<string, string>();

  for (const { prefix, uri } of entries) {
    // Strip trailing colon from the prefix name for map key
    const key = prefix.endsWith(":") ? prefix.slice(0, -1) : prefix;
    if (!map.has(key)) {
      map.set(key, uri);
    }
  }

  return map;
}
