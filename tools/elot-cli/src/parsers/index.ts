// src/parsers/index.ts
//
// Dispatcher for source parsers.  Maps a SourceType (or a file
// extension) to one of csvTsv / json / ttl / rq.  Org lands in 2.2.5.

import { extname } from "path";
import { parseCsv, parseTsv, ParsedSource } from "./csvTsv.js";
import { parseJson } from "./json.js";
import { parseTtl } from "./ttl.js";
import { parseRq, ParseRqOptions } from "./rq.js";
import { parseOrgSource } from "./org.js";

export type SourceType =
  | "csv"
  | "tsv"
  | "json"
  // legacy / test fixture shape consumed directly by dbCli register
  | "triples-json"
  | "ttl"
  | "rq"
  // 2.2.5
  | "org";

export interface ParseOptions {
  /** For `rq`: local RDF file or http(s) endpoint URL. */
  dataSource?: string | null;
}

export function detectTypeFromExtension(file: string): SourceType | null {
  const ext = extname(file).toLowerCase();
  switch (ext) {
    case ".csv":
      return "csv";
    case ".tsv":
      return "tsv";
    case ".json":
      return "json";
    case ".ttl":
      return "ttl";
    case ".rq":
      return "rq";
    case ".org":
      return "org";
    default:
      return null;
  }
}

export function parseSource(
  file: string,
  type: SourceType,
  opts: ParseOptions = {},
): ParsedSource {
  switch (type) {
    case "csv":
      return parseCsv(file);
    case "tsv":
      return parseTsv(file);
    case "json":
      return parseJson(file);
    case "ttl":
      return parseTtl(file);
    case "rq":
      return parseRq(file, opts as ParseRqOptions);
    case "org":
      return parseOrgSource(file);
    default:
      throw new Error(
        `parseSource: type '${type}' is not implemented in this build ` +
          `(triples-json uses the dbCli register legacy path).`,
      );
  }
}

export type { ParsedSource };
export { parseCsv, parseTsv, parseJson, parseTtl, parseRq, parseOrgSource };
export { robotAvailable, resolveRobot } from "./robot.js";
export { harvestPrefixes } from "./prefixes.js";
