// src/parsers/index.ts
//
// Dispatcher for source parsers.  Maps a SourceType (or a file
// extension) to one of csvTsv / json.  TTL/RQ land in Step 2.2.4 and
// Org in Step 2.2.5.

import { extname } from "path";
import { parseCsv, parseTsv, ParsedSource } from "./csvTsv.js";
import { parseJson } from "./json.js";

export type SourceType =
  | "csv"
  | "tsv"
  | "json"
  // legacy / test fixture shape consumed directly by dbCli register
  | "triples-json"
  // 2.2.4 / 2.2.5
  | "ttl"
  | "rq"
  | "org";

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

export function parseSource(file: string, type: SourceType): ParsedSource {
  switch (type) {
    case "csv":
      return parseCsv(file);
    case "tsv":
      return parseTsv(file);
    case "json":
      return parseJson(file);
    default:
      throw new Error(
        `parseSource: type '${type}' is not implemented in Step 2.2.3 ` +
          `(TTL/RQ land in 2.2.4, Org in 2.2.5, triples-json uses the ` +
          `dbCli register legacy path).`,
      );
  }
}

export type { ParsedSource };
export { parseCsv, parseTsv, parseJson };
