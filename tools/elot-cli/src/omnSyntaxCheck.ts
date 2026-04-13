// src/omnSyntaxCheck.ts
//
// OMN (OWL Manchester Syntax) syntax checking for elot-cli.
//
// Uses a pre-generated Peggy parser (omnParser.js) that is built at
// build time from syntax/owl-manchester.peggy via:
//
//   npm run build:parser
//
// The grammar source of truth is syntax/owl-manchester.peggy (shared
// with the Emacs/peg.el side).

// --- Load the pre-generated parser ---
//
// The parser is generated as CommonJS by:
//   peggy --format commonjs ... -o src/omnParser.js
//
// esbuild resolves this statically at bundle time and inlines it into
// dist/extension.js. The generated parser has no external dependencies.
//
// For tsx (dev/test mode), the .js file is resolved relative to the
// source .ts file.

let parser: { parse: (input: string, options?: { startRule?: string }) => unknown } | null = null;

try {
  // eslint-disable-next-line @typescript-eslint/ban-ts-comment
  // @ts-ignore — generated file, may not exist until build:parser runs
  const omnParser = require("./omnParser.js");
  // Peggy CJS module exports { parse, SyntaxError } at top level
  if (typeof omnParser?.parse === "function") {
    parser = omnParser;
  } else if (typeof omnParser?.default?.parse === "function") {
    parser = omnParser.default;
  }
} catch {
  // omnParser.js not found — syntax checking will be silently disabled
  parser = null;
}


// --- Keyword → start-rule mapping (mirrors elot-omn-keyword-parser-alist) ---

const KEYWORD_START_RULE: Record<string, string> = {
  SubClassOf: "ClassExpressionList",
  EquivalentTo: "ClassExpressionList",
  DisjointWith: "ClassExpressionList",
  Domain: "ClassExpressionList",
  Range: "ClassExpressionList",
  Types: "ClassExpressionList",
  InverseOf: "ObjectPropertyExpressionList",
  SubPropertyOf: "ObjectPropertyExpressionList",
  SubPropertyChain: "SubPropertyChain",
  Facts: "Fact",
  SameAs: "IndividualIRIList",
  DifferentFrom: "IndividualIRIList",
};

// --- Public API ---

export interface SyntaxOk {
  ok: true;
}

export interface SyntaxError {
  ok: false;
  offset: number;
  message: string;
}

export type SyntaxResult = SyntaxOk | SyntaxError;

/**
 * Parse `input` using the given Peggy start rule.
 * Returns { ok: true } on success, or { ok: false, offset, message } on failure.
 */
export function parseWithRule(
  input: string,
  startRule: string
): SyntaxResult {
  if (!parser) {
    // Parser not available (omnParser.js not generated) — skip silently
    return { ok: true };
  }
  try {
    parser.parse(input, { startRule });
    return { ok: true };
  } catch (e: unknown) {
    if (e && typeof e === "object" && "location" in e && "message" in e) {
      const pe = e as { location?: { start?: { offset?: number } }; message: string };
      return {
        ok: false,
        offset: pe.location?.start?.offset ?? 0,
        message: pe.message,
      };
    }
    return { ok: false, offset: 0, message: String(e) };
  }
}

/**
 * Check the syntax of an OMN axiom value for a given keyword.
 *
 * @param keyword  The OMN keyword (e.g. "SubClassOf", "Domain", "Facts")
 * @param value    The axiom value string (after " :: ")
 * @returns        SyntaxResult — ok: true if valid, or error info
 */
export function checkOmnSyntax(
  keyword: string,
  value: string
): SyntaxResult {
  const startRule = KEYWORD_START_RULE[keyword];
  if (!startRule) {
    // Unknown keyword — skip (don't flag as error)
    return { ok: true };
  }
  return parseWithRule(value.trim(), startRule);
}
