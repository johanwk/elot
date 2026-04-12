// src/omnParser.d.ts
//
// Type declaration for the Peggy-generated parser (omnParser.js).
// The .js file is generated at build time by:
//   npm run build:parser
//
// This file exists only so that TypeScript doesn't complain about
// the require("./omnParser.js") in omnSyntaxCheck.ts.

declare const omnParser: {
  parse(input: string, options?: { startRule?: string }): unknown;
  SyntaxError: new (message: string, expected: unknown, found: unknown, location: unknown) => Error;
};
export = omnParser;
