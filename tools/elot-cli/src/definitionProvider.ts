// src/definitionProvider.ts
//
// VS Code DefinitionProvider for ELOT Org files.
//
// When the user Ctrl+Clicks (or presses F12) on a CURIE or URI,
// this provider jumps to the Org headline where that entity is declared.
//
// This is the VS Code equivalent of Emacs's M-. (xref-find-definitions).
//
// The provider scans the document for headlines, extracts entity identity
// from each heading using entityFromHeader(), and returns the location
// of the matching headline.

import * as vscode from "vscode";
import { entityFromHeader } from "./entityFromHeader.js";

// ─── CURIE / URI detection (same patterns as hoverProvider.ts) ───

const CURIE_RE = /(?:[a-zA-Z][-a-zA-Z0-9_.]*|):[-\w_./:]+/;
const ANGLE_URI_RE = /<(https?:\/\/[^>]+)>/;

/**
 * At a given document position, try to find a CURIE or <URI> under the cursor.
 * Returns the matched string (normalised to the form used by entityFromHeader)
 * and the Range, or null.
 */
function curieAtPosition(
  document: vscode.TextDocument,
  position: vscode.Position,
): [string, vscode.Range] | null {
  // Try CURIE first (most common case)
  const curieRange = document.getWordRangeAtPosition(position, CURIE_RE);
  if (curieRange) {
    return [document.getText(curieRange), curieRange];
  }

  // Try angle-bracket URI
  const uriRange = document.getWordRangeAtPosition(position, ANGLE_URI_RE);
  if (uriRange) {
    const rawText = document.getText(uriRange);
    const match = rawText.match(ANGLE_URI_RE);
    if (match) {
      return [`<${match[1]}>`, uriRange];
    }
  }

  return null;
}

// ─── Headline scanning ──────────────────────────────────────────

/** Regex matching an Org headline: one or more stars, a space, then text */
const HEADLINE_RE = /^(\*+)\s+(.+)$/;

/**
 * Build a map from entity URI → line number by scanning all headlines
 * in the document and extracting entity identity from each.
 *
 * This is fast (single pass, line-based, no WASM) and mirrors how
 * foldingProvider.ts scans for headlines.
 */
function buildDefinitionIndex(
  document: vscode.TextDocument,
): Map<string, number> {
  const index = new Map<string, number>();
  for (let i = 0; i < document.lineCount; i++) {
    const lineText = document.lineAt(i).text;
    const m = lineText.match(HEADLINE_RE);
    if (!m) continue;

    const headingText = m[2];
    const entity = entityFromHeader(headingText);
    if (entity) {
      // Store the first occurrence (entity declarations should be unique,
      // but if duplicated, the first one wins — like Emacs xref)
      if (!index.has(entity.id)) {
        index.set(entity.id, i);
      }
    }
  }
  return index;
}

// ─── Provider ────────────────────────────────────────────────────

export class ElotDefinitionProvider implements vscode.DefinitionProvider {
  provideDefinition(
    document: vscode.TextDocument,
    position: vscode.Position,
    _token: vscode.CancellationToken,
  ): vscode.Location | null {
    const found = curieAtPosition(document, position);
    if (!found) return null;

    const [uri] = found;

    const index = buildDefinitionIndex(document);
    const line = index.get(uri);
    if (line === undefined) return null;

    // Position at the start of the heading text (after the stars and space)
    const headingLine = document.lineAt(line).text;
    const starsMatch = headingLine.match(/^(\*+\s+)/);
    const col = starsMatch ? starsMatch[1].length : 0;

    return new vscode.Location(
      document.uri,
      new vscode.Position(line, col),
    );
  }
}

// ─── Registration ────────────────────────────────────────────────

/**
 * Register the definition provider for Org files.
 * Call this from extension.ts activate().
 * Returns a Disposable that should be pushed into context.subscriptions.
 */
export function registerDefinitionProvider(): vscode.Disposable {
  return vscode.languages.registerDefinitionProvider(
    [
      { language: "org" },
      { scheme: "file", pattern: "**/*.org" },
    ],
    new ElotDefinitionProvider(),
  );
}
