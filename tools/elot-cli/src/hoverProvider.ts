// src/hoverProvider.ts
//
// VS Code HoverProvider for ELOT Org files.
//
// When the user hovers over a CURIE or URI in an Org-mode buffer,
// this provider looks it up in the SlurpEntry map and displays:
//   - The human-readable label
//   - The rdf:type
//   - Any additional description properties (e.g. skos:definition)
//
// The slurp map is rebuilt whenever the document changes and cached
// per-document.

import * as vscode from "vscode";
import type { ElotNode } from "./types.js";
import type { SlurpEntry } from "./buildSlurp.js";
import { buildSlurp } from "./buildSlurp.js";
import { parseOrg } from "./parseOrgWasm.js";

// ─── CURIE / URI detection ───────────────────────────────────────

// Matches a CURIE like "obo:BFO_0000001" or ":localName"
const CURIE_RE = /(?:[a-zA-Z][-a-zA-Z0-9_.]*|):[-\w_./]+/;

// Matches a full URI wrapped in angle brackets like <http://...>
const ANGLE_URI_RE = /<(https?:\/\/[^>]+)>/;

// Matches a bare http(s) URL (without angle brackets).
// Used as a fallback when the cursor is on the URL text itself, where
// VS Code's built-in link detection would otherwise be the only hover.
const BARE_URL_RE = /https?:\/\/[^\s>,)}\]]+/;

/**
 * At a given document position, try to find a CURIE or <URI> under the cursor.
 * Returns [the matched string (as stored in the slurp map), the Range] or null.
 */
function curieAtPosition(
  document: vscode.TextDocument,
  position: vscode.Position,
): [string, vscode.Range] | null {
  // Try CURIE first (most common case)
  const curieRange = document.getWordRangeAtPosition(position, CURIE_RE);
  if (curieRange) {
    const text = document.getText(curieRange);
    return [text, curieRange];
  }

  // Try angle-bracket URI (cursor on < or >)
  const uriRange = document.getWordRangeAtPosition(position, ANGLE_URI_RE);
  if (uriRange) {
    const rawText = document.getText(uriRange);
    const match = rawText.match(ANGLE_URI_RE);
    if (match) {
      return [`<${match[1]}>`, uriRange];
    }
  }

  // Try bare URL (cursor on the http://... text inside angle brackets).
  // VS Code's built-in link detector matches this region too, but both
  // hovers are shown — ours with entity details, theirs with "Follow link".
  const bareRange = document.getWordRangeAtPosition(position, BARE_URL_RE);
  if (bareRange) {
    const bareUrl = document.getText(bareRange);
    // Look up as "<url>" since that's how the slurp map stores full URIs
    return [`<${bareUrl}>`, bareRange];
  }

  return null;
}

// ─── Markdown rendering ──────────────────────────────────────────

function renderHover(entry: SlurpEntry): vscode.MarkdownString {
  const md = new vscode.MarkdownString();
  md.supportThemeIcons = true;
  md.isTrusted = true;

  // Header: label (bold) — URI
  md.appendMarkdown(`**${escapeMarkdown(entry.label)}**`);

  // Show URI — if it's a full URI (angle-bracket form), make it a clickable link
  const fullUriMatch = entry.uri.match(/^<(https?:\/\/.+)>$/);
  if (fullUriMatch) {
    md.appendMarkdown(`\n\n[${escapeMarkdown(fullUriMatch[1])}](${fullUriMatch[1]})`);
  } else {
    md.appendMarkdown(`\n\n\`${entry.uri}\``);
  }

  // Type
  if (entry.rdfType) {
    md.appendMarkdown(`\n\n*Type:* \`${entry.rdfType}\``);
  }

  // Extra properties
  if (entry.properties && entry.properties.length > 0) {
    md.appendMarkdown("\n\n---\n\n");
    for (const prop of entry.properties) {
      md.appendMarkdown(`*${escapeMarkdown(prop.tag)}:* ${escapeMarkdown(prop.value)}\n\n`);
    }
  }

  return md;
}

function escapeMarkdown(s: string): string {
  // Escape characters that have special meaning in Markdown
  return s.replace(/([\\`*_{}[\]()#+\-.!|])/g, "\\$1");
}

// ─── Slurp cache ─────────────────────────────────────────────────

/**
 * Per-document cache of the slurp map.
 * Keyed by document URI string, value is { version, map }.
 * Invalidated when the document version changes.
 */
const slurpCache = new Map<
  string,
  { version: number; map: Map<string, SlurpEntry> }
>();

function getSlurpMap(document: vscode.TextDocument): Map<string, SlurpEntry> {
  const key = document.uri.toString();
  const cached = slurpCache.get(key);
  if (cached && cached.version === document.version) {
    return cached.map;
  }

  // Rebuild
  try {
    const root = parseOrg(document.getText());
    const map = buildSlurp(root);
    slurpCache.set(key, { version: document.version, map });
    return map;
  } catch (err) {
    console.error("elot HoverProvider: failed to parse document", err);
    return new Map();
  }
}

// ─── Provider ────────────────────────────────────────────────────

export class ElotHoverProvider implements vscode.HoverProvider {
  provideHover(
    document: vscode.TextDocument,
    position: vscode.Position,
    _token: vscode.CancellationToken,
  ): vscode.Hover | null {
    const found = curieAtPosition(document, position);
    if (!found) return null;

    const [uri, range] = found;
    const slurp = getSlurpMap(document);
    const entry = slurp.get(uri);
    if (!entry) return null;

    // Don't show a hover if the label is the same as the URI (no extra info)
    if (entry.label === entry.uri && !entry.rdfType && !entry.properties) {
      return null;
    }

    return new vscode.Hover(renderHover(entry), range);
  }
}

/**
 * Register the hover provider for Org files.
 * Call this from extension.ts activate().
 * Returns a Disposable that should be pushed into context.subscriptions.
 */
export function registerHoverProvider(): vscode.Disposable {
  // Register for both the "org" language ID and .org files by glob
  return vscode.languages.registerHoverProvider(
    [
      { language: "org" },
      { scheme: "file", pattern: "**/*.org" },
    ],
    new ElotHoverProvider(),
  );
}

/**
 * Clear the slurp cache for a specific document (e.g. on close).
 */
export function clearSlurpCache(uri: string): void {
  slurpCache.delete(uri);
}

/**
 * Clear the entire slurp cache.
 */
export function clearAllSlurpCaches(): void {
  slurpCache.clear();
}
