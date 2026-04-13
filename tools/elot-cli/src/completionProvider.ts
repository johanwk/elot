// src/completionProvider.ts
//
// VS Code CompletionItemProvider for ELOT Org files.
//
// Provides "insert existing resource" functionality: as the user types,
// a completion dropdown shows all known OWL entities from the document's
// heading hierarchy, displaying label, CURIE, and RDF type.
//
// Selecting an item inserts the CURIE at the cursor position.
//
// This is the VS Code equivalent of Emacs's completing-read for
// ontology resource insertion.
//
// The completion list is populated from the same SlurpEntry map used
// by the hover provider and label decorations, rebuilt per-document
// when the completion is triggered.

import * as vscode from "vscode";
import type { SlurpEntry } from "./buildSlurp.js";
import { buildSlurp } from "./buildSlurp.js";
import { parseOrg } from "./parseOrgWasm.js";

// ─── Slurp cache (shared pattern with hoverProvider.ts) ─────────

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

  try {
    const root = parseOrg(document.getText());
    const map = buildSlurp(root);
    slurpCache.set(key, { version: document.version, map });
    return map;
  } catch (err) {
    console.error("elot CompletionProvider: failed to parse document", err);
    return new Map();
  }
}

// ─── RDF type to icon mapping ────────────────────────────────────

/**
 * Map RDF types to VS Code CompletionItemKind for appropriate icons.
 */
function kindForType(rdfType?: string): vscode.CompletionItemKind {
  switch (rdfType) {
    case "owl:Class":
      return vscode.CompletionItemKind.Class;
    case "owl:ObjectProperty":
    case "owl:DatatypeProperty":
    case "owl:AnnotationProperty":
      return vscode.CompletionItemKind.Property;
    case "owl:NamedIndividual":
      return vscode.CompletionItemKind.Value;
    case "owl:Ontology":
      return vscode.CompletionItemKind.Module;
    case "rdfs:Datatype":
      return vscode.CompletionItemKind.TypeParameter;
    default:
      return vscode.CompletionItemKind.Reference;
  }
}

// ─── Provider ────────────────────────────────────────────────────

export class ElotCompletionProvider implements vscode.CompletionItemProvider {
  provideCompletionItems(
    document: vscode.TextDocument,
    position: vscode.Position,
    _token: vscode.CancellationToken,
    _context: vscode.CompletionContext,
  ): vscode.CompletionItem[] {
    const slurp = getSlurpMap(document);
    if (slurp.size === 0) return [];

    const items: vscode.CompletionItem[] = [];

    for (const [uri, entry] of slurp) {
      // The completion item's label is the human-readable name
      const item = new vscode.CompletionItem(
        {
          label: entry.label,
          description: uri,
          detail: entry.rdfType ?? "",
        },
        kindForType(entry.rdfType),
      );

      // Inserting the completion puts the CURIE into the document
      item.insertText = uri;

      // Filter: match on both the label and the CURIE so the user
      // can type either "entity" or "obo:BFO" to find a match
      item.filterText = `${entry.label} ${uri}`;

      // Sort: group by RDF type, then alphabetically by label
      const typeOrder = entry.rdfType === "owl:Class" ? "1"
        : entry.rdfType?.includes("Property") ? "2"
        : entry.rdfType === "owl:NamedIndividual" ? "3"
        : "4";
      item.sortText = `${typeOrder}-${entry.label}`;

      // Detail shown in the completion detail pane
      const docParts: string[] = [];
      docParts.push(`**${entry.label}**`);
      docParts.push(`\n\n\`${uri}\``);
      if (entry.rdfType) {
        docParts.push(`\n\n*Type:* \`${entry.rdfType}\``);
      }
      if (entry.properties && entry.properties.length > 0) {
        docParts.push("\n\n---");
        for (const prop of entry.properties) {
          docParts.push(`\n\n*${prop.tag}:* ${prop.value}`);
        }
      }
      const doc = new vscode.MarkdownString(docParts.join(""));
      doc.supportThemeIcons = true;
      item.documentation = doc;

      items.push(item);
    }

    return items;
  }
}

// ─── Registration ────────────────────────────────────────────────

/**
 * Register the completion provider for Org files.
 * Call this from extension.ts activate().
 * Returns a Disposable that should be pushed into context.subscriptions.
 *
 * Trigger characters: ":" triggers after typing a prefix name (e.g. "obo:"),
 * and " " triggers to allow inserting a resource after a space in a
 * description list or axiom.
 */
export function registerCompletionProvider(): vscode.Disposable {
  return vscode.languages.registerCompletionItemProvider(
    [
      { language: "org" },
      { scheme: "file", pattern: "**/*.org" },
    ],
    new ElotCompletionProvider(),
    ":", // trigger after typing a prefix like "obo:"
  );
}
