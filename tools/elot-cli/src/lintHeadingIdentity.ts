// src/lintHeadingIdentity.ts
//
// Stage 4: Heading identity checker (checker #5).
// Validates that each heading under :resourcedefs: has a valid CURIE/URI
// with known prefix and proper label, or is tagged :nodeclare:.
// Pure TypeScript — no VS Code imports.

import type { ElotNode } from "./types.js";
import type { LintDiagnostic } from "./elotLintHelpers.js";
import {
  walkResourceNodes,
  isNodeclare,
  isCurieKnown,
} from "./elotLintHelpers.js";
import { buildPrefixMap } from "./prefixes.js";

// ─── Local prefix map helper (avoids importing parseOrgWasm/WASM) ─

/**
 * Find the prefix map from the tree by locating the `prefixdefs` node.
 * Same as in lintStructural.ts — avoids pulling in WASM.
 */
function getPrefixMapFromTree(root: ElotNode): Map<string, string> {
  const stack: ElotNode[] = [root];
  while (stack.length > 0) {
    const node = stack.pop()!;
    if (node.prefixdefs && (node.prefixes ?? []).length > 0) {
      return buildPrefixMap(node.prefixes!);
    }
    const children = node.children ?? [];
    for (let i = children.length - 1; i >= 0; i--) {
      stack.push(children[i]);
    }
  }
  return new Map();
}

// ─── CURIE detection ────────────────────────────────────────────

/**
 * Check whether a string looks like a CURIE (prefix:localname).
 * Full URIs wrapped in angle brackets are not CURIEs.
 */
function isCurie(uri: string): boolean {
  // Full URI in angle brackets — not a CURIE
  if (uri.startsWith("<")) return false;
  // Must contain a colon (CURIE separator)
  return uri.includes(":");
}

// ─── Label validation ───────────────────────────────────────────

/**
 * Check whether a label containing quotes is a valid `"text"@lang` string.
 *
 * The Elisp check is:
 *   (not (or (not (string-match-p "\"" label))
 *            (string-match-p "^\"[^\"]+\"@[[:alpha:]-]+$" label)))
 *
 * In plain English: if the label contains quotes, it must match the
 * pattern `"text"@lang` (e.g. `"Apple"@en`). If it doesn't contain
 * quotes, it's fine.
 */
function isLabelValid(label: string): boolean {
  if (!label.includes('"')) {
    // No quotes — always fine
    return true;
  }
  // Has quotes — must match "text"@lang
  return /^"[^"]+"@[a-zA-Z-]+$/.test(label);
}

// ─── Checker #5 ─────────────────────────────────────────────────

/**
 * Check heading identity: each heading under :resourcedefs: must have a
 * valid CURIE/URI with a known prefix and proper label, or be tagged
 * :nodeclare:.
 *
 * Mirrors `elot-check-nodeclare-id-prefix-label` from elot-lint.el.
 *
 * Skips:
 * - Nodes tagged :nodeclare: (or with :nodeclare: ancestor)
 * - Section root nodes (those with `resourcedefs === true` directly)
 *   — they are structural headings, not entity declarations
 */
export function checkHeadingIdentity(root: ElotNode): LintDiagnostic[] {
  const diagnostics: LintDiagnostic[] = [];
  const prefixMap = getPrefixMapFromTree(root);

  walkResourceNodes(root, (node, ancestors) => {
    // Skip section roots — they set resourcedefs themselves and are not
    // entity-declaring headings.
    if (node.resourcedefs) return;

    // Skip :nodeclare: headings (own tag or inherited from ancestor)
    if (isNodeclare(node, ancestors)) return;

    // Check that the heading has an identifier
    if (!node.uri) {
      diagnostics.push({
        node,
        message:
          "No identifier found – add CURIE/URI or tag :nodeclare:",
        severity: "error",
      });
      return;
    }

    // If it's a CURIE, check the prefix is known
    if (isCurie(node.uri)) {
      if (!isCurieKnown(node.uri, prefixMap)) {
        diagnostics.push({
          node,
          message: "Unknown prefix in identifier",
          severity: "error",
        });
        return;
      }
    }

    // Check label format — if it contains quotes, must be "text"@lang
    if (node.label && !isLabelValid(node.label)) {
      diagnostics.push({
        node,
        message:
          'Label should have no quotes or be a "text"@lang string',
        severity: "warning",
      });
    }
  });

  return diagnostics;
}
