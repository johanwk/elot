// src/foldingProvider.ts
//
// Headline-based folding for Org-mode files in VS Code.
//
// Uses the built-in FoldingRangeProvider API so that VS Code renders
// fold/unfold gutters, respects keybindings (Ctrl+Shift+[ / ]), and
// works with "Fold All" / "Unfold All" commands out of the box.
//
// Each Org headline (line starting with one or more `*` characters)
// opens a fold region that extends to just before the next headline
// at the same or higher level, or to the end of the file.

import * as vscode from "vscode";

/**
 * Regex matching an Org headline.  Captures the leading stars so we
 * can derive the headline level from the length of group 1.
 *
 *   * Top-level           → level 1
 *   ** Second level       → level 2
 *   *** Third level       → level 3
 */
const HEADLINE_RE = /^(\*+)\s/;

/**
 * A FoldingRangeProvider that creates fold regions based on Org
 * headline hierarchy.
 */
export class OrgFoldingRangeProvider implements vscode.FoldingRangeProvider {
  provideFoldingRanges(
    document: vscode.TextDocument,
    _context: vscode.FoldingContext,
    _token: vscode.CancellationToken,
  ): vscode.FoldingRange[] {
    const ranges: vscode.FoldingRange[] = [];

    // First pass: collect all headline positions
    interface HeadlineInfo {
      line: number;
      level: number;
    }
    const headlines: HeadlineInfo[] = [];

    for (let i = 0; i < document.lineCount; i++) {
      const lineText = document.lineAt(i).text;
      const m = HEADLINE_RE.exec(lineText);
      if (m) {
        headlines.push({ line: i, level: m[1].length });
      }
    }

    // Second pass: for each headline, find where its region ends.
    // A headline's fold region extends until just before the next
    // headline at the same or higher (numerically ≤) level, or to
    // the end of the document.
    for (let i = 0; i < headlines.length; i++) {
      const current = headlines[i];
      let endLine: number;

      // Look for the next headline at same or higher level
      let found = false;
      for (let j = i + 1; j < headlines.length; j++) {
        if (headlines[j].level <= current.level) {
          // Fold region ends on the line just before this sibling/ancestor headline.
          // But skip backwards over any trailing blank lines for a cleaner fold.
          endLine = headlines[j].line - 1;
          while (endLine > current.line && document.lineAt(endLine).isEmptyOrWhitespace) {
            endLine--;
          }
          found = true;
          break;
        }
      }

      if (!found) {
        // No same-or-higher-level headline follows: fold to end of document,
        // skipping trailing blank lines.
        endLine = document.lineCount - 1;
        while (endLine > current.line && document.lineAt(endLine).isEmptyOrWhitespace) {
          endLine--;
        }
      }

      // Only create a range if there is at least one line to fold
      if (endLine! > current.line) {
        ranges.push(
          new vscode.FoldingRange(
            current.line,
            endLine!,
            vscode.FoldingRangeKind.Region,
          ),
        );
      }
    }

    return ranges;
  }
}

/**
 * Register the Org folding range provider.
 *
 * Returns a Disposable so the caller can add it to
 * `context.subscriptions` for automatic cleanup.
 */
export function registerFoldingProvider(): vscode.Disposable {
  // Register for both the "org" languageId (if an Org language
  // extension is installed) and a file-glob pattern for .org files
  // (so it works even without a language grammar).
  const selector: vscode.DocumentSelector = [
    { language: "org" },
    { pattern: "**/*.org" },
  ];

  return vscode.languages.registerFoldingRangeProvider(
    selector,
    new OrgFoldingRangeProvider(),
  );
}
