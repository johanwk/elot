// src/exportHtml.ts
//
// Export ELOT Org files to styled HTML using Pandoc.
//
// - Locates or auto-downloads a standalone Pandoc binary
// - Invokes Pandoc with a custom HTML5 template embedding the ELOT theme
// - Works from both the VS Code extension and the CLI

import { execFile } from "child_process";
import { existsSync, mkdirSync, writeFileSync, unlinkSync, chmodSync, readFileSync } from "fs";
import { join } from "path";
import * as https from "https";
import * as http from "http";
import * as fs from "fs";
import * as os from "os";
import { parseOrg, getPrefixMap } from "./parseOrgWasm.js";
import { buildSlurp } from "./buildSlurp.js";

// ── ELOT Pandoc HTML5 template ──────────────────────────────────────
//
// Pandoc generates <nav id="TOC"> but elot-nav.js expects
// <div id="table-of-contents">.  The template wraps the TOC in a
// <div id="table-of-contents"> so elot-nav.js works unchanged.

const ELOT_CSS_URL =
  "https://johanwk.github.io/elot/elot-package/elot-style.css";
const ELOT_NAV_JS_URL =
  "https://johanwk.github.io/elot/elot-package/elot-nav.js";

// Pandoc template variables use $var$ syntax.  Since we're in a JS
// template literal, we must avoid clashing with JS ${...} interpolation.
// We use string concatenation for the two JS constants (CSS/JS URLs)
// and keep Pandoc $...$ variables as literal text.
const PANDOC_TEMPLATE =
  '<!DOCTYPE html>\n' +
  '<html xmlns="http://www.w3.org/1999/xhtml" lang="$lang$" xml:lang="$lang$">\n' +
  '<head>\n' +
  '  <meta charset="utf-8" />\n' +
  '  <meta name="viewport" content="width=device-width, initial-scale=1.0" />\n' +
  '$if(title-prefix)$\n' +
  '  <title>$title-prefix$ – $pagetitle$</title>\n' +
  '$else$\n' +
  '  <title>$pagetitle$</title>\n' +
  '$endif$\n' +
  '  <link rel="stylesheet" href="' + ELOT_CSS_URL + '" />\n' +
  '$for(css)$\n' +
  '  <link rel="stylesheet" href="$css$" />\n' +
  '$endfor$\n' +
  '$for(header-includes)$\n' +
  '  $header-includes$\n' +
  '$endfor$\n' +
  '</head>\n' +
  '<body>\n' +
  '$if(toc)$\n' +
  '<div id="table-of-contents">\n' +
  '<h2>Table of Contents</h2>\n' +
  '$toc$\n' +
  '</div>\n' +
  '$endif$\n' +
  '<div id="content">\n' +
  '$if(title)$\n' +
  '<h1 class="title">$title$</h1>\n' +
  '$endif$\n' +
  '$if(subtitle)$\n' +
  '<p class="subtitle">$subtitle$</p>\n' +
  '$endif$\n' +
  '$if(date)$\n' +
  '<p class="date">$date$</p>\n' +
  '$endif$\n' +
  '$body$\n' +
  '</div>\n' +
  '<script src="' + ELOT_NAV_JS_URL + '"></script>\n' +
  '$for(include-after)$\n' +
  '$include-after$\n' +
  '$endfor$\n' +
  '</body>\n' +
  '</html>\n';

// ── Pandoc binary management ────────────────────────────────────────

/** Pandoc version to auto-download */
const PANDOC_VERSION = "3.6.4";

/**
 * Return the platform-specific Pandoc download URL and extracted binary name.
 */
function pandocDownloadInfo(storageDir: string): {
  url: string;
  archiveName: string;
  binaryPath: string;
} {
  const platform = os.platform();
  const arch = os.arch();

  let archSuffix: string;
  let ext: string;
  let binaryName: string;

  if (platform === "win32") {
    archSuffix = arch === "arm64" ? "arm64" : "x86_64";
    ext = "zip";
    binaryName = "pandoc.exe";
  } else if (platform === "darwin") {
    archSuffix = arch === "arm64" ? "arm64" : "x86_64";
    ext = "zip";
    binaryName = "pandoc";
  } else {
    // Linux
    archSuffix = arch === "arm64" ? "arm64" : "amd64";
    ext = "tar.gz";
    binaryName = "pandoc";
  }

  const base = `pandoc-${PANDOC_VERSION}-${platform === "win32" ? "windows" : platform === "darwin" ? "macOS" : "linux"}-${archSuffix}`;
  const archiveName = `${base}.${ext}`;
  const url = `https://github.com/jgm/pandoc/releases/download/${PANDOC_VERSION}/${archiveName}`;

  return {
    url,
    archiveName,
    binaryPath: join(storageDir, binaryName),
  };
}

/**
 * Follow-redirect HTTPS GET that streams to a file.
 */
function downloadToFile(
  url: string,
  dest: string,
  onProgress?: (bytes: number) => void
): Promise<void> {
  return new Promise((resolve, reject) => {
    const get = (u: string) => {
      const mod = u.startsWith("https") ? https : http;
      (mod as typeof https)
        .get(u, { headers: { "User-Agent": "vscode-elot" } }, (res) => {
          if (
            res.statusCode &&
            res.statusCode >= 300 &&
            res.statusCode < 400 &&
            res.headers.location
          ) {
            get(res.headers.location);
            return;
          }
          if (res.statusCode !== 200) {
            reject(new Error(`HTTP ${res.statusCode} downloading ${u}`));
            return;
          }
          const file = fs.createWriteStream(dest);
          let received = 0;
          res.on("data", (chunk: Buffer) => {
            received += chunk.length;
            if (onProgress) onProgress(received);
          });
          res.pipe(file);
          file.on("finish", () => file.close(() => resolve()));
          file.on("error", (err) => {
            fs.unlinkSync(dest);
            reject(err);
          });
        })
        .on("error", reject);
    };
    get(url);
  });
}

/**
 * Extract the pandoc binary from a downloaded archive.
 * Uses `tar` on Unix and PowerShell on Windows.
 */
function extractPandocBinary(
  archivePath: string,
  storageDir: string
): Promise<string> {
  return new Promise((resolve, reject) => {
    const platform = os.platform();
    const info = pandocDownloadInfo(storageDir);

    if (platform === "win32") {
      // Use PowerShell to extract zip
      const ps = `Expand-Archive -Path '${archivePath}' -DestinationPath '${storageDir}' -Force`;
      execFile(
        "powershell",
        ["-NoProfile", "-Command", ps],
        (err) => {
          if (err) { reject(err); return; }
          // Find pandoc.exe recursively
          const found = findFileRecursive(storageDir, "pandoc.exe");
          if (found) {
            const dest = info.binaryPath;
            if (found !== dest) {
              fs.renameSync(found, dest);
            }
            resolve(dest);
          } else {
            reject(new Error("pandoc.exe not found in archive"));
          }
        }
      );
    } else if (archivePath.endsWith(".tar.gz")) {
      execFile(
        "tar",
        ["xzf", archivePath, "-C", storageDir, "--strip-components=2", "--include=*/bin/pandoc"],
        (err) => {
          if (err) { reject(err); return; }
          const dest = info.binaryPath;
          if (existsSync(dest)) {
            chmodSync(dest, 0o755);
            resolve(dest);
          } else {
            reject(new Error("pandoc binary not found after extraction"));
          }
        }
      );
    } else {
      // macOS zip
      execFile("unzip", ["-o", archivePath, "-d", storageDir], (err) => {
        if (err) { reject(err); return; }
        const found = findFileRecursive(storageDir, "pandoc");
        if (found) {
          const dest = info.binaryPath;
          if (found !== dest) {
            fs.renameSync(found, dest);
          }
          chmodSync(dest, 0o755);
          resolve(dest);
        } else {
          reject(new Error("pandoc binary not found in archive"));
        }
      });
    }
  });
}

/** Recursively find a file by name in a directory. */
function findFileRecursive(dir: string, name: string): string | null {
  for (const entry of fs.readdirSync(dir, { withFileTypes: true })) {
    const full = join(dir, entry.name);
    if (entry.isDirectory()) {
      const found = findFileRecursive(full, name);
      if (found) return found;
    } else if (entry.name === name) {
      return full;
    }
  }
  return null;
}

// ── Public API ──────────────────────────────────────────────────────

export interface ExportHtmlOptions {
  /** Path to pandoc executable (overrides auto-detection). */
  pandocPath?: string;
  /** Storage directory for auto-downloaded pandoc. */
  storageDir?: string;
  /** Output HTML file path. If omitted, derived from input path. */
  outputPath?: string;
  /** Progress callback for downloads. */
  onProgress?: (message: string) => void;
  /** Called when pandoc needs to be downloaded; return false to cancel. */
  onDownloadPrompt?: () => Promise<boolean>;
}

/**
 * Find an available pandoc binary.
 *
 * Search order:
 *   1. options.pandocPath (explicit setting)
 *   2. Previously downloaded binary in storageDir
 *   3. System PATH
 *
 * Returns the path, or null if not found.
 */
export function findPandoc(options?: ExportHtmlOptions): string | null {
  // 1. Explicit path
  if (options?.pandocPath && options.pandocPath !== "pandoc") {
    if (existsSync(options.pandocPath)) return options.pandocPath;
  }

  // 2. Previously downloaded
  if (options?.storageDir) {
    const info = pandocDownloadInfo(options.storageDir);
    if (existsSync(info.binaryPath)) return info.binaryPath;
  }

  // 3. System PATH — check by running `pandoc --version`
  try {
    const cmd = options?.pandocPath || "pandoc";
    require("child_process").execFileSync(cmd, ["--version"], {
      timeout: 5000,
      stdio: "pipe",
    });
    return cmd;
  } catch {
    return null;
  }
}

/**
 * Download pandoc to the storage directory.
 */
export async function downloadPandoc(
  storageDir: string,
  onProgress?: (message: string) => void
): Promise<string> {
  if (!existsSync(storageDir)) {
    mkdirSync(storageDir, { recursive: true });
  }

  const info = pandocDownloadInfo(storageDir);
  const archivePath = join(storageDir, info.archiveName);

  onProgress?.(`Downloading Pandoc ${PANDOC_VERSION}…`);

  await downloadToFile(info.url, archivePath, (bytes) => {
    onProgress?.(`Downloading Pandoc: ${(bytes / (1024 * 1024)).toFixed(1)} MB`);
  });

  onProgress?.("Extracting Pandoc…");
  const binaryPath = await extractPandocBinary(archivePath, storageDir);

  // Clean up archive
  try { unlinkSync(archivePath); } catch { /* ignore */ }
  // Clean up extracted directory (keep only the binary)
  for (const entry of fs.readdirSync(storageDir, { withFileTypes: true })) {
    const full = join(storageDir, entry.name);
    if (entry.isDirectory() && entry.name.startsWith("pandoc-")) {
      fs.rmSync(full, { recursive: true, force: true });
    }
  }

  return binaryPath;
}

/**
 * Ensure pandoc is available — find it or download it.
 */
export async function ensurePandoc(
  options: ExportHtmlOptions
): Promise<string | null> {
  const found = findPandoc(options);
  if (found) return found;

  // Need to download
  if (!options.storageDir) return null;

  if (options.onDownloadPrompt) {
    const proceed = await options.onDownloadPrompt();
    if (!proceed) return null;
  }

  return downloadPandoc(options.storageDir, options.onProgress);
}

// ── Org pre-processing for CURIE anchors & links ───────────────────
//
// The Elisp export pipeline (elot--prepare-export-buffer) does three things
// before handing the buffer to org-export:
//   1. Sets CUSTOM_ID on every resource-declaring heading to the CURIE
//   2. Replaces CURIE mentions in body text with [[#curie][label]] links
//
// We replicate this by pre-processing the Org text before feeding it to
// Pandoc.  This ensures that:
//   - Every heading that declares an OWL entity gets an HTML id= equal
//     to its CURIE (e.g. id="lis:Object")
//   - CURIEs appearing in body text become clickable links showing labels
//     (except inside src/example blocks, fixed-width lines, and drawers)

/**
 * Global regex matching CURIEs (same pattern as labelDecorations.ts / hoverProvider.ts).
 */
// Match CURIEs like ex:Foo or :localName.
// The local part may contain dots/slashes but must not END with punctuation
// (period, comma) which is likely sentence-level punctuation.
const CURIE_GLOBAL_RE = /(?:[a-zA-Z][-a-zA-Z0-9_.]*|):[-\w_./]*[-\w_/]/g;

/**
 * Replace all CURIE occurrences in `text` with Org internal links [[#curie][label]].
 * Skips CURIEs that are already inside an Org link ([[...]]).
 */
function linkifyCuries(
  text: string,
  declaredCuries: Set<string>,
  curieToLabel: Map<string, string>
): string {
  // Find all existing Org link spans so we can skip them
  const linkSpans: Array<[number, number]> = [];
  const linkRe = /\[\[[^\]]*\]\[[^\]]*\]\]/g;
  let lm: RegExpExecArray | null;
  while ((lm = linkRe.exec(text)) !== null) {
    linkSpans.push([lm.index, lm.index + lm[0].length]);
  }

  function insideLink(start: number, end: number): boolean {
    return linkSpans.some(([lo, hi]) => start >= lo && end <= hi);
  }

  // Collect replacements (reverse order to preserve offsets)
  const replacements: Array<{ start: number; end: number; replacement: string }> = [];

  CURIE_GLOBAL_RE.lastIndex = 0;
  let m: RegExpExecArray | null;
  while ((m = CURIE_GLOBAL_RE.exec(text)) !== null) {
    const curie = m[0];
    if (!declaredCuries.has(curie)) continue;
    const start = m.index;
    const end = start + curie.length;
    if (insideLink(start, end)) continue;

    // Check word boundary: char before start and after end should not be word-like
    if (start > 0 && /[\w:]/.test(text[start - 1])) continue;
    if (end < text.length && /[\w]/.test(text[end])) continue;

    const label = curieToLabel.get(curie) ?? curie;
    replacements.push({ start, end, replacement: `[[#${curie}][${label}]]` });
  }

  // Apply in reverse order
  let result = text;
  for (let i = replacements.length - 1; i >= 0; i--) {
    const r = replacements[i];
    result = result.substring(0, r.start) + r.replacement + result.substring(r.end);
  }
  return result;
}

/**
 * Expand a bare prefix in a description-list value.
 * E.g. if value is "ex:" and prefixMap has ex → "http://example.org/ontology/",
 * returns "http://example.org/ontology/".
 * Only expands if the trimmed value is exactly a prefix followed by colon.
 */
function expandBarePrefix(
  value: string,
  prefixMap: Map<string, string>
): string {
  const trimmed = value.trim();
  // Match a bare prefix: one or more word/hyphen chars followed by ":"
  const m = trimmed.match(/^([a-zA-Z][-a-zA-Z0-9_.]*):\s*$/);
  if (m) {
    const prefixName = m[1];
    const uri = prefixMap.get(prefixName);
    if (uri) {
      return uri;
    }
  }
  return value;
}

/**
 * Pre-process Org text to inject CUSTOM_ID properties and internal links.
 *
 * Returns the modified Org text ready for Pandoc.
 */
export function preprocessOrgForLinks(orgText: string): string {
  const lines = orgText.split("\n");

  // --- Pass 1: collect all declared CURIEs and their labels ----------
  //
  // Use parseOrg + buildSlurp — the same infrastructure that powers
  // label decorations, hover, and completions.
  const root = parseOrg(orgText);
  const slurp = buildSlurp(root);

  const curieToLabel = new Map<string, string>();
  const declaredCuries = new Set<string>();

  for (const [uri, entry] of slurp) {
    // Only handle simple CURIEs (not <URI> or composite)
    if (uri.startsWith("<") || uri.includes(" ")) continue;
    declaredCuries.add(uri);
    curieToLabel.set(uri, entry.label);
  }

  // Build the prefix→URI map for bare-prefix expansion
  const prefixMap = getPrefixMap(root);

  if (declaredCuries.size === 0 && !prefixMap) return orgText;
  if (declaredCuries.size === 0 && prefixMap) {
    // Only prefix expansion needed, no CURIE linkification — fall through
  }

  // --- Pass 2: inject CUSTOM_ID and rewrite body CURIEs -------------
  const result: string[] = [];
  let i = 0;
  let insideBlock = false; // tracks #+begin_src/#+begin_example regions

  while (i < lines.length) {
    const line = lines[i];
    const headingMatch = line.match(/^(\*+)\s+(.+)$/);

    if (headingMatch) {
      result.push(line);
      i++;

      // Determine CURIE for this heading by checking if any declared
      // CURIE appears as a word in the heading text.
      const titleText = headingMatch[2].replace(/\s*:[\w:]+:\s*$/, "");
      let curie: string | null = null;
      for (const c of declaredCuries) {
        // Match the CURIE at a word boundary in the heading
        const escaped = c.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
        if (new RegExp(`\\b${escaped}\\b`).test(titleText)) {
          curie = c;
          break;
        }
      }

      // Check if next lines are a :PROPERTIES: drawer
      if (i < lines.length && lines[i].match(/^\s*:PROPERTIES:\s*$/i)) {
        result.push(lines[i]); // :PROPERTIES:
        i++;

        // Check if CUSTOM_ID already exists in this drawer
        let hasCustomId = false;
        const drawerStart = result.length;
        const drawerLines: string[] = [];

        while (i < lines.length && !lines[i].match(/^\s*:END:\s*$/i)) {
          if (lines[i].match(/^\s*:CUSTOM_ID:/i)) {
            hasCustomId = true;
          }
          drawerLines.push(lines[i]);
          i++;
        }

        // Inject CUSTOM_ID if missing and we have a CURIE
        if (curie && !hasCustomId) {
          result.push(`:CUSTOM_ID: ${curie}`);
        }

        // Push the rest of the drawer properties
        for (const dl of drawerLines) {
          result.push(dl);
        }

        // Push :END:
        if (i < lines.length) {
          result.push(lines[i]); // :END:
          i++;
        }
      } else if (curie) {
        // No property drawer — create one with CUSTOM_ID
        result.push(":PROPERTIES:");
        result.push(`:CUSTOM_ID: ${curie}`);
        result.push(":END:");
      }
    } else {
      // Body line — replace CURIE references with [[#curie][label]] links,
      // but only in description-list values (after " :: ") and NOT in
      // headings, src blocks, example blocks, fixed-width lines, or
      // drawer lines.
      //
      // Track block state for src/example exclusion.
      const beginBlock = line.match(/^\s*#\+begin_(src|example)\b/i);
      const endBlock = line.match(/^\s*#\+end_(src|example)\b/i);
      if (beginBlock) insideBlock = true;
      if (endBlock) { result.push(line); i++; insideBlock = false; continue; }

      const isDrawerLine = /^\s*:/.test(line);
      const isKeywordLine = /^\s*#\+/i.test(line);
      const isFixedWidth = /^\s*: /.test(line);
      const skip = insideBlock || isDrawerLine || isKeywordLine || isFixedWidth;

      if (!skip) {
        let processed = line;
        // For description lists, only linkify the value (after " :: "),
        // leaving the tag (before " :: ") untouched.
        // For all other body text, linkify the entire line.
        const sepIdx = processed.indexOf(" :: ");
        const isDescList = /^\s*-\s+/.test(processed) && sepIdx >= 0;

        if (isDescList) {
          // Extract the tag (before " :: ") and value (after " :: ")
          const fullTag = processed.substring(0, sepIdx);
          const separator = " :: ";
          let value = processed.substring(sepIdx + 4);

          // Linkify the tag: if it contains a CURIE, make it a link.
          // E.g. "- rdfs:isDefinedBy" → "- [[#rdfs:isDefinedBy][rdfs:isDefinedBy]]"
          let tag = fullTag;
          const tagCurieMatch = fullTag.match(/^(\s*-\s+)(.+)$/);
          if (tagCurieMatch) {
            const prefix = tagCurieMatch[1]; // leading "- "
            const tagText = tagCurieMatch[2].trim();
            // Linkify if the entire tag looks like a CURIE (e.g. rdfs:isDefinedBy).
            // We linkify ALL CURIE-like tags, not just declared ones — matching Elisp behaviour.
            // This makes annotation property names clickable.
            const curieTagRe = /^(?:[a-zA-Z][-a-zA-Z0-9_.]*|):[-\w_./]*[-\w_/]$/;
            if (curieTagRe.test(tagText)) {
              tag = `${prefix}[[#${tagText}][${tagText}]]`;
            }
          }
          value = linkifyCuries(value, declaredCuries, curieToLabel);
          // Resolve bare prefix: if value is just a prefix like "ex:", expand it
          if (prefixMap) {
            value = expandBarePrefix(value, prefixMap);
          }
          processed = tag + separator + value;
        } else {
          processed = linkifyCuries(processed, declaredCuries, curieToLabel);
        }
        result.push(processed);
      } else {
        result.push(line);
      }
      i++;
    }
  }

  // Ensure Pandoc treats all heading levels as actual headings (not list items)
  const joined = result.join("\n");
  if (/^#\+OPTIONS:/im.test(joined)) {
    return joined.replace(/^(#\+OPTIONS:.*)/im, "$1 H:12 ^:nil");
  }
  return "#+OPTIONS: H:12 ^:nil\n" + joined;
}

/**
 * Export an Org file to HTML using Pandoc with the ELOT theme.
 *
 * Pre-processes the Org text to inject CUSTOM_ID properties (using the
 * CURIE as anchor id) and internal links, then invokes Pandoc.
 *
 * @returns The path to the generated HTML file.
 */
export async function exportOrgToHtml(
  inputPath: string,
  pandocPath: string,
  outputPath?: string
): Promise<string> {
  const outPath =
    outputPath || inputPath.replace(/\.org$/, ".html");

  // Read and pre-process the Org file
  const rawOrg = readFileSync(inputPath, "utf-8");
  const processedOrg = preprocessOrgForLinks(rawOrg);

  // Write pre-processed Org to a temp file (Pandoc reads from file)
  const tmpOrgPath = join(os.tmpdir(), `elot-export-${Date.now()}.org`);
  writeFileSync(tmpOrgPath, processedOrg, "utf-8");

  // Write the template to a temp file
  const templatePath = join(os.tmpdir(), "elot-pandoc-template.html5");
  writeFileSync(templatePath, PANDOC_TEMPLATE, "utf-8");

  const args = [
    tmpOrgPath,
    "-f", "org",
    "-t", "html5",
    "--standalone",
    "--template", templatePath,
    "--toc",
    "--toc-depth=6",
    "--shift-heading-level-by=-1",
    "-o", outPath,
  ];

  return new Promise((resolve, reject) => {
    execFile(pandocPath, args, { maxBuffer: 50 * 1024 * 1024 }, (err, _stdout, stderr) => {
      // Clean up temp files
      try { unlinkSync(templatePath); } catch { /* ignore */ }
      try { unlinkSync(tmpOrgPath); } catch { /* ignore */ }

      if (err) {
        reject(new Error(`Pandoc failed (exit ${err.code}):\n${stderr || err.message}`));
        return;
      }
      if (stderr) {
        console.log("[pandoc stderr]", stderr);
      }
      // Post-process the HTML for ELOT conventions
      try {
        let html = readFileSync(outPath, "utf-8");
        // Remove <p>...</p> wrapping inside <dd> when followed by a sub-<dl>
        // Pandoc wraps in <p> for "loose" lists; ELOT expects bare text.
        // Must run BEFORE the org-dl class addition so <dl> is still bare.
        html = html.replace(/<dd>\r?\n?<p>(.*?)<\/p>\r?\n?<dl/gs, (m, content) => `<dd>\n${content}\n<dl`);
        // Add class="org-dl" to all <dl> elements (for ELOT CSS)
        html = html.replace(/<dl>/g, '<dl class="org-dl">');
        writeFileSync(outPath, html, "utf-8");
      } catch { /* non-fatal: HTML is still valid without these tweaks */ }
      resolve(outPath);
    });
  });
}
