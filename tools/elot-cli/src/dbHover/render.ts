// src/dbHover/render.ts
//
// Step 2.3.2: pure Markdown renderer for the DB hover provider.
//
// Returns a single Markdown string given (token, label, attrs,
// origin-source).  No vscode import -- testable under tsx; the
// extension wraps the result in `vscode.MarkdownString`.

export interface HoverRenderInput {
  /** Token as it appeared at the cursor (CURIE, IRI, ...). */
  token: string;
  /** Resolved label, or null if none. */
  label: string | null;
  /** All attributes of the entity, in DB-given order. */
  attrs: ReadonlyArray<[string, string]>;
  /** Source the entity was resolved from. */
  origin?: { source: string; dataSource: string } | null;
}

/** Markdown special chars we escape when emitting plain text. */
const MD_ESCAPE_RE = /([\\`*_{}\[\]()#+\-.!|<>])/g;

function esc(s: string): string {
  return s.replace(MD_ESCAPE_RE, "\\$1");
}

/** Heuristic: is this an http(s) URI we should render as a clickable link? */
function isHttpUri(s: string): boolean {
  return /^https?:\/\//.test(s);
}

/** Properties that get top-billing (in this order) above the rule. */
const FEATURED_PROPS = ["rdf:type", "skos:definition", "rdfs:comment"];

export function renderHoverMarkdown(inp: HoverRenderInput): string {
  const { token, label, attrs, origin } = inp;
  const lines: string[] = [];

  // ── Header ──────────────────────────────────────────────────
  if (label && label.length > 0) {
    lines.push(`**${esc(label)}**`);
  } else {
    lines.push(`**${esc(token)}**`);
  }

  // ── id row ──────────────────────────────────────────────────
  if (isHttpUri(token)) {
    // Render the IRI as a clickable link.  esc() the visible text
    // but keep the URL itself intact for the link target.
    lines.push("");
    lines.push(`[${esc(token)}](${token})`);
  } else {
    lines.push("");
    lines.push("`" + token + "`");
  }

  // ── Featured props ──────────────────────────────────────────
  const byProp = new Map<string, string[]>();
  for (const [p, v] of attrs) {
    if (!byProp.has(p)) byProp.set(p, []);
    byProp.get(p)!.push(v);
  }

  const featured = FEATURED_PROPS.filter((p) => byProp.has(p));
  if (featured.length > 0) {
    lines.push("");
    for (const p of featured) {
      const values = byProp.get(p)!;
      // For rdf:type, emit a code-formatted CURIE/URI; for definitions
      // and comments emit prose.
      if (p === "rdf:type") {
        const t = values.map((v) => "`" + v + "`").join(", ");
        lines.push(`*Type:* ${t}`);
      } else {
        for (const v of values) {
          lines.push(`*${esc(p)}:* ${esc(v)}`);
        }
      }
    }
  }

  // ── Other props (separator) ─────────────────────────────────
  const otherKeys: string[] = [];
  for (const p of byProp.keys()) {
    if (p === "rdfs:label") continue;
    if (FEATURED_PROPS.includes(p)) continue;
    otherKeys.push(p);
  }
  if (otherKeys.length > 0) {
    lines.push("");
    lines.push("---");
    lines.push("");
    for (const p of otherKeys) {
      for (const v of byProp.get(p)!) {
        lines.push(`*${esc(p)}:* ${esc(v)}`);
      }
    }
  }

  // ── Provenance ──────────────────────────────────────────────
  if (origin && origin.source.length > 0) {
    lines.push("");
    const tag =
      origin.dataSource && origin.dataSource.length > 0
        ? `${origin.source} \u2014 ${origin.dataSource}`
        : origin.source;
    lines.push(`*[src: ${esc(tag)}]*`);
  }

  return lines.join("\n");
}
