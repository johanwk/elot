// elot-orgize/src/parse.rs
//
// Walk the orgize AST and produce an ElotNode tree.
//
// STEP 5: Headlines + property drawers + prefix tables + description lists +
// #+begin_src omn blocks.
// Extracts headline hierarchy with level, title, tags, property drawer
// values (ID, ELOT-context-type, resourcedefs, prefixdefs, header-args:omn, etc.),
// prefix table entries from headings with :prefixdefs: yes,
// description list items with nested meta-annotations,
// and raw content of #+begin_src omn source blocks.

use crate::types::{ElotNode, DescriptionItem, PrefixEntry};
use orgize::Org;
use orgize::ast::{Headline, PropertyDrawer, NodeProperty, OrgTable, OrgTableRow};
use orgize::SyntaxKind;
use orgize::rowan::ast::AstNode;
use orgize::rowan;
use std::collections::HashMap;

/// Parse Org text into an ElotNode hierarchy.
///
/// Current capabilities (Step 4):
///   - Extracts headline hierarchy with level, title, and tags.
///   - Extracts property drawer values: ID, ELOT-context-type,
///     ELOT-context-localname, ELOT-default-prefix, resourcedefs,
///     prefixdefs, and header-args:omn (tangle target).
///   - Extracts prefix tables from headings with :prefixdefs: yes.
///   - Extracts description list items with nested meta-annotations.
///   - Extracts raw content of #+begin_src omn blocks.
///   - Returns a level-0 "ROOT" node wrapping the tree.
pub fn parse_to_elot(org_text: &str) -> ElotNode {
    let org = Org::parse(org_text);

    let mut root = ElotNode {
        level: 0,
        title: "ROOT".to_string(),
        ..Default::default()
    };

    // Collect all headlines in document order.
    let document = org.document();

    // Walk the syntax tree to find all Headline nodes.
    // orgize 0.10-alpha uses rowan, so we walk SyntaxNode children.
    let mut headlines: Vec<RawHeadline> = Vec::new();
    collect_headlines(document.syntax(), &mut headlines);

    // Build the tree by inserting each headline at the correct depth.
    for hl in &headlines {
        let props = &hl.properties;

        // Extract tangle target from :header-args:omn: :tangle <path>
        let tangle_target_omn = props
            .get("header-args:omn")
            .and_then(|v| extract_tangle_target(v));

        let node = ElotNode {
            level: hl.level,
            title: hl.title.clone(),
            tags: hl.tags.clone(),
            id: props.get("id").cloned(),
            elot_context_type: props.get("elot-context-type").cloned(),
            elot_context_localname: props.get("elot-context-localname").cloned(),
            elot_default_prefix: props.get("elot-default-prefix").cloned(),
            resourcedefs: props.get("resourcedefs").map_or(false, |v| v == "yes"),
            prefixdefs: props.get("prefixdefs").map_or(false, |v| v == "yes"),
            tangle_target_omn,
            prefixes: hl.prefixes.clone(),
            descriptions: hl.descriptions.clone(),
            omn_src_blocks: hl.omn_src_blocks.clone(),
            ..Default::default()
        };
        insert_node(&mut root, node);
    }

    root
}

struct RawHeadline {
    level: u32,
    title: String,
    tags: Vec<String>,
    properties: HashMap<String, String>,
    /// Prefix entries extracted from an Org table under this headline
    /// (only populated when :prefixdefs: yes).
    prefixes: Vec<PrefixEntry>,
    /// Description list items extracted from the headline's body.
    descriptions: Vec<DescriptionItem>,
    /// Raw content of #+begin_src omn blocks under this headline.
    omn_src_blocks: Vec<String>,
}

/// Recursively walk the rowan syntax tree to find all Headline nodes.
///
/// orgize 0.10.0-alpha.10 uses rowan's SyntaxNode<OrgLanguage>.
/// We use AstNode::cast() to detect Headline nodes.
fn collect_headlines(
    node: &orgize::SyntaxNode,
    headlines: &mut Vec<RawHeadline>,
) {
    for child in node.children() {
        if let Some(headline) = Headline::cast(child.clone()) {
            let level = headline.level() as u32;

            // headline.title() returns impl Iterator<Item = SyntaxElement>.
            // Each SyntaxElement is a NodeOrToken. We collect the text
            // from all elements to reconstruct the title string.
            let title_text: String = headline
                .title()
                .map(|element| element.to_string())
                .collect::<String>()
                .trim()
                .to_string();

            // headline.tags() returns an iterator of tag strings
            let tags: Vec<String> = headline
                .tags()
                .map(|tag| tag.to_string())
                .collect();

            // Extract property drawer
            let properties = extract_properties(&headline);

            // Extract prefix table if this heading has :prefixdefs: yes
            let prefixes = if properties.get("prefixdefs").map_or(false, |v| v == "yes") {
                extract_prefix_table(&headline)
            } else {
                Vec::new()
            };

            // Extract description list items from the headline's body
            let descriptions = extract_description_list(&headline);

            // Extract #+begin_src omn blocks from the headline's body
            let omn_src_blocks = extract_omn_src_blocks(&headline);

            headlines.push(RawHeadline {
                level,
                title: title_text,
                tags,
                properties,
                prefixes,
                descriptions,
                omn_src_blocks,
            });
        }

        // Recurse into child nodes to find nested headlines
        collect_headlines(&child, headlines);
    }
}

/// Extract properties from a headline's property drawer.
///
/// orgize models property drawers as a `PropertyDrawer` node containing
/// `NodeProperty` children. In orgize 0.10.0-alpha.10, `NodeProperty` may
/// not expose `.key()` / `.value()` methods, so we parse the raw text
/// of each property line, which has the form `:KEY: VALUE`.
/// Keys are normalised to lowercase for uniform lookup.
fn extract_properties(headline: &Headline) -> HashMap<String, String> {
    let mut props = HashMap::new();

    // The PropertyDrawer is a child of the Section that follows the Headline.
    // In orgize's rowan tree: Headline > Section > PropertyDrawer > NodeProperty
    // But the Headline AST node in orgize wraps the whole subtree, so we
    // look for PropertyDrawer among all descendants.
    for node in headline.syntax().descendants() {
        if let Some(drawer) = PropertyDrawer::cast(node.clone()) {
            for child in drawer.syntax().children() {
                if let Some(prop) = NodeProperty::cast(child) {
                    // Parse the raw text of the property line: ":KEY: VALUE"
                    if let Some((key, value)) = parse_node_property_text(&prop.syntax().text().to_string()) {
                        props.insert(key, value);
                    }
                }
            }
            break; // Only the first (direct) property drawer matters
        }
    }

    props
}

/// Parse a raw property line like `:KEY: VALUE` into (lowercase_key, trimmed_value).
///
/// Org property keys can contain colons (e.g. `:header-args:omn:`), so we
/// can't just split on the first inner colon.  The *end* of the key is marked
/// by a colon followed by a space, end-of-string, or a newline — i.e. the
/// pattern `:<space>` or `:$`.  We therefore search for `: ` (colon-space)
/// scanning from the *second* character onward (the first `:` is the opening
/// delimiter).
fn parse_node_property_text(text: &str) -> Option<(String, String)> {
    let trimmed = text.trim();
    // Must start with ':'
    let rest = trimmed.strip_prefix(':')?;
    // Find the closing ': ' (colon followed by space-or-end) that terminates
    // the key.  Scan for ": " first; if not found, try a trailing ':' at the
    // very end (for empty-value properties like `:KEY:`).
    let (key, value) = if let Some(pos) = rest.find(": ") {
        (&rest[..pos], rest[pos + 1..].trim())
    } else if rest.ends_with(':') {
        (&rest[..rest.len() - 1], "")
    } else {
        return None;
    };
    let key = key.trim().to_lowercase();
    if key.is_empty() {
        None
    } else {
        Some((key, value.to_string()))
    }
}

/// Extract prefix entries from an Org table under a headline.
///
/// Looks for the first `OrgTable` node among the headline's descendants
/// (but not descending into child headlines). Skips rule rows and the
/// header row (where column 1 is "prefix"). Returns a vec of
/// PrefixEntry { prefix, uri }.
fn extract_prefix_table(headline: &Headline) -> Vec<PrefixEntry> {
    let mut entries = Vec::new();

    // Find the first OrgTable in the headline's section (body), not in sub-headlines.
    let table = match find_table_in_headline(headline) {
        Some(t) => t,
        None => return entries,
    };

    for child in table.syntax().children() {
        if let Some(row) = OrgTableRow::cast(child) {
            // Skip rule/separator rows (e.g. |---+---|)
            if row.is_rule() {
                continue;
            }

            // Extract cell text by parsing the row's raw text.
            // A standard row looks like: "| cell1 | cell2 |\n"
            let cells = extract_cells_from_row(&row);

            if cells.len() < 2 {
                continue;
            }

            let col1 = cells[0].trim().to_string();
            let col2 = cells[1].trim().to_string();

            // Skip header row
            if col1.to_lowercase() == "prefix" || col1.to_lowercase() == "prefix:" {
                continue;
            }

            // Skip empty rows
            if col1.is_empty() && col2.is_empty() {
                continue;
            }

            // Strip angle brackets from URIs
            let uri = if col2.starts_with('<') && col2.ends_with('>') {
                col2[1..col2.len() - 1].to_string()
            } else {
                col2
            };

            entries.push(PrefixEntry {
                prefix: col1,
                uri,
            });
        }
    }

    entries
}

/// Extract cell contents from a standard table row.
///
/// A standard row in orgize's raw text looks like: `| cell1 | cell2 |`
/// We split on `|`, trim, and drop the empty leading/trailing entries.
fn extract_cells_from_row(row: &OrgTableRow) -> Vec<String> {
    let text = row.syntax().text().to_string();
    // Split on '|', skip leading empty segment (before first |) and
    // trailing empty segment (after last |). Trim each cell.
    text.split('|')
        .map(|s| s.trim().to_string())
        .filter(|s| !s.is_empty())
        .collect()
}

/// Find the first OrgTable node within a headline's body (section),
/// without descending into child headlines.
fn find_table_in_headline(headline: &Headline) -> Option<OrgTable> {
    // In orgize's rowan tree, a Headline's syntax node contains
    // the section content and nested sub-headlines.
    // We look for OrgTable nodes that are NOT inside a nested Headline.
    for descendant in headline.syntax().descendants() {
        // Skip nested headlines (they are child sections, not our body)
        if descendant != *headline.syntax() {
            if Headline::cast(descendant.clone()).is_some() {
                continue;
            }
        }
        if let Some(table) = OrgTable::cast(descendant) {
            return Some(table);
        }
    }
    None
}

// ─── Description list extraction ─────────────────────────────────

/// Extract description list items from a headline's body.
///
/// In orgize's rowan AST, a headline contains a Section child, which holds
/// the body content (paragraphs, lists, tables, etc.). The List AST type
/// contains ListItem children. A description list item has a LIST_ITEM_TAG
/// child (the predicate before "::") and a LIST_ITEM_CONTENT child (the
/// value text, possibly containing nested Lists for meta-annotations).
///
/// We use the typed AST API: headline.section() → Section, then find
/// List nodes in the section (not in sub-headlines).
fn extract_description_list(headline: &Headline) -> Vec<DescriptionItem> {
    let mut items = Vec::new();

    // Find List nodes within this headline's subtree, but skip nested Headlines.
    // We use the same descendants() approach that works for tables/property drawers.
    let lists = find_lists_in_headline(headline);
    for list_node in &lists {
        extract_items_from_list(list_node, &mut items);
    }

    items
}

/// Find List nodes within a headline's body, not descending into child Headlines.
/// Uses the same descendants() approach as find_table_in_headline.
fn find_lists_in_headline(headline: &Headline) -> Vec<orgize::SyntaxNode> {
    let mut lists = Vec::new();
    // Walk all descendants, but skip nodes that belong to nested headlines.
    // We track whether we've entered a nested headline.
    collect_lists_recursive(headline.syntax(), &mut lists);
    lists
}

/// Recursively collect LIST nodes, skipping nested Headline subtrees.
///
/// In orgize's rowan tree the starting Headline node's direct children
/// include both its Section (body content) *and* any nested sub-Headlines.
/// We must skip child Headlines at *every* level — including the very first
/// call — otherwise description lists belonging to sub-headings leak into
/// the parent.
fn collect_lists_recursive(
    node: &orgize::SyntaxNode,
    lists: &mut Vec<orgize::SyntaxNode>,
) {
    for child in node.children() {
        // Skip nested headlines — their lists belong to sub-headings
        if Headline::cast(child.clone()).is_some() {
            continue;
        }
        if child.kind() == SyntaxKind::LIST {
            lists.push(child);
        } else {
            collect_lists_recursive(&child, lists);
        }
    }
}

/// Extract description items from a List's syntax node.
///
/// orgize may or may not produce LIST_ITEM_TAG nodes for description list
/// items — in particular, when the tag text contains colons (e.g.
/// `dc11:identifier`), orgize treats it as a plain list item and puts the
/// entire text (including ` :: `) into the PARAGRAPH inside
/// LIST_ITEM_CONTENT.  We therefore use a two-pronged approach:
///
/// 1. If a LIST_ITEM has a LIST_ITEM_TAG child → use it (orgize recognised
///    the description list syntax).
/// 2. Otherwise, look for " :: " in the first paragraph of
///    LIST_ITEM_CONTENT and split on that.
fn extract_items_from_list(list_node: &orgize::SyntaxNode, items: &mut Vec<DescriptionItem>) {
    for child in list_node.children() {
        if child.kind() != SyntaxKind::LIST_ITEM {
            continue;
        }

        // ── Strategy 1: orgize parsed a LIST_ITEM_TAG ────────────────
        let tag_node = child.children().find(|c| c.kind() == SyntaxKind::LIST_ITEM_TAG);

        if let Some(ref tn) = tag_node {
            let tag_text = extract_tag_text(tn);
            if tag_text.is_empty() {
                continue;
            }

            let content_node = child.children().find(|c| c.kind() == SyntaxKind::LIST_ITEM_CONTENT);
            let (value_text, nested_lists) = match content_node {
                Some(ref cn) => extract_content_text_and_nested_lists(cn),
                None => (String::new(), Vec::new()),
            };

            let mut meta = Vec::new();
            for nested_list in &nested_lists {
                extract_items_from_list(nested_list, &mut meta);
            }

            items.push(DescriptionItem {
                tag: tag_text,
                value: value_text,
                meta,
            });
            continue;
        }

        // ── Strategy 2: no LIST_ITEM_TAG — split on " :: " in text ─
        let content_node = child.children().find(|c| c.kind() == SyntaxKind::LIST_ITEM_CONTENT);
        let content_node = match content_node {
            Some(cn) => cn,
            None => continue,
        };

        // Gather paragraph text and nested lists from the content node
        let mut text_parts: Vec<String> = Vec::new();
        let mut nested_lists: Vec<orgize::SyntaxNode> = Vec::new();
        for cc in content_node.children() {
            if cc.kind() == SyntaxKind::LIST {
                if is_description_list(&cc) {
                    nested_lists.push(cc);
                } else {
                    // Plain ordered/unordered list — render as text content
                    let list_text = render_plain_list_as_text(&cc);
                    if !list_text.is_empty() {
                        text_parts.push(list_text);
                    }
                }
            } else if cc.kind() == SyntaxKind::PARAGRAPH {
                let para_text = cc.text().to_string();
                let trimmed_end = para_text.trim_end();
                if !trimmed_end.is_empty() {
                    text_parts.push(trimmed_end.to_string());
                }
            }
        }

        let full_text = text_parts.join("\n");
        // Look for the " :: " separator
        if let Some(sep_pos) = full_text.find(" :: ") {
            let tag_text = full_text[..sep_pos].trim().to_string();
            // Trim only the start of the value (first line), preserve
            // continuation line indentation.
            let value_text = full_text[sep_pos + 4..].trim_start().to_string();
            let value_text = value_text.trim_end().to_string();

            if tag_text.is_empty() {
                continue;
            }

            let mut meta = Vec::new();
            for nested_list in &nested_lists {
                extract_items_from_list(nested_list, &mut meta);
            }

            items.push(DescriptionItem {
                tag: tag_text,
                value: value_text,
                meta,
            });
        }
    }
}

/// Extract the tag text from a LIST_ITEM_TAG node.
/// The tag node contains inline content (TEXT tokens, possibly BOLD etc.)
/// followed by COLON2 ("::"). We want everything before the COLON2.
fn extract_tag_text(tag_node: &orgize::SyntaxNode) -> String {
    let mut text = String::new();
    for element in tag_node.children_with_tokens() {
        match element {
            rowan::NodeOrToken::Token(token) => {
                if token.kind() == SyntaxKind::COLON2 {
                    break; // Stop before the "::"
                }
                text.push_str(token.text());
            }
            rowan::NodeOrToken::Node(node) => {
                // Inline markup nodes (BOLD, ITALIC, etc.) — collect their text
                text.push_str(&node.text().to_string());
            }
        }
    }
    text.trim().to_string()
}

/// Check whether a LIST syntax node looks like a description list.
fn is_description_list(list_node: &orgize::SyntaxNode) -> bool {
    for child in list_node.children() {
        if child.kind() != SyntaxKind::LIST_ITEM {
            continue;
        }
        if child.children().any(|c| c.kind() == SyntaxKind::LIST_ITEM_TAG) {
            return true;
        }
        let content = child.children().find(|c| c.kind() == SyntaxKind::LIST_ITEM_CONTENT);
        if let Some(cn) = content {
            let text = cn.text().to_string();
            if text.contains(" :: ") {
                return true;
            }
        }
    }
    false
}

/// Render a plain (non-description) list node as text lines.
fn render_plain_list_as_text(list_node: &orgize::SyntaxNode) -> String {
    let mut lines: Vec<String> = Vec::new();
    let mut counter = 0u32;

    for child in list_node.children() {
        if child.kind() != SyntaxKind::LIST_ITEM {
            continue;
        }
        counter += 1;

        // Detect ordered list by checking if the item's raw text starts
        // with a digit followed by '.'
        let raw = child.text().to_string();
        let trimmed = raw.trim_start();
        let is_ordered = trimmed.starts_with(|c: char| c.is_ascii_digit())
            && trimmed.contains('.');

        // Get the content text
        let mut item_text = String::new();
        let content = child.children().find(|c| c.kind() == SyntaxKind::LIST_ITEM_CONTENT);
        if let Some(cn) = content {
            for cc in cn.children() {
                if cc.kind() == SyntaxKind::PARAGRAPH {
                    item_text.push_str(&cc.text().to_string().trim().to_string());
                }
            }
        }

        if is_ordered {
            lines.push(format!("{}.  {}", counter, item_text));
        } else {
            lines.push(format!("- {}", item_text));
        }
    }

    lines.join("\n")
}

/// Extract the value text and any nested LIST nodes from a LIST_ITEM_CONTENT node.
///
/// The content node contains PARAGRAPH nodes (the value text) and possibly
/// nested LIST nodes (which hold meta-annotations or continuation items).
///
/// Returns (value_text, nested_list_nodes).
fn extract_content_text_and_nested_lists(
    content_node: &orgize::SyntaxNode,
) -> (String, Vec<orgize::SyntaxNode>) {
    let mut parts: Vec<(String, bool)> = Vec::new(); // (text, is_paragraph)
    let mut nested_lists: Vec<orgize::SyntaxNode> = Vec::new();

    for child in content_node.children() {
        if child.kind() == SyntaxKind::LIST {
            if is_description_list(&child) {
                nested_lists.push(child);
            } else {
                let list_text = render_plain_list_as_text(&child);
                if !list_text.is_empty() {
                    parts.push((list_text, false));
                }
            }
        } else if child.kind() == SyntaxKind::PARAGRAPH {
            let para_text = child.text().to_string();
            let trimmed_end = para_text.trim_end();
            if !trimmed_end.is_empty() {
                parts.push((trimmed_end.to_string(), true));
            }
        }
    }

    let mut joined = String::new();
    for (i, (text, is_para)) in parts.iter().enumerate() {
        if i > 0 {
            let prev_is_para = parts[i - 1].1;
            if *is_para && prev_is_para {
                joined.push_str("\n\n");
            } else {
                joined.push('\n');
            }
        }
        joined.push_str(text);
    }

    (joined.trim_start().to_string(), nested_lists)
}

// ─── Source block extraction ─────────────────────────────────────

/// Extract the content of `#+begin_src omn` blocks from a headline's body.
///
/// In orgize's rowan AST, source blocks are represented as nodes with
/// `SyntaxKind::SOURCE_BLOCK`. The raw text of the node looks like:
///
/// ```text
/// #+begin_src omn
/// ... content ...
/// #+end_src
/// ```
///
/// We find such nodes under the headline (skipping nested headlines),
/// check that the language is "omn", and extract the inner content
/// (everything between the begin/end lines).
fn extract_omn_src_blocks(headline: &Headline) -> Vec<String> {
    let mut blocks = Vec::new();
    collect_src_blocks_recursive(headline.syntax(), &mut blocks);
    blocks
}

/// Recursively collect SOURCE_BLOCK nodes, skipping nested Headline subtrees.
///
/// The caller passes `headline.syntax()` as `node`.  In orgize's rowan tree
/// the Headline's syntax node wraps its entire subtree: its direct children
/// include Section nodes (body content) **and** nested Headline nodes
/// (sub-headings).  We always skip any child that is a Headline so that
/// src blocks belonging to sub-headings are not collected here.
fn collect_src_blocks_recursive(
    node: &orgize::SyntaxNode,
    blocks: &mut Vec<String>,
) {
    for child in node.children() {
        // Skip nested headlines — their blocks belong to them, not us.
        if Headline::cast(child.clone()).is_some() {
            continue;
        }
        if child.kind() == SyntaxKind::SOURCE_BLOCK {
            if let Some(content) = extract_src_block_content(&child, "omn") {
                if !content.is_empty() {
                    blocks.push(content);
                }
            }
        } else {
            collect_src_blocks_recursive(&child, blocks);
        }
    }
}

/// Extract the inner content of a source block node, if its language matches.
///
/// The raw text of a SOURCE_BLOCK looks like:
/// ```text
/// #+begin_src omn
/// line1
/// line2
/// #+end_src
/// ```
///
/// We check the first line for `#+begin_src <lang>` (case-insensitive),
/// strip the first and last lines, and return the inner content.
fn extract_src_block_content(node: &orgize::SyntaxNode, lang: &str) -> Option<String> {
    let text = node.text().to_string();
    let mut lines: Vec<&str> = text.lines().collect();

    if lines.is_empty() {
        return None;
    }

    // Check the begin line: #+begin_src <lang> [optional params]
    let first_line = lines[0].trim().to_lowercase();
    let expected_prefix = format!("#+begin_src {}", lang.to_lowercase());
    if !first_line.starts_with(&expected_prefix) {
        return None;
    }

    // Skip blocks marked with :tangle no (org-babel convention)
    if first_line.contains(":tangle no") {
        return None;
    }

    // Remove the begin line
    lines.remove(0);

    // Remove any trailing #+end_src line(s).  orgize may or may not
    // include the end marker inside the SOURCE_BLOCK text.  We strip
    // lines from the end that are blank or contain #+end_src.
    while let Some(last) = lines.last() {
        let t = last.trim().to_lowercase();
        if t.is_empty() || t.starts_with("#+end_src") || t.starts_with("#+end_src") {
            lines.pop();
        } else {
            break;
        }
    }

    let content = lines.join("\n").trim_end().to_string();
    Some(content)
}

/// Extract tangle target from a `:header-args:omn:` value.
/// Looks for `:tangle <path>` within the string.
fn extract_tangle_target(header_args: &str) -> Option<String> {
    let idx = header_args.find(":tangle")?;
    let after = &header_args[idx + ":tangle".len()..];
    let trimmed = after.trim_start();
    let path = trimmed.split_whitespace().next()?;
    if path.is_empty() || path == "no" {
        None
    } else {
        Some(path.to_string())
    }
}

/// Insert a node into the tree at the correct nesting depth.
/// Finds the deepest rightmost child whose level < node.level.
fn insert_node(parent: &mut ElotNode, node: ElotNode) {
    if let Some(last_child) = parent.children.last_mut() {
        if last_child.level < node.level {
            insert_node(last_child, node);
            return;
        }
    }
    parent.children.push(node);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_headline_extraction() {
        let org_text = r#"
* Level 1 heading                                                  :tag1:
** Level 2 heading
*** Level 3 heading
** Another level 2
"#;
        let root = parse_to_elot(org_text);

        assert_eq!(root.level, 0);
        assert_eq!(root.title, "ROOT");
        assert_eq!(root.children.len(), 1, "one level-1 child");

        let l1 = &root.children[0];
        assert_eq!(l1.level, 1);
        assert_eq!(l1.title, "Level 1 heading");
        assert_eq!(l1.tags, vec!["tag1"]);
        assert_eq!(l1.children.len(), 2, "two level-2 children");

        let l2a = &l1.children[0];
        assert_eq!(l2a.level, 2);
        assert_eq!(l2a.title, "Level 2 heading");
        assert_eq!(l2a.children.len(), 1, "one level-3 child");

        let l3 = &l2a.children[0];
        assert_eq!(l3.level, 3);
        assert_eq!(l3.title, "Level 3 heading");
        assert_eq!(l3.children.len(), 0);

        let l2b = &l1.children[1];
        assert_eq!(l2b.level, 2);
        assert_eq!(l2b.title, "Another level 2");
    }

    #[test]
    fn test_empty_input() {
        let root = parse_to_elot("");
        assert_eq!(root.level, 0);
        assert_eq!(root.children.len(), 0);
    }

    #[test]
    fn test_underscores_preserved() {
        // This was broken in orga — underscores turned into subscripts.
        let org_text = "* BFO_0000001 entity\n";
        let root = parse_to_elot(org_text);
        assert_eq!(root.children[0].title, "BFO_0000001 entity");
    }

    #[test]
    fn test_property_drawers() {
        let org_text = r#"* My ontology
:PROPERTIES:
:ID:       my-onto
:ELOT-context-type: ontology
:ELOT-context-localname: my.owl
:ELOT-default-prefix: ex
:header-args:omn: :tangle ./output.omn :noweb yes
:END:
** Prefixes
:PROPERTIES:
:prefixdefs: yes
:END:
** Classes
:PROPERTIES:
:ID:       my-onto-class-hierarchy
:resourcedefs: yes
:END:
"#;
        let root = parse_to_elot(org_text);

        let l1 = &root.children[0];
        assert_eq!(l1.title, "My ontology");
        assert_eq!(l1.id, Some("my-onto".to_string()));
        assert_eq!(l1.elot_context_type, Some("ontology".to_string()));
        assert_eq!(l1.elot_context_localname, Some("my.owl".to_string()));
        assert_eq!(l1.elot_default_prefix, Some("ex".to_string()));
        assert_eq!(l1.tangle_target_omn, Some("./output.omn".to_string()));
        assert!(!l1.resourcedefs);
        assert!(!l1.prefixdefs);

        let prefixes = &l1.children[0];
        assert_eq!(prefixes.title, "Prefixes");
        assert!(prefixes.prefixdefs);
        assert!(!prefixes.resourcedefs);

        let classes = &l1.children[1];
        assert_eq!(classes.title, "Classes");
        assert_eq!(classes.id, Some("my-onto-class-hierarchy".to_string()));
        assert!(classes.resourcedefs);
        assert!(!classes.prefixdefs);
    }

    #[test]
    fn test_prefix_table_extraction() {
        let org_text = r#"* My ontology
:PROPERTIES:
:header-args:omn: :tangle ./out.omn :noweb yes
:END:
** Prefixes
:PROPERTIES:
:prefixdefs: yes
:END:
The ontology uses these prefixes.

#+name: prefix-table
| prefix | uri                                         |
|--------+---------------------------------------------|
| :      | http://purl.obolibrary.org/obo/bfo.owl#     |
| dc11:  | http://purl.org/dc/elements/1.1/            |
| obo:   | http://purl.obolibrary.org/obo/             |
| owl:   | http://www.w3.org/2002/07/owl#              |

** Classes
:PROPERTIES:
:ID:       my-onto-class-hierarchy
:resourcedefs: yes
:END:
"#;
        let root = parse_to_elot(org_text);

        let prefixes_node = &root.children[0].children[0];
        assert_eq!(prefixes_node.title, "Prefixes");
        assert!(prefixes_node.prefixdefs);

        let prefixes = &prefixes_node.prefixes;
        assert!(prefixes.len() >= 4, "expected at least 4 prefix entries, got {}", prefixes.len());

        // Check a few entries
        assert!(prefixes.iter().any(|p| p.prefix == ":" && p.uri == "http://purl.obolibrary.org/obo/bfo.owl#"),
            "expected default prefix entry, got: {:?}", prefixes);
        assert!(prefixes.iter().any(|p| p.prefix == "dc11:" && p.uri == "http://purl.org/dc/elements/1.1/"),
            "expected dc11 prefix entry, got: {:?}", prefixes);
        assert!(prefixes.iter().any(|p| p.prefix == "obo:" && p.uri == "http://purl.obolibrary.org/obo/"),
            "expected obo prefix entry, got: {:?}", prefixes);
        assert!(prefixes.iter().any(|p| p.prefix == "owl:" && p.uri == "http://www.w3.org/2002/07/owl#"),
            "expected owl prefix entry, got: {:?}", prefixes);

        // Classes heading should NOT have prefixes
        let classes = &root.children[0].children[1];
        assert!(classes.prefixes.is_empty());
    }

    #[test]
    fn test_prefix_table_no_table() {
        // :prefixdefs: yes but no table present → empty prefixes
        let org_text = r#"* Ontology
** Prefixes
:PROPERTIES:
:prefixdefs: yes
:END:
No table here.
"#;
        let root = parse_to_elot(org_text);
        let prefixes_node = &root.children[0].children[0];
        assert!(prefixes_node.prefixdefs);
        assert!(prefixes_node.prefixes.is_empty());
    }

    #[test]
    fn test_tangle_target_extraction() {
        assert_eq!(
            extract_tangle_target(":tangle ./foo.omn :noweb yes"),
            Some("./foo.omn".to_string())
        );
        assert_eq!(
            extract_tangle_target(":tangle no"),
            None
        );
        assert_eq!(
            extract_tangle_target(":noweb yes"),
            None
        );
    }

    #[test]
    fn test_description_list_basic() {
        let org_text = r#"* Classes
:PROPERTIES:
:ID:       my-onto-class-hierarchy
:resourcedefs: yes
:END:
** "entity"@en (obo:BFO_0000001)
 - dc11:identifier :: 001-BFO
 - skos:definition :: "(Elucidation) An entity is anything that exists or has existed or will exist"@en
 - skos:example :: "Julius Caesar"@en
"#;
        let root = parse_to_elot(org_text);
        let entity = &root.children[0].children[0];
        assert_eq!(entity.title, "\"entity\"@en (obo:BFO_0000001)");

        let descs = &entity.descriptions;
        assert_eq!(descs.len(), 3, "expected 3 description items, got {}: {:?}", descs.len(), descs);

        assert_eq!(descs[0].tag, "dc11:identifier");
        assert_eq!(descs[0].value, "001-BFO");
        assert!(descs[0].meta.is_empty());

        assert_eq!(descs[1].tag, "skos:definition");
        assert!(descs[1].value.contains("Elucidation"));

        assert_eq!(descs[2].tag, "skos:example");
        assert!(descs[2].value.contains("Julius Caesar"));
    }

    #[test]
    fn test_description_list_with_meta_annotations() {
        let org_text = r#"* Classes
:PROPERTIES:
:ID:       my-onto-class-hierarchy
:resourcedefs: yes
:END:
** "entity"@en (obo:BFO_0000001)
 - skos:definition :: "(Elucidation) An entity is anything that exists"@en
   - rdfs:comment :: this is an example of a meta-annotation
   - rdfs:comment :: this is a second meta-annotation
 - skos:example :: "Julius Caesar"@en
"#;
        let root = parse_to_elot(org_text);
        let entity = &root.children[0].children[0];
        let descs = &entity.descriptions;

        assert_eq!(descs.len(), 2, "expected 2 top-level description items, got {}: {:?}", descs.len(), descs);

        // First item: skos:definition with 2 meta-annotations
        assert_eq!(descs[0].tag, "skos:definition");
        assert!(descs[0].value.contains("Elucidation"));
        assert_eq!(descs[0].meta.len(), 2, "expected 2 meta-annotations, got {}: {:?}", descs[0].meta.len(), descs[0].meta);
        assert_eq!(descs[0].meta[0].tag, "rdfs:comment");
        assert_eq!(descs[0].meta[0].value, "this is an example of a meta-annotation");
        assert_eq!(descs[0].meta[1].tag, "rdfs:comment");
        assert_eq!(descs[0].meta[1].value, "this is a second meta-annotation");

        // Second item: skos:example, no meta
        assert_eq!(descs[1].tag, "skos:example");
        assert!(descs[1].meta.is_empty());
    }

    #[test]
    fn test_description_list_multiline_value() {
        // SubClassOf with continuation line (indented)
        let org_text = r#"* Classes
:PROPERTIES:
:ID:       my-onto-class-hierarchy
:resourcedefs: yes
:END:
** "site"@en (obo:BFO_0000029)
 - SubClassOf :: obo:BFO_0000176 only 
          (obo:BFO_0000029 or obo:BFO_0000040)
"#;
        let root = parse_to_elot(org_text);
        let site = &root.children[0].children[0];
        let descs = &site.descriptions;

        assert_eq!(descs.len(), 1, "expected 1 description item, got {}: {:?}", descs.len(), descs);
        assert_eq!(descs[0].tag, "SubClassOf");
        // The value should contain both lines with continuation indentation preserved
        assert!(descs[0].value.contains("obo:BFO_0000176"), "value should contain 'obo:BFO_0000176', got: {}", descs[0].value);
        assert!(descs[0].value.contains("obo:BFO_0000029"), "value should contain 'obo:BFO_0000029', got: {}", descs[0].value);
        // Verify the continuation line starts with whitespace (original indentation preserved)
        let lines: Vec<&str> = descs[0].value.lines().collect();
        assert!(lines.len() >= 2, "expected at least 2 lines, got: {:?}", lines);
        assert!(lines[1].starts_with(' '), "continuation line should preserve leading spaces, got: {:?}", lines[1]);
    }

    #[test]
    fn test_no_descriptions_on_prefix_heading() {
        // A heading with :prefixdefs: should not pick up table content as descriptions
        let org_text = r#"* Ontology
** Prefixes
:PROPERTIES:
:prefixdefs: yes
:END:
| prefix | uri                    |
|--------+------------------------|
| obo:   | http://example.org/obo |
** Classes
:PROPERTIES:
:ID:       my-onto-class-hierarchy
:resourcedefs: yes
:END:
"#;
        let root = parse_to_elot(org_text);
        let prefixes_node = &root.children[0].children[0];
        // The prefix heading should have no description items
        assert!(prefixes_node.descriptions.is_empty(),
            "prefix heading should have no descriptions, got: {:?}", prefixes_node.descriptions);
    }

    #[test]
    fn test_omn_src_block_extraction() {
        let org_text = r#"* bfo.owl
:PROPERTIES:
:ID:       bfo.owl
:ELOT-context-type: ontology
:header-args:omn: :tangle ./bfo.omn :noweb yes
:END:

#+begin_src omn
## Test file for new ELOT
#+end_src

** Prefixes
:PROPERTIES:
:prefixdefs: yes
:END:
"#;
        let root = parse_to_elot(org_text);
        let onto = &root.children[0];
        assert_eq!(onto.omn_src_blocks.len(), 1,
            "expected 1 omn src block, got {}: {:?}", onto.omn_src_blocks.len(), onto.omn_src_blocks);
        assert_eq!(onto.omn_src_blocks[0], "## Test file for new ELOT");
    }

    #[test]
    fn test_omn_src_block_multiline() {
        let org_text = r#"* onto
#+begin_src omn
## Line 1
## Line 2
## Line 3
#+end_src
"#;
        let root = parse_to_elot(org_text);
        let onto = &root.children[0];
        assert_eq!(onto.omn_src_blocks.len(), 1);
        assert!(onto.omn_src_blocks[0].contains("Line 1"));
        assert!(onto.omn_src_blocks[0].contains("Line 2"));
        assert!(onto.omn_src_blocks[0].contains("Line 3"));
    }

    #[test]
    fn test_omn_src_block_not_other_languages() {
        // A #+begin_src emacs-lisp block should NOT be extracted
        let org_text = r#"* onto
#+begin_src emacs-lisp
(message "hello")
#+end_src

#+begin_src omn
## this one should be extracted
#+end_src
"#;
        let root = parse_to_elot(org_text);
        let onto = &root.children[0];
        assert_eq!(onto.omn_src_blocks.len(), 1,
            "expected only the omn block, got {}: {:?}", onto.omn_src_blocks.len(), onto.omn_src_blocks);
        assert!(onto.omn_src_blocks[0].contains("this one should be extracted"));
    }

    #[test]
    fn test_omn_src_block_not_from_child_headline() {
        // A src block under a child headline should NOT appear on the parent
        let org_text = r#"* parent
#+begin_src omn
## parent block
#+end_src
** child
#+begin_src omn
## child block
#+end_src
"#;
        let root = parse_to_elot(org_text);
        let parent = &root.children[0];
        assert_eq!(parent.omn_src_blocks.len(), 1,
            "parent should have 1 block, got {}: {:?}", parent.omn_src_blocks.len(), parent.omn_src_blocks);
        assert!(parent.omn_src_blocks[0].contains("parent block"));

        let child = &parent.children[0];
        assert_eq!(child.omn_src_blocks.len(), 1,
            "child should have 1 block, got {}: {:?}", child.omn_src_blocks.len(), child.omn_src_blocks);
        assert!(child.omn_src_blocks[0].contains("child block"));
    }

    #[test]
    fn test_descriptions_not_leaked_to_parent() {
        // Description lists under child headlines must NOT appear on the parent.
        let org_text = r#"* bfo.owl
:PROPERTIES:
:ID:       bfo.owl
:ELOT-context-type: ontology
:END:
** bfo.owl ontology (obo:bfo.owl)
:PROPERTIES:
:ID:       bfo.owl-ontology-declaration
:resourcedefs: yes
:END:
 - dc11:contributor :: Alan Ruttenberg
 - dc11:contributor :: Barry Smith
** Classes
:PROPERTIES:
:ID:       bfo.owl-class-hierarchy
:resourcedefs: yes
:END:
*** "entity"@en (obo:BFO_0000001)
 - skos:definition :: "(Elucidation) An entity is anything"@en
"#;
        let root = parse_to_elot(org_text);
        let bfo = &root.children[0];
        assert_eq!(bfo.title, "bfo.owl");
        // The parent should have NO descriptions — they belong to children
        assert!(bfo.descriptions.is_empty(),
            "parent 'bfo.owl' should have no descriptions, got: {:?}", bfo.descriptions);

        let onto = &bfo.children[0];
        assert_eq!(onto.descriptions.len(), 2,
            "ontology child should have 2 descriptions, got: {:?}", onto.descriptions);

        let entity = &bfo.children[1].children[0];
        assert_eq!(entity.descriptions.len(), 1,
            "entity should have 1 description, got: {:?}", entity.descriptions);
    }

    #[test]
    fn test_omn_src_block_tangle_no_skipped() {
        // A #+begin_src omn block with :tangle no should be excluded
        let org_text = r#"* onto
#+begin_src omn :tangle no
## This should NOT be extracted
#+end_src

#+begin_src omn
## This SHOULD be extracted
#+end_src
"#;
        let root = parse_to_elot(org_text);
        let onto = &root.children[0];
        assert_eq!(onto.omn_src_blocks.len(), 1,
            "expected only the non-:tangle-no block, got {}: {:?}",
            onto.omn_src_blocks.len(), onto.omn_src_blocks);
        assert!(onto.omn_src_blocks[0].contains("SHOULD be extracted"));
    }

    #[test]
    fn test_description_list_numbered_list_in_value() {
        let org_text = r#"* Classes
:PROPERTIES:
:ID:       my-onto-class-hierarchy
:resourcedefs: yes
:END:
** ControlArea (cim:ControlArea)
 - rdfs:comment :: "The following general principles apply to ControlArea:
          1.  Orientation for net interchange is positive for import.
          2.  Net interchange is determined by summing flows.
          3.  A tie between two areas must be modelled in both.
          4.  Normal orientation of Terminal flow is positive into equipment."@en
"#;
        let root = parse_to_elot(org_text);
        let ca = &root.children[0].children[0];
        let descs = &ca.descriptions;

        assert_eq!(descs.len(), 1,
            "expected 1 description item, got {}: {:?}", descs.len(), descs);
        assert_eq!(descs[0].tag, "rdfs:comment");
        assert!(descs[0].value.contains("Orientation for net interchange"),
            "numbered list item 1 missing from value: {:?}", descs[0].value);
        assert!(descs[0].value.contains("Normal orientation"),
            "numbered list item 4 missing from value: {:?}", descs[0].value);
        assert!(descs[0].meta.is_empty(),
            "numbered list should not produce meta-annotations: {:?}", descs[0].meta);
    }

    #[test]
    fn test_description_list_blank_line_preserved() {
        // A description value that spans two paragraphs (separated by a blank
        // line in the Org source) should retain the double newline (\n\n).
        let org_text = r#"* Classes
:PROPERTIES:
:ID:       my-onto-class-hierarchy
:resourcedefs: yes
:END:
** "BoatReservation"@en (schema:BoatReservation)
 - rdfs:comment :: A reservation for boat travel.

   Note: This type is for information about actual reservations.
"#;
        let root = parse_to_elot(org_text);
        let boat = &root.children[0].children[0];
        let descs = &boat.descriptions;

        assert_eq!(descs.len(), 1,
            "expected 1 description item, got {}: {:?}", descs.len(), descs);
        assert_eq!(descs[0].tag, "rdfs:comment");
        // The value must contain \n\n (double newline) between the paragraphs
        assert!(descs[0].value.contains("\n\n"),
            "expected double newline in value, got: {:?}", descs[0].value);
        assert!(descs[0].value.contains("A reservation for boat travel."));
        assert!(descs[0].value.contains("Note: This type"));
    }
}
