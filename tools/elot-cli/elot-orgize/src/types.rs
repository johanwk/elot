// elot-orgize/src/types.rs
//
// Rust structs that mirror the TypeScript ElotNode / DescriptionItem types.
// These serialize to JSON that the TypeScript pipeline can consume directly.

use serde::Serialize;

/// A single description-list item: tag :: value, with optional meta-annotations.
#[derive(Serialize, Clone, Debug, Default)]
#[serde(rename_all = "camelCase")]
pub struct DescriptionItem {
    pub tag: String,
    pub value: String,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub meta: Vec<DescriptionItem>,
}

/// A prefix mapping: prefix name → namespace URI.
#[derive(Serialize, Clone, Debug)]
pub struct PrefixEntry {
    pub prefix: String,
    pub uri: String,
}

/// One node in the ELOT hierarchy (mirrors TypeScript ElotNode exactly).
#[derive(Serialize, Clone, Debug)]
#[serde(rename_all = "camelCase")]
pub struct ElotNode {
    pub level: u32,
    pub title: String,
    pub tags: Vec<String>,

    // Identity
    #[serde(skip_serializing_if = "Option::is_none")]
    pub uri: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub label: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub rdf_type: Option<String>,

    // Context properties (from property drawers)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub id: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub elot_context_type: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub elot_context_localname: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub elot_default_prefix: Option<String>,

    pub resourcedefs: bool,
    pub prefixdefs: bool,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub tangle_target_omn: Option<String>,

    // Content
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub prefixes: Vec<PrefixEntry>,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub descriptions: Vec<DescriptionItem>,

    /// Raw content of #+begin_src omn blocks found under this headline.
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub omn_src_blocks: Vec<String>,

    pub children: Vec<ElotNode>,
}

impl Default for ElotNode {
    fn default() -> Self {
        ElotNode {
            level: 0,
            title: String::new(),
            tags: Vec::new(),
            uri: None,
            label: None,
            rdf_type: None,
            id: None,
            elot_context_type: None,
            elot_context_localname: None,
            elot_default_prefix: None,
            resourcedefs: false,
            prefixdefs: false,
            tangle_target_omn: None,
            prefixes: Vec::new(),
            descriptions: Vec::new(),
            omn_src_blocks: Vec::new(),
            children: Vec::new(),
        }
    }
}
