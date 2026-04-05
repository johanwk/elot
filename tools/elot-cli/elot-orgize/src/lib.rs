// elot-orgize/src/lib.rs
//
// WASM entry point: accepts Org text, returns ElotNode JSON.
// For now, this is a skeleton that parses with orgize and returns
// a minimal root node, proving the WASM pipeline works end-to-end.

use wasm_bindgen::prelude::*;

mod types;
mod parse;

/// Parse an Org-mode string and return an ELOT hierarchy as JSON.
///
/// This is the single entry point called from TypeScript.
/// Returns a JSON string representing an `ElotNode` tree.
#[wasm_bindgen]
pub fn parse_org_to_elot(org_text: &str) -> String {
    let root = parse::parse_to_elot(org_text);
    serde_json::to_string(&root).unwrap()
}
