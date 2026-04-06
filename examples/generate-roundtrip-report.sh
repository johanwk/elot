#!/usr/bin/env bash
# generate-roundtrip-report.sh — Consume per-ontology JSON reports and produce roundtrip-report.md
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPORTS_DIR="$SCRIPT_DIR/reports"
REPORT="$SCRIPT_DIR/roundtrip-report.md"

cat > "$REPORT" <<'HEADER'
# ELOT Round-Trip Test Results

Each ontology below was downloaded, converted to ELOT Org format via
`elot-exporter.jar`, tangled back to OWL Manchester Syntax via
`elot-tangle`, and compared to the original using `robot diff`.

| Ontology | Source | Processed | ELOT Version | Status | Diff Lines |
|----------|--------|-----------|--------------|--------|------------|
HEADER

for report in "$REPORTS_DIR"/*.json; do
    [[ -f "$report" ]] || continue

    # Minimal JSON parsing with grep/sed (no jq dependency required,
    # but jq would be cleaner if available)
    get() { grep "\"$1\"" "$report" | sed 's/.*: *"\?\([^",}]*\)"\?.*/\1/'; }

    id=$(get id)
    source_uri=$(get source_uri)
    timestamp=$(get timestamp)
    elot_version=$(get elot_version)
    diff_status=$(get diff_status)
    diff_lines=$(get diff_lines)

    if [[ "$diff_status" == "identical" ]]; then
        status_badge="✅ identical"
    elif [[ "$diff_status" == "differences" ]]; then
        status_badge="⚠️ differences"
    else
        status_badge="❌ $diff_status"
    fi

    # Link to diff file if it exists
    diff_display="$diff_lines"
    if [[ "$diff_lines" -gt 0 ]] 2>/dev/null; then
        diff_display="[$diff_lines](reports/$id.diff)"
    fi

    echo "| $id | $source_uri | $timestamp | $elot_version | $status_badge | $diff_display |" >> "$REPORT"
done

cat >> "$REPORT" <<'FOOTER'

---

*Generated automatically by `make report`.*
FOOTER

echo "roundtrip-report.md generated at $REPORT"
