# Makefile for elot-cli — friendly aliases for npm commands.
# Run "make help" to see available targets.

.PHONY: help all install bundle build watch test test-syntax test-html test-html-post test-html-integration test-html-all test-db test-db-lang test-db-read test-db-write test-db-paths test-db-parsers test-db-prefixes test-db-rq-cache test-db-ttl-robot test-db-rq-live test-db-roundtrip test-db-org test-db-cli-e2e test-db-golden test-db-bridge test-db-hover-token test-db-hover-render test-db-decorations test-db-decorations-scan test-db-resolve test-settings test-settings-active-sources test-settings-reorder test-settings-cli-runner test-settings-persist test-label-lookup test-label-lookup-items test-views test-views-source-items test-electron compile-test clean package cli

all: install bundle test test-html-all package ## Full build from scratch: install → bundle → test → package

help: ## Show this help
	@grep -E '^[a-zA-Z_-]+:.*?## ' $(MAKEFILE_LIST) | \
		awk 'BEGIN {FS = ":.*?## "}; {printf "  \033[36m%-15s\033[0m %s\n", $$1, $$2}'

install: ## Install Node dependencies (run once after cloning)
	npm install

bundle: ## Full build: compile Rust→WASM, generate Peggy parser, then bundle TypeScript→dist/
	npm run bundle

build-parser: ## Regenerate the Peggy parser from owl-manchester.peggy
	npm run build:parser

build: ## Type-check TypeScript only (no bundling)
	npm run build

watch: ## Watch mode — recompile TypeScript on every file change
	npm run watch

test: ## Run the golden-file test suite
	npm test

test-syntax: ## Run the OMN syntax-checking tests (Peggy grammar)
	npm run test:syntax

test-html: ## Run the HTML export preprocessing tests
	npm run test:html

test-html-post: ## Run the HTML post-processing tests (outline divs, CSS classes)
	npm run test:html-post

test-html-integration: ## Run the HTML export integration tests (requires Pandoc)
	npm run test:html-integration

test-html-all: ## Run all HTML export tests
	npm run test:html-all

test-db-lang: ## Run pure language-picker tests (Step 2.2.1)
	npm run test:db-lang

test-db-read: ## Run ElotDb read-path tests (Step 2.2.1)
	npm run test:db-read

test-db-write: ## Run ElotDb writer tests (Step 2.2.2)
	npm run test:db-write

test-db-paths: ## Run DB-path resolver tests (Step 2.2.2)
	npm run test:db-paths

test-db-parsers: ## Run CSV/TSV/JSON parser tests (Step 2.2.3)
	npm run test:db-parsers

test-db-prefixes: ## Run prefix-harvest tests (Step 2.2.4)
	npm run test:db-prefixes

test-db-rq-cache: ## Run .rq cache-freshness tests (Step 2.2.4)
	npm run test:db-rq-cache

test-db-ttl-robot: ## Run TTL-via-ROBOT end-to-end tests (Step 2.2.4; skips without ROBOT)
	npm run test:db-ttl-robot

test-db-rq-live: ## Run live SPARQL-endpoint RQ test (skipped unless ROBOT + ELOT_TEST_NETWORK=1)
	npm run test:db-rq-live

test-db-roundtrip: ## Run parser -> updateSource round-trip tests (Step 2.2.3)
	npm run test:db-roundtrip

test-db-org: ## Run Org parser adaptor tests (Step 2.2.5)
	npm run test:db-org

test-db-cli-e2e: ## Run end-to-end CLI black-box tests (Step 2.2.5)
	npm run test:db-cli-e2e

test-db-golden: ## Run byte-identical golden round-trip tests vs Elisp writer (Step 2.2.5 slice 3a)
	npm run test:db-golden

test-db-bridge: ## Run ElotDbBridge tests (Step 2.3.1)
	npm run test:db-bridge

test-db-hover-token: ## Run DB hover token-detection tests (Step 2.3.2)
	npm run test:db-hover-token

test-db-hover-render: ## Run DB hover Markdown renderer tests (Step 2.3.2)
	npm run test:db-hover-render

test-db-decorations: ## Run all DB-decoration tests (Step 2.3.3)
	npm run test:db-decorations

test-db-decorations-scan: ## Run dbDecorations token-scanner tests (Step 2.3.3)
	npm run test:db-decorations-scan

test-settings: ## Run all settings-normaliser tests (Step 2.3.1+)
	npm run test:settings

test-settings-active-sources: ## Run activeLabelSources / preferredLanguages normaliser tests (Step 2.3.1)
	npm run test:settings-active-sources

test-settings-reorder: ## Run reorder/set-arithmetic helper tests (Step 2.3.4)
	npm run test:settings-reorder

test-settings-cli-runner: ## Run cliRunner argv-builder tests (Step 2.3.6)
	npm run test:settings-cli-runner

test-settings-persist: ## Run persist-plan helper tests (Step 2.3.7b)
	npm run test:settings-persist

test-label-lookup: ## Run all label-lookup tests (Step 2.4.1)
	npm run test:label-lookup

test-label-lookup-items: ## Run label-lookup item-builder unit tests (Step 2.4.1)
	npm run test:label-lookup-items

test-views: ## Run all TreeView builder tests (Step 2.3.7a)
	npm run test:views

test-views-source-items: ## Run sourceItems builder tests (Step 2.3.7a)
	npm run test:views-source-items

test-db-resolve: ## Run extension DB-path resolver tests (Step 2.3.1)
	npm run test:db-resolve

test-db: ## Run all DB tests
	npm run test:db

compile-test: ## Compile electron tests under dist-test/ (Step 2.3.5b)
	npm run compile:test

test-electron: ## Run @vscode/test-electron smoke tests (Step 2.3.5b; downloads VS Code on first run)
	npm run test:electron

cli: ## Run CLI on examples/bfo-core.org (dev mode, no bundling needed)
	npm run elot -- examples/bfo-core.org

package: ## Build a .vsix for VS Code (runs bundle automatically)
	npx @vscode/vsce package

clean: ## Remove build artifacts (dist/, src/wasm/, and generated parser)
	rm -rf dist src/wasm src/omnParser.js
