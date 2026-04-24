# Makefile for elot-cli — friendly aliases for npm commands.
# Run "make help" to see available targets.

.PHONY: help all install bundle build watch test test-syntax test-html test-html-post test-html-integration test-html-all test-db test-db-lang test-db-read test-db-write test-db-paths test-db-parsers test-db-roundtrip clean package cli

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

test-db-roundtrip: ## Run parser -> updateSource round-trip tests (Step 2.2.3)
	npm run test:db-roundtrip

test-db: ## Run all DB tests (lang + read + write + paths + parsers + roundtrip)
	npm run test:db

cli: ## Run CLI on examples/bfo-core.org (dev mode, no bundling needed)
	npm run elot -- examples/bfo-core.org

package: ## Build a .vsix for VS Code (runs bundle automatically)
	npx @vscode/vsce package

clean: ## Remove build artifacts (dist/, src/wasm/, and generated parser)
	rm -rf dist src/wasm src/omnParser.js
