# Makefile for elot-cli — friendly aliases for npm commands.
# Run "make help" to see available targets.

.PHONY: help all install bundle build watch test test-syntax clean package cli

all: install bundle test package ## Full build from scratch: install → bundle → test → package

help: ## Show this help
	@grep -E '^[a-zA-Z_-]+:.*?## ' $(MAKEFILE_LIST) | \
		awk 'BEGIN {FS = ":.*?## "}; {printf "  \033[36m%-15s\033[0m %s\n", $$1, $$2}'

install: ## Install Node dependencies (run once after cloning)
	npm install

bundle: ## Full build: compile Rust→WASM then bundle TypeScript→dist/
	npm run bundle

build: ## Type-check TypeScript only (no bundling)
	npm run build

watch: ## Watch mode — recompile TypeScript on every file change
	npm run watch

test: ## Run the golden-file test suite
	npm test

test-syntax: ## Run the OMN syntax-checking tests (Peggy grammar)
	npm run test:syntax

cli: ## Run CLI on examples/bfo-core.org (dev mode, no bundling needed)
	npm run elot -- examples/bfo-core.org

package: ## Build a .vsix for VS Code (runs bundle automatically)
	npx @vscode/vsce package

clean: ## Remove build artifacts (dist/ and src/wasm/)
	rm -rf dist src/wasm
