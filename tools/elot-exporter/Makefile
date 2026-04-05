# Makefile for elot-exporter — friendly aliases for Maven commands.
# Run "make help" to see available targets.

VERSION  := 0.10.0
JAR      := target/elot-exporter-$(VERSION).jar

.PHONY: help all build test clean run

all: build test ## Full build from scratch: compile + test

help: ## Show this help
	@grep -E '^[a-zA-Z_-]+:.*?## ' $(MAKEFILE_LIST) | \
		awk 'BEGIN {FS = ":.*?## "}; {printf "  \033[36m%-15s\033[0m %s\n", $$1, $$2}'

build: ## Compile and package the shaded JAR (mvn clean package)
	mvn clean package -q

test: ## Run the JUnit test suite
	mvn test -q

run: $(JAR) ## Run elot-exporter (pass ARGS, e.g. make run ARGS="ontology.owl -o out.org")
	java -jar $(JAR) $(ARGS)

clean: ## Remove build artifacts (target/)
	mvn clean -q
