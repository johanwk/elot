# ELOT-Exporter

ELOT-Exporter is a Java-based command-line tool designed to convert OWL ontologies into a structured Org-mode format. It's specifically tailored for users who want to manage, browse, and edit ontologies within Emacs using ELOT (Emacs Literate Ontology Tool).

## Purpose

The primary goal of `elot-exporter` is to provide a human-readable, Org-mode-friendly representation of an OWL ontology. It uses the [OWLAPI](https://github.com/owlcs/owlapi) to parse ontologies and transforms them into a hierarchical Org-mode structure, making use of Manchester Syntax for axiom rendering.

## Key Features

- **Org-mode Hierarchies**: Automatically generates nested Org-mode headlines representing the class and property hierarchies.
- **Manchester Syntax**: Axioms are rendered using Manchester Syntax for maximum readability.
- **Logging and Verbosity**: Provides detailed statistics and processing logs via CLI flags (`-v`, `--debug`).
- **Comprehensive Coverage**: Supports classes, object properties, data properties, annotation properties, datatypes, and individuals.
- **Prefix Management**: Generates Org-mode source blocks for prefix declarations, compatible with Emacs SPARQL and OWL modes.
- **Checksum Verification**: Protects against accidental overwrites of user-edited files by including a SHA-256 checksum in the generated Org-mode file.

## Checksum & File Protection

To prevent accidental data loss, `elot-exporter` includes a checksum mechanism:

1.  **Generation**: Every generated `.org` file contains a line like `#+ELOT_EXPORTER_CHECKSUM: <hash>`. This hash is a SHA-256 checksum of the file's content (excluding the checksum line itself).
2.  **Verification**: When running the exporter with an existing output file, it calculates the checksum of the current file content.
3.  **Protection**:
    - If the calculated checksum matches the one stored in the file, the file is considered "pristine" and will be safely overwritten.
    - If the checksums do not match, it means the file has been manually edited. The exporter will issue an error and refuse to overwrite the file.
4.  **Overriding**: Use the `-F` or `--force` flag to force an overwrite even if the file has been modified.

## Architecture & Design

The tool is designed with a modular architecture to handle the complexities of OWL ontologies and Org-mode formatting:

### Core Components

- **`ElotExporter`**: The main entry point. Handles CLI argument parsing (using Picocli), logging setup, and orchestrates the loading and formatting process.
- **`OntologyLoader`**: Responsible for loading OWL ontologies from various formats (RDF/XML, Turtle, Manchester Syntax, etc.) using the OWLAPI. It also uses `XMLCatalogResolver` to register an `AutoIRIMapper` for local files, enabling resolution of local imports.
- **`OrgModeFormatter`**: The central coordinator for the export process. It initializes the formatting context and delegates specific tasks to specialized formatter components.
- **`FormattingContext`**: A shared object that holds the `OWLOntology`, `PrefixManager`, `ManchesterOWLSyntaxOWLObjectRendererImpl`, and `TaxonomyBuilder`. It ensures consistent state across all formatters.
- **`TaxonomyBuilder`**: Pre-computes the class and property hierarchies to enable efficient tree-based rendering in Org-mode.

### Specialized Formatters

The formatting logic is split into several focused classes:

- **`AxiomFormatter`**: Renders logical axioms using Manchester Syntax.
- **`AnnotationFormatter`**: Formats entity annotations (e.g., labels, comments, custom metadata).
- **`HierarchyFormatter`**: Generates the nested Org-mode structure for classes and properties.
- **`IndividualFormatter`**: Handles the export of named individuals and their types/assertions.
- **`DatatypeFormatter`**: Formats custom and built-in datatypes.
- **`HeaderSectionGenerator` & `PrefixSectionGenerator`**: Create the Org-mode file headers, context properties, and prefix blocks.

## Developer Information

- **Author**: Johan W. Klüwer (johan.w.kluwer@gmail.com)

### Prerequisites

- Java 17 or higher
- Maven 3.6+

### Building the Project

To build the executable JAR file:

```bash
mvn clean package
```

The resulting JAR will be located in the `target/` directory: `target/elot-exporter-0.9.jar`.

### Running the Tool

You can run the exporter directly using Java:

```bash
java -jar target/elot-exporter-0.9.jar path/to/ontology.owl -o output.org
```

Use the `-v` flag for statistics and basic info logs, or `--debug` (or `-vv`) for detailed processing and debugging logs.

See `elot-exporter.1` for the manual page.

### Extending the Exporter

If you need to add support for new Org-mode features or change how certain OWL constructs are rendered:

1.  **Modify existing formatters**: Most rendering logic is contained within the `com.elotexport.*Formatter` classes.
2.  **Add a new formatter**: If you're adding a significant new feature (e.g., exporting SWRL rules), create a new formatter class, add it to the `FormattingContext` if necessary, and integrate it into `OrgModeFormatter.generateFullOutput()`.
3.  **Update `OntologyConstants`**: Shared string constants, markers, and Org-mode property names should be kept in `OntologyConstants.java` for consistency.

## License

This project is licensed under the GNU General Public License v3 - see the [LICENSE](LICENSE) file for details.
