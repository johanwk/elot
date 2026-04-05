/*
 * Copyright (C) 2024-2025 Johan W. Klüwer <johan.w.kluwer@gmail.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 */

package com.elotexport;

import java.io.PrintStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.concurrent.Callable;
import org.semanticweb.owlapi.model.*;
import picocli.CommandLine;
import picocli.CommandLine.Command;
import picocli.CommandLine.Option;
import picocli.CommandLine.Parameters;

@Command(name = "elot-exporter", mixinStandardHelpOptions = true, versionProvider = ElotExporter.VersionProvider.class,
        description = "Exports an OWL ontology to Org-mode format.")
public class ElotExporter implements Callable<Integer> {

    @Parameters(index = "0", description = "Path to the ontology file, or '-' to read from stdin.")
    private String ontologyPath;

    @Option(names = {"-o", "--output"}, description = "Optional switch to specify an explicit output filename.")
    private String outputFile;

    @Option(names = {"--tangle-target"}, description = "The OMN tangle target filename (default: <ontology-local-name>.omn).")
    private String tangleTarget;

    @Option(names = {"-F", "--force"}, description = "Force overwrite if the output file has been edited.")
    private boolean force;

    @Option(names = {"-v", "--verbose"}, description = "Verbose mode. Use -v for INFO, -vv or --debug for DEBUG logs.")
    private boolean[] verbosity = new boolean[0];

    @Option(names = {"--debug"}, description = "Debug mode (same as -vv).")
    private boolean debug;

    public static void main(String[] args) {
        // Pre-parse for verbosity to set SLF4J properties before loggers are initialized
        int verbosityLevel = 0;
        for (String arg : args) {
            if (arg.equals("-v") || arg.equals("--verbose")) verbosityLevel++;
            else if (arg.equals("-vv") || arg.equals("--debug")) verbosityLevel += 2;
            else if (arg.startsWith("-") && !arg.startsWith("--")) {
                for (int i = 1; i < arg.length(); i++) {
                    if (arg.charAt(i) == 'v') verbosityLevel++;
                }
            }
        }
        
        if (verbosityLevel == 1) {
            System.setProperty("org.slf4j.simpleLogger.defaultLogLevel", "info");
        } else if (verbosityLevel >= 2) {
            System.setProperty("org.slf4j.simpleLogger.defaultLogLevel", "debug");
        } else {
            System.setProperty("org.slf4j.simpleLogger.defaultLogLevel", "warn");
        }
        // Force slf4j-simple to output to stderr (which is its default, but let's be sure)
        System.setProperty("org.slf4j.simpleLogger.logFile", "System.err");
        System.setProperty("org.slf4j.simpleLogger.showThreadName", "false");
        System.setProperty("org.slf4j.simpleLogger.showLogName", "false");
        System.setProperty("org.slf4j.simpleLogger.showShortLogName", "true");
        System.setProperty("org.slf4j.simpleLogger.levelInBrackets", "true");

        try {
            System.setOut(new PrintStream(System.out, true, "UTF-8"));
        } catch (Exception e) {
            e.printStackTrace();
        }

        int exitCode = new CommandLine(new ElotExporter()).execute(args);
        System.exit(exitCode);
    }

    @Override
    public Integer call() {
        try {
            boolean verbose = (verbosity != null && verbosity.length > 0) || debug;
            // -- Ontology processing -----------------------------------------------------------
            OntologyLoader loader = new OntologyLoader(ontologyPath);
            if (verbose) {
                printStatistics(loader.getOntology(), ontologyPath.equals("-") ? "standard input" : ontologyPath);
                System.err.println();
            }
            OrgModeFormatter formatter = new OrgModeFormatter(loader, tangleTarget);

            String output = formatter.generateFullOutput();

            if (verbose) {
                System.err.println("Export statistics:");
                long headerCount = java.util.Arrays.stream(output.split("\\r?\\n"))
                        .filter(line -> line.startsWith("*"))
                        .count();
                System.err.printf("  %-22s %d%n", "Org Headers:", headerCount);
            }

            // -- Output -----------------------------------------------------------------------
            output = output.replaceAll("\\r\\n", "\n"); // normalise line endings
            
            String finalOutput = output;
            if (outputFile != null) {
                java.io.File file = new java.io.File(outputFile);
                if (file.exists() && !force) {
                    String existingContent = new String(Files.readAllBytes(file.toPath()), StandardCharsets.UTF_8);
                    if (!ChecksumUtility.isPristine(existingContent)) {
                        String storedChecksum = ChecksumUtility.extractChecksum(existingContent);
                        if (storedChecksum == null) {
                            // If it doesn't have a checksum, but it looks like it was generated by us...
                            if (existingContent.contains("This org-mode file was created using elot-exporter")) {
                                System.err.println("Warning: Output file exists and lacks a checksum. It may have been edited.");
                                System.err.println("Use --force to overwrite.");
                                return 1;
                            }
                            // If it doesn't look like our file, we should definitely not overwrite it without force!
                            System.err.println("Error: Output file exists and does not appear to be a generated file.");
                            System.err.println("Use --force to overwrite.");
                            return 1;
                        } else {
                            System.err.println("Error: Output file has been modified since it was generated.");
                            System.err.println("Use --force to overwrite.");
                            return 1;
                        }
                    }
                }
                
                // Add checksum to the output before writing
                String checksum = ChecksumUtility.calculateChecksum(output);
                String checksumLine = ChecksumUtility.formatChecksumLine(checksum);
                // Insert after the first comment line
                if (output.contains("\n")) {
                    finalOutput = output.replaceFirst("\n", "\n" + checksumLine + "\n");
                } else {
                    finalOutput = output + "\n" + checksumLine;
                }

                byte[] bytes = finalOutput.getBytes(StandardCharsets.UTF_8);
                Files.write(Paths.get(outputFile), bytes);
                if (verbose) {
                    System.err.printf("  %-22s %s (%d bytes)%n", "Output written to:", outputFile, bytes.length);
                }
            } else {
                // If writing to stdout, also include checksum
                String checksum = ChecksumUtility.calculateChecksum(output);
                String checksumLine = ChecksumUtility.formatChecksumLine(checksum);
                if (output.contains("\n")) {
                    finalOutput = output.replaceFirst("\n", "\n" + checksumLine + "\n");
                } else {
                    finalOutput = output + "\n" + checksumLine;
                }

                System.out.print(finalOutput);
                if (verbose) {
                    System.err.printf("  %-22s %s (%d bytes)%n", "Output written to:", "standard output", finalOutput.getBytes(StandardCharsets.UTF_8).length);
                }
            }
            return 0;
        } catch (OWLOntologyCreationException e) {
            System.err.println("Error: Failed to load ontology: " + e.getMessage());
            if ((verbosity != null && verbosity.length > 0) || debug) {
                e.printStackTrace();
            }
            return 1;
        } catch (Exception e) {
            System.err.println("An unexpected error occurred: " + e.getMessage());
            if ((verbosity != null && verbosity.length > 0) || debug) {
                e.printStackTrace();
            }
            return 1;
        }
    }

    private void printStatistics(OWLOntology ontology, String label) {
        System.err.println("Statistics for " + label + ":");
        ontology.getOntologyID().getOntologyIRI().ifPresent(iri ->
            System.err.printf("  %-22s %s%n", "Ontology IRI:", iri));
        System.err.printf("  %-22s %d%n", "Classes:", ontology.getClassesInSignature().size());
        System.err.printf("  %-22s %d%n", "Object Properties:", ontology.getObjectPropertiesInSignature().size());
        System.err.printf("  %-22s %d%n", "Data Properties:", ontology.getDataPropertiesInSignature().size());
        System.err.printf("  %-22s %d%n", "Annotation Properties:", ontology.getAnnotationPropertiesInSignature().size());
        System.err.printf("  %-22s %d%n", "Individuals:", ontology.getIndividualsInSignature().size());
        System.err.printf("  %-22s %d%n", "Axioms:", ontology.getAxiomCount());
        System.err.printf("  %-22s %d%n", "Logical Axioms:", ontology.getLogicalAxiomCount());
    }

    static class VersionProvider implements CommandLine.IVersionProvider {
        @Override
        public String[] getVersion() {
            Package pkg = ElotExporter.class.getPackage();
            String version = (pkg != null && pkg.getImplementationVersion() != null)
                    ? pkg.getImplementationVersion() : "unknown";
            return new String[] { "elot-exporter " + version };
        }
    }
}
