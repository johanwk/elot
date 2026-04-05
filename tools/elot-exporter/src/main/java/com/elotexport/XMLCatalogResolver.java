/*
 * Copyright (C) 2024-2025 Johan W. Klüwer <johan.w.kluwer@gmail.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 */

package com.elotexport;

import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLOntologyIRIMapper;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import org.semanticweb.owlapi.util.AutoIRIMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import java.io.File;
import java.util.LinkedHashMap;
import java.util.Map;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

/**
 * Resolves local copies of imported ontologies using both:
 * <ul>
 *   <li>Protégé-style {@code catalog-v001.xml} files (OASIS XML Catalog format)</li>
 *   <li>OWLAPI's {@link AutoIRIMapper} for directory scanning</li>
 * </ul>
 *
 * <p>The catalog file maps ontology IRIs (the {@code name} attribute of {@code <uri>}
 * elements) to local file paths (the {@code uri} attribute), exactly as Protégé does.
 * If no catalog file is found, falls back to AutoIRIMapper only.</p>
 */
public class XMLCatalogResolver {
    private static final Logger logger = LoggerFactory.getLogger(XMLCatalogResolver.class);

    /** The standard catalog filename used by Protégé. */
    private static final String CATALOG_FILENAME = "catalog-v001.xml";

    /**
     * Registers IRI mappers for the given ontology path.
     *
     * <p>If a {@code catalog-v001.xml} file exists in the same directory as the
     * ontology file, its {@code <uri>} entries are parsed and registered as IRI
     * mappings. Additionally, an {@link AutoIRIMapper} is registered for the
     * directory as a fallback.</p>
     *
     * @param manager      The OWL manager to register the mapper(s) with.
     * @param ontologyPath The path to the ontology file.
     */
    public static void registerCatalog(OWLOntologyManager manager, String ontologyPath) {
        if (ontologyPath == null || "-".equals(ontologyPath) ||
            ontologyPath.startsWith("http://") || ontologyPath.startsWith("https://")) {
            return;
        }

        File ontologyFile = new File(ontologyPath);
        File parentDir = ontologyFile.getAbsoluteFile().getParentFile();

        if (parentDir == null || !parentDir.exists()) {
            return;
        }

        // Try to parse catalog-v001.xml
        File catalogFile = new File(parentDir, CATALOG_FILENAME);
        if (catalogFile.exists() && catalogFile.isFile()) {
            Map<IRI, IRI> mappings = parseCatalog(catalogFile, parentDir);
            if (!mappings.isEmpty()) {
                logger.info("Loaded {} IRI mapping(s) from {}", mappings.size(), catalogFile.getAbsolutePath());
                OWLOntologyIRIMapper catalogMapper = new CatalogIRIMapper(mappings);
                manager.getIRIMappers().add(catalogMapper);
            }
        } else {
            logger.debug("No {} found in {}", CATALOG_FILENAME, parentDir.getAbsolutePath());
        }

        // Also register AutoIRIMapper as a fallback
        logger.info("Registering AutoIRIMapper for directory: {}", parentDir.getAbsolutePath());
        manager.getIRIMappers().add(new AutoIRIMapper(parentDir, true));
    }

    /**
     * Parses a Protégé-style {@code catalog-v001.xml} file and returns a map
     * from ontology IRIs to local file IRIs.
     *
     * <p>Handles {@code <uri>} elements both at the top level and inside
     * {@code <group>} elements. The {@code xml:base} attribute on {@code <group>}
     * elements is respected when resolving relative URIs.</p>
     *
     * @param catalogFile the catalog XML file
     * @param baseDir     the base directory for resolving relative URIs
     * @return a map from ontology IRI to local document IRI
     */
    static Map<IRI, IRI> parseCatalog(File catalogFile, File baseDir) {
        Map<IRI, IRI> mappings = new LinkedHashMap<>();
        try {
            DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
            factory.setNamespaceAware(true);
            // Security: disable external entities
            factory.setFeature("http://apache.org/xml/features/disallow-doctype-decl", true);
            DocumentBuilder builder = factory.newDocumentBuilder();
            Document doc = builder.parse(catalogFile);
            doc.getDocumentElement().normalize();

            // Process all <uri> elements (both top-level and inside <group>)
            NodeList uriNodes = doc.getElementsByTagNameNS(
                    "urn:oasis:names:tc:entity:xmlns:xml:catalog", "uri");

            // Also try without namespace (some catalogs may not use namespace)
            if (uriNodes.getLength() == 0) {
                uriNodes = doc.getElementsByTagName("uri");
            }

            for (int i = 0; i < uriNodes.getLength(); i++) {
                Element uriElement = (Element) uriNodes.item(i);
                String name = uriElement.getAttribute("name");
                String uri = uriElement.getAttribute("uri");

                if (name == null || name.isEmpty() || uri == null || uri.isEmpty()) {
                    logger.debug("Skipping catalog <uri> entry with missing name or uri");
                    continue;
                }

                // Determine the effective base for resolving relative URIs.
                // Check for xml:base on the parent <group> element.
                File effectiveBase = baseDir;
                if (uriElement.getParentNode() instanceof Element parent) {
                    String xmlBase = parent.getAttribute("xml:base");
                    if (xmlBase != null && !xmlBase.isEmpty()) {
                        File groupBase = new File(baseDir, xmlBase);
                        if (groupBase.isDirectory()) {
                            effectiveBase = groupBase;
                        }
                    }
                }

                IRI ontologyIRI = IRI.create(name);
                IRI documentIRI;

                if (uri.startsWith("http://") || uri.startsWith("https://") || uri.startsWith("file:")) {
                    // Absolute URI
                    documentIRI = IRI.create(uri);
                } else {
                    // Relative path — resolve against the effective base directory
                    File localFile = new File(effectiveBase, uri);
                    documentIRI = IRI.create(localFile.toURI());
                }

                mappings.put(ontologyIRI, documentIRI);
                logger.info("Catalog mapping: {} -> {}", ontologyIRI, documentIRI);
            }
        } catch (Exception e) {
            logger.warn("Failed to parse catalog file {}: {}", catalogFile.getAbsolutePath(), e.getMessage());
        }
        return mappings;
    }

    /**
     * An OWLOntologyIRIMapper backed by a static map of IRI-to-IRI mappings
     * parsed from a catalog file.
     */
    private static class CatalogIRIMapper implements OWLOntologyIRIMapper {
        private final Map<IRI, IRI> mappings;

        CatalogIRIMapper(Map<IRI, IRI> mappings) {
            this.mappings = mappings;
        }

        @Override
        public IRI getDocumentIRI(IRI ontologyIRI) {
            return mappings.get(ontologyIRI);
        }
    }
}
