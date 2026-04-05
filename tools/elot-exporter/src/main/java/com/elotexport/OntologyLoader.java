/*
 * Copyright (C) 2024-2025 Johan W. Klüwer <johan.w.kluwer@gmail.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 */

package com.elotexport;

import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.io.StreamDocumentSource;

import org.semanticweb.owlapi.util.DefaultPrefixManager;

import java.io.File;

public class OntologyLoader {
    private final OWLOntology ontology;
    private final DefaultPrefixManager prefixManager;
    // Add a field to store the original source (e.g., file path or URL)
    private final String ontologySource;

    public OntologyLoader(String ontologyPath) throws OWLOntologyCreationException {
        this.ontologySource = ontologyPath;
        OWLOntologyManager manager = OWLManager.createOWLOntologyManager();

        // Register catalog if it exists for the local ontology
        XMLCatalogResolver.registerCatalog(manager, ontologyPath);

        if ("-".equals(ontologyPath)) {
            // Use StreamDocumentSource to avoid deprecated InputStream method.
            ontology = manager.loadOntologyFromOntologyDocument(new StreamDocumentSource(System.in));
        } else if (ontologyPath.startsWith("http://") || ontologyPath.startsWith("https://")) {
            // Use loadOntology(IRI) for remote documents.
            ontology = manager.loadOntology(IRI.create(ontologyPath));
        } else {
            // For local files, create an IRI from the File.
            File ontologyFile = new File(ontologyPath);
            ontology = manager.loadOntology(IRI.create(ontologyFile));
        }

        // Initialize the prefix manager using your helper method.
        prefixManager = OntologyUtils.createPrefixManager(ontology);
    }

    public OWLOntology getOntology() {
        return ontology;
    }

    public DefaultPrefixManager getPrefixManager() {
        return prefixManager;
    }
    
    // New getter to retrieve the original ontology source
    public String getOntologySource() {
        return ontologySource;
    }
}
