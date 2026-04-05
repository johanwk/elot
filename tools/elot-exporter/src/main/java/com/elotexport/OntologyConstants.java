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
import org.semanticweb.owlapi.vocab.OWLRDFVocabulary;

public final class OntologyConstants {
    private OntologyConstants() {}

    public static final String RDFS_LABEL = OWLRDFVocabulary.RDFS_LABEL.getIRI().toString();
    public static final String DC_TITLE = "http://purl.org/dc/elements/1.1/title";
    public static final String DCTERMS_TITLE = "http://purl.org/dc/terms/title";
    
    public static final java.util.List<String> LABEL_IRIS = java.util.List.of(
        "http://www.w3.org/2004/02/skos/core#prefLabel",
        "http://www.w3.org/2000/01/rdf-schema#label",
        "http://purl.org/dc/elements/1.1/title",
        DCTERMS_TITLE,
        "http://xmlns.com/foaf/0.1/name"
    );

    public static final String OWL_THING = OWLRDFVocabulary.OWL_THING.getIRI().toString();
}
