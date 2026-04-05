/*
 * Copyright (C) 2024-2025 Johan W. Klüwer <johan.w.kluwer@gmail.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 */

package com.elotexport;

import org.semanticweb.owlapi.model.*;
import java.util.stream.Collectors;

public class OntologyDeclarationFormatter {

    private final FormattingContext context;
    private final AnnotationFormatter annotationFormatter;

    public OntologyDeclarationFormatter(FormattingContext context, AnnotationFormatter annotationFormatter) {
        this.context = context;
        this.annotationFormatter = annotationFormatter;
    }

    public String formatOntologyDeclaration(String localName) {
        StringBuilder sb = new StringBuilder();

        OWLOntologyID ontologyID = context.ontology().getOntologyID();
        IRI ontologyIRI = ontologyID.getOntologyIRI().orElse(IRI.create("unknown"));
        String shortForm = context.prefixManager().getShortForm(ontologyIRI);
        if (shortForm.startsWith("<") && shortForm.endsWith(">")) {
            shortForm = shortForm.substring(1, shortForm.length() - 1);
        }

        sb.append("** ").append(localName).append(" ontology (").append(shortForm);
        ontologyID.getVersionIRI().ifPresent(viri -> sb.append(" ").append(viri.toString()));
        sb.append(")\n");

        sb.append(":PROPERTIES:\n");
        String id = localName + "-ontology-declaration";
        sb.append(":ID:       ").append(id).append("\n");
        sb.append(":custom_id: ").append(id).append("\n");
        sb.append(":resourcedefs: yes\n");
        sb.append(":END:\n\n");

        // Direct Imports
        context.ontology().directImportsDocuments().sorted().forEach(iri ->
            sb.append(" - Import :: ").append(iri.toString()).append("\n"));

        // Ontology Annotations
        context.ontology().annotations().sorted().forEach(ann -> {
            String prop = context.renderer().render(ann.getProperty());
            String val = annotationFormatter.formatAnnotationValue(ann.getValue());
            sb.append(" - ").append(prop).append(" :: ").append(val).append("\n");
        });

        return sb.toString();
    }
}
