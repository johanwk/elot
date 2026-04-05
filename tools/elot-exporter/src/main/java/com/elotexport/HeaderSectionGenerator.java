/*
 * Copyright (C) 2024-2025 Johan W. Klüwer <johan.w.kluwer@gmail.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 */

package com.elotexport;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Comparator;
import java.util.Optional;
import org.semanticweb.owlapi.model.OWLAnnotationValue;
import org.semanticweb.owlapi.model.OWLLiteral;

public class HeaderSectionGenerator {

    private final FormattingContext context;

    public HeaderSectionGenerator(FormattingContext context) {
        this.context = context;
    }

    public String generateFullHeader(String ontologySource) {
        String title = getOntologyTitle();
        if (title == null || title.isEmpty()) {
            title = "Untitled Ontology";
        }
        String subtitle = "An OWL ontology";
        String author = "";
        LocalDateTime now = LocalDateTime.now();
        DateTimeFormatter dtf = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm");
        String formattedDate = now.format(dtf);
        String callLine = "theme-elot()";
        
        StringBuilder sb = new StringBuilder();
        Package pkg = ElotExporter.class.getPackage();
        String version = (pkg != null && pkg.getImplementationVersion() != null)
                         ? pkg.getImplementationVersion() : "unknown";

        sb.append("# This org-mode file was created using elot-exporter version ").append(version).append(".\n");
        if (ontologySource != null && !ontologySource.trim().isEmpty()) {
            if (ontologySource.equals("-")) {
                sb.append("# Source ontology: [Standard Input]\n");
            } else if (ontologySource.startsWith("http://") || ontologySource.startsWith("https://")) {
                sb.append("# Source ontology: [URL] ").append(ontologySource).append("\n");
            } else {
                sb.append("# Source ontology: [Local File] ").append(ontologySource).append("\n");
            }
        }
        sb.append("\n");

        sb.append("#+title: ").append(title).append("\n");
        sb.append("#+subtitle: ").append(subtitle).append("\n");
        sb.append("#+author: ").append(author).append("\n");
        sb.append("#+date: WIP (version of ").append(formattedDate).append(")\n");
        sb.append("#+call: ").append(callLine).append("\n");
        sb.append("\n"); // blank line after call
        
        return sb.toString();
    }
    
    private String getOntologyTitle() {
        Optional<String> dcTermsTitle = context.ontology().getAnnotations().stream()
            .filter(ax -> ax.getProperty().getIRI().toString().equals(OntologyConstants.DCTERMS_TITLE))
            .sorted(Comparator.comparing(ax -> context.renderer().render(ax.getValue())))
            .map(ax -> {
                OWLAnnotationValue val = ax.getValue();
                if (val instanceof OWLLiteral) {
                    return ((OWLLiteral) val).getLiteral();
                }
                return "";
            })
            .filter(s -> !s.isEmpty())
            .findFirst();

        if (dcTermsTitle.isPresent()) {
            return dcTermsTitle.get();
        }
        Optional<String> dcTitle = context.ontology().getAnnotations().stream()
            .filter(ax -> ax.getProperty().getIRI().toString().equals(OntologyConstants.DC_TITLE))
            .sorted(Comparator.comparing(ax -> context.renderer().render(ax.getValue())))
            .map(ax -> {
                OWLAnnotationValue val = ax.getValue();
                if (val instanceof OWLLiteral) {
                    return ((OWLLiteral) val).getLiteral();
                }
                return "";
            })
            .filter(s -> !s.isEmpty())
            .findFirst();

        if (dcTitle.isPresent()) {
            return dcTitle.get();
        }
        
        Optional<String> rdfsLabel = context.ontology().getAnnotations().stream()
            .filter(ax -> ax.getProperty().getIRI().toString().equals(OntologyConstants.RDFS_LABEL))
            .sorted(Comparator.comparing(ax -> context.renderer().render(ax.getValue())))
            .map(ax -> {
                OWLAnnotationValue val = ax.getValue();
                if (val instanceof OWLLiteral) {
                    return ((OWLLiteral) val).getLiteral();
                }
                return "";
            })
            .filter(s -> !s.isEmpty())
            .findFirst();
        
        return rdfsLabel.orElse("");
    }

    public String generateContextHeader(String contextName, String defaultPrefix) {
        StringBuilder sb = new StringBuilder();
        sb.append("* ").append(contextName).append("\n");
        sb.append(":PROPERTIES:\n");
        sb.append(":ID:       ").append(contextName).append("\n");
        sb.append(":ELOT-context-type: ontology\n");
        sb.append(":ELOT-context-localname: ").append(contextName).append("\n");
        sb.append(":ELOT-default-prefix: ").append(defaultPrefix).append("\n");
        sb.append(":header-args:omn: :tangle ").append(context.tangleTarget()).append(" :noweb yes\n");
        sb.append(":header-args:emacs-lisp: :tangle no :exports results\n");
        sb.append(":header-args: :padline yes\n");
        sb.append(":END:\n");
        return sb.toString();
    }
    
}
