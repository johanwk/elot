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
import org.semanticweb.owlapi.util.DefaultPrefixManager;
import java.util.Comparator;

public class HeaderGenerator {

    private final FormattingContext context;
    private final AnnotationFormatter annotationFormatter;

    public HeaderGenerator(FormattingContext context, AnnotationFormatter annotationFormatter) {
        this.context = context;
        this.annotationFormatter = annotationFormatter;
    }

    public String formatEntityEntry(OWLEntity entity, int level) {
        String curie = context.prefixManager().getShortForm(entity.getIRI());
        AnnotationFormatter.LabelInfo li = annotationFormatter.getEntityLabelWithSource(entity);
        String label;
        if (li != null) {
            label = li.label();
        } else {
            label = annotationFormatter.getEntityLabel(entity);
        }
        if (label != null && !label.isEmpty()) {
            return "*".repeat(level) + " " + label + " (" + curie + ")";
        } else {
            return "*".repeat(level) + " " + curie;
        }
    }
    
    public String getSortKey(OWLEntity entity) {
        String label = annotationFormatter.getEntityLabel(entity);
        if (label != null && !label.isEmpty()) {
            return label;
        }
        String curie = context.prefixManager().getShortForm(entity.getIRI());
        // Extract local name from CURIE
        int colonIndex = curie.lastIndexOf(":");
        if (colonIndex != -1 && colonIndex < curie.length() - 1) {
            return curie.substring(colonIndex + 1);
        }
        // Fallback to fragment or last part of IRI
        IRI iri = entity.getIRI();
        String fragment = iri.getRemainder().orElse("");
        if (!fragment.isEmpty()) {
            return fragment;
        }
        String iriStr = iri.toString();
        int slashIndex = iriStr.lastIndexOf("/");
        if (slashIndex != -1 && slashIndex < iriStr.length() - 1) {
            return iriStr.substring(slashIndex + 1);
        }
        return curie;
    }

    public Comparator<OWLEntity> getEntityComparator() {
        return (e1, e2) -> {
            String sk1 = getSortKey(e1);
            String sk2 = getSortKey(e2);
            int cmp = sk1.compareToIgnoreCase(sk2);
            if (cmp != 0) {
                return cmp;
            }
            String c1 = context.prefixManager().getShortForm(e1.getIRI());
            String c2 = context.prefixManager().getShortForm(e2.getIRI());
            return c1.compareToIgnoreCase(c2);
        };
    }
    
    public static String formatTopLevelHeading(String title, String localnameSuffix, String prefix) {
        String id = prefix + "-" + localnameSuffix;
        return "** " + title + "\n" +
               ":PROPERTIES:\n" +
               ":ID:       " + id + "\n" +
               ":custom_id: " + id + "\n" +
               ":resourcedefs: yes\n" +
               ":END:\n";
    }
}
