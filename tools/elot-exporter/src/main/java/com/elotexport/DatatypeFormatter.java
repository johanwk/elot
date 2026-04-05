/*
 * Copyright (C) 2024-2025 Johan W. Klüwer <johan.w.kluwer@gmail.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 */

package com.elotexport;

import java.util.*;
import java.util.stream.Collectors;
import org.semanticweb.owlapi.model.*;

public class DatatypeFormatter {

    private final FormattingContext context;
    private final AnnotationFormatter annotationFormatter;
    private final HeaderGenerator headerGenerator;
    private final AxiomFormatter axiomFormatter;

    public DatatypeFormatter(FormattingContext context, AnnotationFormatter annotationFormatter, HeaderGenerator headerGenerator, AxiomFormatter axiomFormatter) {
        this.context = context;
        this.annotationFormatter = annotationFormatter;
        this.headerGenerator = headerGenerator;
        this.axiomFormatter = axiomFormatter;
    }

    public String generateDatatypeList(Set<OWLDatatype> datatypes, int level) {
        StringBuilder sb = new StringBuilder();
        List<OWLDatatype> sortedDatatypes = datatypes.stream()
            .filter(dt -> !dt.isBuiltIn() || context.ontology().isDeclared(dt))
            .sorted(headerGenerator.getEntityComparator())
            .collect(Collectors.toList());
        for (OWLDatatype dt : sortedDatatypes) {
            sb.append("*".repeat(level)).append(" ").append(context.prefixManager().getShortForm(dt.getIRI())).append("\n");
            sb.append(annotationFormatter.formatAnnotations(dt));
            sb.append(axiomFormatter.formatAdditionalSuperEntities(dt));
        }
        return sb.toString();
    }
}
