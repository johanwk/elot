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

public class IndividualFormatter {

    private final FormattingContext context;
    private final HeaderGenerator headerGenerator;
    private final AnnotationFormatter annotationFormatter;

    public IndividualFormatter(FormattingContext context, HeaderGenerator headerGenerator, AnnotationFormatter annotationFormatter) {
        this.context = context;
        this.headerGenerator = headerGenerator;
        this.annotationFormatter = annotationFormatter;
    }

    public String generateIndividualSection(String localName, Set<OWLNamedIndividual> individuals) {
        Set<OWLNamedIndividual> nonDeprecatedInds = individuals.stream()
                .filter(e -> !OntologyUtils.isDeprecated(e, context.ontology()))
                .collect(Collectors.toSet());
        Set<OWLNamedIndividual> deprecatedInds = individuals.stream()
                .filter(e -> OntologyUtils.isDeprecated(e, context.ontology()))
                .collect(Collectors.toSet());

        StringBuilder sb = new StringBuilder();
        sb.append(HeaderGenerator.formatTopLevelHeading("Individuals", "individuals", localName)).append("\n");
        AxiomFormatter af = new AxiomFormatter(context, annotationFormatter);
        String sameness = af.generateSameIndividualClauses();
        if (!sameness.isEmpty()) {
            sb.append(sameness).append("\n");
        }
        String differentness = af.generateDifferentnessClauses();
        if (!differentness.isEmpty()) {
            sb.append(differentness).append("\n");
        }
        sb.append(generateFlatList(nonDeprecatedInds, 3)).append("\n");
        if (!deprecatedInds.isEmpty()) {
            sb.append("*** Deprecated Individuals                                          :nodeclare:").append("\n");
            sb.append(generateFlatList(deprecatedInds, 4)).append("\n");
        }
        return sb.toString();
    }

    public String generateFlatList(Set<OWLNamedIndividual> individuals, int level) {
        Map<String, List<OWLNamedIndividual>> groups = new TreeMap<>(String.CASE_INSENSITIVE_ORDER);
        for (OWLNamedIndividual ind : context.taxonomy().getIndividuals(individuals)) {
            OWLClass mostSpecific = getMostSpecificType(ind);
            String typeLabel;
            if (mostSpecific != null) {
                String label = annotationFormatter.getEntityLabel(mostSpecific);
                if (label != null) {
                    typeLabel = label;
                } else {
                    typeLabel = context.renderer().render(mostSpecific);
                }
            } else {
                typeLabel = "Unknown";
            }
            groups.computeIfAbsent(typeLabel, k -> new ArrayList<>()).add(ind);
        }
        StringBuilder sb = new StringBuilder();
        for (Map.Entry<String, List<OWLNamedIndividual>> entry : groups.entrySet()) {
            String typeLabel = entry.getKey();
            sb.append(String.format("%-65s :nodeclare:\n", "*".repeat(level) + " " + typeLabel));
            List<OWLNamedIndividual> inds = entry.getValue();
            inds.sort(headerGenerator.getEntityComparator());
            for (OWLNamedIndividual ind : inds) {
                sb.append(formatIndividual(ind, level + 1)).append("\n");
            }
        }
        return sb.toString();
    }

    private OWLClass getMostSpecificType(OWLNamedIndividual individual) {
        Set<OWLClass> types = context.ontology().classAssertionAxioms(individual)
            .map(ax -> ax.getClassExpression())
            .filter(ce -> !ce.isAnonymous() && ce instanceof OWLClass)
            .map(ce -> (OWLClass) ce)
            .collect(Collectors.toSet());
        if (types.isEmpty()) {
            return null;
        }
        List<OWLClass> minimal = new ArrayList<>(types);
        for (OWLClass t : types) {
            for (OWLClass u : types) {
                if (!u.equals(t)) {
                    boolean isSuper = context.ontology().subClassAxiomsForSubClass(u)
                            .anyMatch(ax -> ax.getSuperClass().equals(t));
                    if (isSuper) {
                        minimal.remove(t);
                        break;
                    }
                }
            }
        }
        if (minimal.isEmpty()) {
            minimal.addAll(types);
        }
        minimal.sort(headerGenerator.getEntityComparator());
        return minimal.get(0);
    }

    public String formatIndividual(OWLNamedIndividual ind, int level) {
        StringBuilder sb = new StringBuilder();
        sb.append(headerGenerator.formatEntityEntry(ind, level)).append("\n");
        sb.append(annotationFormatter.formatAnnotations(ind));
        
        // Types
        context.ontology().classAssertionAxioms(ind)
            .sorted(java.util.Comparator.comparing(ax -> context.renderer().render(ax.getClassExpression())))
            .forEach(ax -> {
                sb.append(" - Types :: ").append(context.renderer().render(ax.getClassExpression())).append("\n");
                sb.append(annotationFormatter.formatMetaAnnotations(ax.annotations(), 4));
            });
            
        // Object Property Assertions
        context.ontology().objectPropertyAssertionAxioms(ind)
            .sorted(Comparator.comparing((OWLObjectPropertyAssertionAxiom ax) -> context.renderer().render(ax.getProperty()))
                    .thenComparing(ax -> context.renderer().render(ax.getObject())))
            .forEach(ax -> {
                sb.append(" - Facts :: ").append(context.renderer().render(ax.getProperty()))
                  .append(" ").append(context.renderer().render(ax.getObject())).append("\n");
                sb.append(annotationFormatter.formatMetaAnnotations(ax.annotations(), 4));
            });

        // Negative Object Property Assertions
        context.ontology().negativeObjectPropertyAssertionAxioms(ind)
            .sorted(Comparator.comparing((OWLNegativeObjectPropertyAssertionAxiom ax) -> context.renderer().render(ax.getProperty()))
                    .thenComparing(ax -> context.renderer().render(ax.getObject())))
            .forEach(ax -> {
                sb.append(" - Facts :: not ").append(context.renderer().render(ax.getProperty()))
                  .append(" ").append(context.renderer().render(ax.getObject())).append("\n");
                sb.append(annotationFormatter.formatMetaAnnotations(ax.annotations(), 4));
            });
            
        // Data Property Assertions
        context.ontology().dataPropertyAssertionAxioms(ind)
            .sorted(Comparator.comparing((OWLDataPropertyAssertionAxiom ax) -> context.renderer().render(ax.getProperty()))
                    .thenComparing(ax -> annotationFormatter.formatDataPropertyValue(ax.getObject())))
            .forEach(ax -> {
                sb.append(" - Facts :: ").append(context.renderer().render(ax.getProperty()))
                  .append(" ").append(annotationFormatter.formatDataPropertyValue(ax.getObject())).append("\n");
                sb.append(annotationFormatter.formatMetaAnnotations(ax.annotations(), 4));
            });

        // Negative Data Property Assertions
        context.ontology().negativeDataPropertyAssertionAxioms(ind)
            .sorted(Comparator.comparing((OWLNegativeDataPropertyAssertionAxiom ax) -> context.renderer().render(ax.getProperty()))
                    .thenComparing(ax -> annotationFormatter.formatDataPropertyValue(ax.getObject())))
            .forEach(ax -> {
                sb.append(" - Facts :: not ").append(context.renderer().render(ax.getProperty()))
                  .append(" ").append(annotationFormatter.formatDataPropertyValue(ax.getObject())).append("\n");
                sb.append(annotationFormatter.formatMetaAnnotations(ax.annotations(), 4));
            });

        // SameAs
        context.ontology().sameIndividualAxioms(ind)
            .forEach(ax -> {
                // Filter out axioms that are handled by the separate "Sameness clauses" section (size > 2)
                if (ax.individuals().count() > 2) {
                    return;
                }
                ax.individuals()
                    .filter(i -> !i.equals(ind))
                    .map(i -> context.renderer().render(i))
                    .sorted()
                    .forEach(rendered -> sb.append(" - SameAs :: ").append(rendered).append("\n"));
                sb.append(annotationFormatter.formatMetaAnnotations(ax.annotations(), 4));
            });

        // DifferentFrom
        context.ontology().differentIndividualAxioms(ind)
            .forEach(ax -> {
                // Filter out axioms that are handled by the separate "Differentness clauses" section (size > 2)
                if (ax.individuals().count() > 2) {
                    return;
                }
                ax.individuals()
                    .filter(i -> !i.equals(ind))
                    .map(i -> context.renderer().render(i))
                    .sorted()
                    .forEach(rendered -> sb.append(" - DifferentFrom :: ").append(rendered).append("\n"));
                sb.append(annotationFormatter.formatMetaAnnotations(ax.annotations(), 4));
            });
            
        return sb.toString();
    }
}
