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

public class HierarchyFormatter {

    private final FormattingContext context;
    private final AnnotationFormatter annotationFormatter;
    private final AxiomFormatter axiomFormatter;
    private final HeaderGenerator headerGenerator;

    public HierarchyFormatter(FormattingContext context, AnnotationFormatter annotationFormatter, AxiomFormatter axiomFormatter, HeaderGenerator headerGenerator) {
        this.context = context;
        this.annotationFormatter = annotationFormatter;
        this.axiomFormatter = axiomFormatter;
        this.headerGenerator = headerGenerator;
    }

    public String generateOrgHierarchy(Set<? extends OWLEntity> entities, int level) {
        Map<OWLEntity, OWLEntity> parentMap = context.taxonomy().buildParentMap(entities);
        
        // Pre-compute children map for O(N) traversal
        Map<OWLEntity, List<OWLEntity>> childrenMap = new HashMap<>();
        for (Map.Entry<OWLEntity, OWLEntity> entry : parentMap.entrySet()) {
            childrenMap.computeIfAbsent(entry.getValue(), k -> new ArrayList<>()).add(entry.getKey());
        }
        // Sort children lists once
        for (List<OWLEntity> children : childrenMap.values()) {
            children.sort(headerGenerator.getEntityComparator());
        }

        // Collect entities whose parent is outside the entity set (e.g. an
        // equivalent class from an imported ontology).  These must be treated
        // as top-level so they are not silently dropped from the output.
        Set<OWLEntity> entitySet = new HashSet<>(entities);
        Set<OWLEntity> orphaned = new HashSet<>();
        for (Map.Entry<OWLEntity, OWLEntity> entry : parentMap.entrySet()) {
            if (!entitySet.contains(entry.getValue())) {
                orphaned.add(entry.getKey());
            }
        }

        List<OWLEntity> topEntities = context.taxonomy().getTopLevelEntities(entities).stream()
            .filter(e -> !e.getIRI().toString().equals(OntologyConstants.OWL_THING))
            .collect(Collectors.toList());

        // Add orphaned entities that would otherwise be unreachable
        for (OWLEntity o : orphaned) {
            if (!topEntities.contains(o)) {
                topEntities.add(o);
            }
        }

        topEntities.sort(headerGenerator.getEntityComparator());
        StringBuilder sb = new StringBuilder();
        for (OWLEntity entity : topEntities) {
            sb.append(formatHierarchy(entity, childrenMap, level));
        }
        return sb.toString();
    }

    private String formatHierarchy(OWLEntity entity, Map<OWLEntity, List<OWLEntity>> childrenMap, int level) {
        StringBuilder sb = new StringBuilder();
        sb.append(headerGenerator.formatEntityEntry(entity, level)).append("\n");
        sb.append(annotationFormatter.formatAnnotations(entity));
        sb.append(axiomFormatter.formatAdditionalSuperEntities(entity));
        
        List<OWLEntity> children = childrenMap.getOrDefault(entity, Collections.emptyList());
        for (OWLEntity child : children) {
            sb.append(formatHierarchy(child, childrenMap, level + 1));
        }
        return sb.toString();
    }

    public <T extends OWLEntity> String generateEntitySection(
            String title,
            String idSuffix,
            String localName,
            Set<T> entities,
            int level) {

        Map<OWLEntity, OWLEntity> parentMap = context.taxonomy().buildParentMap(entities);

        Set<T> nonDeprecated = entities.stream()
                .filter(e -> !OntologyUtils.isDeprecated(e, context.ontology()) || parentMap.containsValue(e))
                .collect(Collectors.toSet());

        Set<T> deprecated = entities.stream()
                .filter(e -> OntologyUtils.isDeprecated(e, context.ontology()) && !parentMap.containsValue(e))
                .collect(Collectors.toSet());

        StringBuilder sb = new StringBuilder();
        sb.append(HeaderGenerator.formatTopLevelHeading(title, idSuffix, localName)).append("\n");

        if ("Classes".equals(title)) {
            String equivalent = axiomFormatter.generateEquivalenceClauses();
            if (!equivalent.isEmpty()) {
                sb.append(equivalent).append("\n");
            }
            String disjoint = axiomFormatter.generateDisjointnessClauses();
            if (!disjoint.isEmpty()) {
                sb.append(disjoint).append("\n");
            }
            String gcis = axiomFormatter.generateGCIClauses();
            if (!gcis.isEmpty()) {
                sb.append(gcis).append("\n");
            }
            Set<SWRLRule> rules = context.ontology().axioms(AxiomType.SWRL_RULE).collect(Collectors.toSet());
            if (!rules.isEmpty()) {
                sb.append("*** Rules      :nodeclare:").append("\n");
                List<SWRLRule> sortedRules = rules.stream()
                    .sorted(Comparator.comparing(r -> context.renderer().render(r)))
                    .collect(Collectors.toList());
                for (SWRLRule rule : sortedRules) {
                    sb.append(" - Rule :: ").append(context.renderer().render(rule)).append("\n");
                    sb.append(annotationFormatter.formatMetaAnnotations(rule.annotations(), 4));
                }
            }
        } else if ("Object properties".equals(title)) {
            String equivalent = axiomFormatter.generateEquivalentObjectPropertiesClauses();
            if (!equivalent.isEmpty()) {
                sb.append(equivalent).append("\n");
            }
            String disjoint = axiomFormatter.generateDisjointObjectPropertiesClauses();
            if (!disjoint.isEmpty()) {
                sb.append(disjoint).append("\n");
            }
        } else if ("Data properties".equals(title)) {
            String equivalent = axiomFormatter.generateEquivalentDataPropertiesClauses();
            if (!equivalent.isEmpty()) {
                sb.append(equivalent).append("\n");
            }
            String disjoint = axiomFormatter.generateDisjointDataPropertiesClauses();
            if (!disjoint.isEmpty()) {
                sb.append(disjoint).append("\n");
            }
        }

        sb.append(generateOrgHierarchy(nonDeprecated, level)).append("\n");
        if (!deprecated.isEmpty()) {
            sb.append("*** Deprecated ").append(title).append("                                          :nodeclare:").append("\n");
            sb.append(generateOrgHierarchy(deprecated, level + 1)).append("\n");
        }
        return sb.toString();
    }
}
