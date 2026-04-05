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
import java.util.*;
import java.util.stream.Collectors;

import java.util.concurrent.ConcurrentHashMap;

/**
 * Builds taxonomy hierarchies for classes, properties, and individuals.
 */
public class TaxonomyBuilder {
    private final OWLOntology ontology;
    private final Map<Set<? extends OWLEntity>, Map<OWLEntity, OWLEntity>> parentMapCache = new ConcurrentHashMap<>();
    private final Map<OWLEntity, Optional<OWLEntity>> directParentCache = new ConcurrentHashMap<>();

    public TaxonomyBuilder(OntologyLoader loader) {
        this.ontology = loader.getOntology();
    }

    /**
     * Builds a map of entities to their direct parent entities.
     * Retains only one parent, selecting the alphabetically first named parent.
     * Detects and breaks cycles (e.g. from EquivalentTo between two named classes)
     * by choosing the more "important" entity as the parent (the one that carries
     * the equivalence axiom, or alphabetically first if tied).
     */
    public Map<OWLEntity, OWLEntity> buildParentMap(Set<? extends OWLEntity> entities) {
        if (parentMapCache.containsKey(entities)) {
            return parentMapCache.get(entities);
        }

        Map<OWLEntity, OWLEntity> parentMap = new HashMap<>();

        for (OWLEntity entity : entities) {
            OWLEntity parent = findParent(entity);
            if (parent != null) {
                parentMap.put(entity, parent);
            }
        }

        // Detect and break cycles.
        breakCycles(parentMap);

        parentMapCache.put(entities, parentMap);
        return parentMap;
    }

    /**
     * Detects cycles in the parentMap and breaks them.  For each cycle,
     * the entity that should remain as the root (superordinate) is determined
     * by picking the "most important" entity: the one that carries equivalence
     * axioms with non-trivial content.  If tied, alphabetical order of short
     * form is used.  The edge pointing to that chosen root is removed.
     */
    private void breakCycles(Map<OWLEntity, OWLEntity> parentMap) {
        Set<OWLEntity> visited = new HashSet<>();
        Set<OWLEntity> allEntities = new HashSet<>(parentMap.keySet());

        for (OWLEntity start : allEntities) {
            if (visited.contains(start)) continue;

            // Follow parent chain to detect a cycle
            Set<OWLEntity> path = new LinkedHashSet<>();
            OWLEntity current = start;
            while (current != null && !path.contains(current) && !visited.contains(current)) {
                path.add(current);
                current = parentMap.get(current);
            }

            if (current != null && path.contains(current) && !visited.contains(current)) {
                // Found a cycle; collect all entities in the cycle
                List<OWLEntity> cycle = new ArrayList<>();
                boolean inCycle = false;
                for (OWLEntity e : path) {
                    if (e.equals(current)) inCycle = true;
                    if (inCycle) cycle.add(e);
                }

                // Choose the root: the entity that is most "important".
                // Importance: more non-equivalence axioms > fewer.
                // Tie-break: alphabetically first short form (reversed
                // because we use max()).
                OWLEntity root = cycle.stream()
                    .max(Comparator
                        .<OWLEntity, Long>comparing(e -> countNonEquivalenceAxioms(e))
                        .thenComparing(e -> e.getIRI().getShortForm(),
                                       Comparator.reverseOrder()))
                    .orElse(cycle.get(0));

                // Remove the edge that points TO the root (i.e., the entity
                // whose parent is the root keeps its edge; the root itself
                // should have its parent edge removed so it becomes top-level).
                parentMap.remove(root);
            }

            visited.addAll(path);
        }
    }

    /**
     * Counts the number of "substantive" axioms referencing an entity,
     * excluding equivalence axioms (since those are exactly what creates the
     * cycle).  An entity with more non-equivalence axioms is considered more
     * important and should be the superordinate (root) when breaking a cycle.
     *
     * For classes this counts SubClassOf, DisjointWith, HasKey, etc.
     * For properties this counts SubPropertyOf, Domain, Range, etc.
     */
    private long countNonEquivalenceAxioms(OWLEntity entity) {
        return ontology.referencingAxioms(entity)
            .filter(ax -> !(ax instanceof OWLEquivalentClassesAxiom)
                       && !(ax instanceof OWLEquivalentObjectPropertiesAxiom)
                       && !(ax instanceof OWLEquivalentDataPropertiesAxiom)
                       && !(ax instanceof OWLDeclarationAxiom))
            .count();
    }

    private OWLEntity findParent(OWLEntity entity) {
        Optional<OWLEntity> cached = directParentCache.get(entity);
        if (cached != null) {
            return cached.orElse(null);
        }

        OWLEntity parent = null;
        if (entity instanceof OWLClass) {
            parent = findClassParent((OWLClass) entity);
        } else if (entity instanceof OWLObjectProperty) {
            parent = findObjectPropertyParent((OWLObjectProperty) entity);
        } else if (entity instanceof OWLDataProperty) {
            parent = findDataPropertyParent((OWLDataProperty) entity);
        } else if (entity instanceof OWLAnnotationProperty) {
            parent = findAnnotationPropertyParent((OWLAnnotationProperty) entity);
        }
        
        directParentCache.put(entity, Optional.ofNullable(parent));
        return parent;
    }

    private OWLClass findClassParent(OWLClass cls) {
        List<OWLClass> superClasses = ontology.subClassAxiomsForSubClass(cls)
                .map(OWLSubClassOfAxiom::getSuperClass)
                .filter(ce -> !ce.isAnonymous() && ce instanceof OWLClass)
                .map(OWLClass.class::cast)
                .filter(c -> !c.isOWLThing())
                .sorted(Comparator.comparing(c -> c.getIRI().getShortForm()))
                .collect(Collectors.toList());
        if (!superClasses.isEmpty()) {
            return superClasses.get(0);
        }
        
        // Check EquivalentClasses axioms
        List<OWLClass> candidateSupers = ontology.equivalentClassesAxioms(cls)
            .flatMap(ax -> ax.classExpressions())
            .filter(ce -> !ce.equals(cls))
            .flatMap(ce -> {
                if (!ce.isAnonymous() && ce instanceof OWLClass) {
                    return java.util.stream.Stream.of((OWLClass) ce);
                } else if (ce instanceof OWLObjectIntersectionOf) {
                    return ((OWLObjectIntersectionOf) ce).operands()
                        .filter(op -> !op.isAnonymous() && op instanceof OWLClass)
                        .map(OWLClass.class::cast);
                }
                return java.util.stream.Stream.empty();
            })
            .distinct()
            .sorted(Comparator.comparing(c -> c.getIRI().getShortForm()))
            .collect(Collectors.toList());
            
        return candidateSupers.isEmpty() ? null : candidateSupers.get(0);
    }

    private OWLObjectProperty findObjectPropertyParent(OWLObjectProperty prop) {
        List<OWLObjectProperty> superProperties = ontology.objectSubPropertyAxiomsForSubProperty(prop)
                .map(OWLSubObjectPropertyOfAxiom::getSuperProperty)
                .filter(pe -> !pe.isAnonymous() && pe instanceof OWLObjectProperty)
                .map(OWLObjectProperty.class::cast)
                .sorted(Comparator.comparing(p -> p.getIRI().getShortForm()))
                .collect(Collectors.toList());
        if (!superProperties.isEmpty()) {
            return superProperties.get(0);
        }
        
        List<OWLObjectProperty> candidateSupers = ontology.equivalentObjectPropertiesAxioms(prop)
            .flatMap(ax -> ax.properties())
            .filter(p -> !p.equals(prop) && !p.isAnonymous() && p instanceof OWLObjectProperty)
            .map(OWLObjectProperty.class::cast)
            .sorted(Comparator.comparing(p -> p.getIRI().getShortForm()))
            .collect(Collectors.toList());
            
        return candidateSupers.isEmpty() ? null : candidateSupers.get(0);
    }

    private OWLDataProperty findDataPropertyParent(OWLDataProperty prop) {
        List<OWLDataProperty> superProperties = ontology.dataSubPropertyAxiomsForSubProperty(prop)
                .map(OWLSubDataPropertyOfAxiom::getSuperProperty)
                .filter(pe -> !pe.isAnonymous() && pe instanceof OWLDataProperty)
                .map(OWLDataProperty.class::cast)
                .sorted(Comparator.comparing(p -> p.getIRI().getShortForm()))
                .collect(Collectors.toList());
        if (!superProperties.isEmpty()) {
            return superProperties.get(0);
        }
        
        List<OWLDataProperty> candidateSupers = ontology.equivalentDataPropertiesAxioms(prop)
            .flatMap(ax -> ax.properties())
            .filter(p -> !p.equals(prop) && !p.isAnonymous() && p instanceof OWLDataProperty)
            .map(OWLDataProperty.class::cast)
            .sorted(Comparator.comparing(p -> p.getIRI().getShortForm()))
            .collect(Collectors.toList());
            
        return candidateSupers.isEmpty() ? null : candidateSupers.get(0);
    }

    private OWLAnnotationProperty findAnnotationPropertyParent(OWLAnnotationProperty prop) {
        List<OWLAnnotationProperty> superProperties = ontology.subAnnotationPropertyOfAxioms(prop)
                .map(OWLSubAnnotationPropertyOfAxiom::getSuperProperty)
                .sorted(Comparator.comparing(p -> p.getIRI().getShortForm()))
                .collect(Collectors.toList());
        return superProperties.isEmpty() ? null : superProperties.get(0);
    }

    /**
     * Returns the direct parent of an entity if it exists.
     */
    public OWLEntity getDirectParent(OWLEntity entity) {
        return findParent(entity);
    }

    /**
     * Returns the top-level entities that do not have a parent.
     */
    public List<OWLEntity> getTopLevelEntities(Set<? extends OWLEntity> entities) {
        Map<OWLEntity, OWLEntity> parentMap = buildParentMap(entities);
        return entities.stream()
                .filter(e -> !parentMap.containsKey(e))
                .sorted(Comparator.comparing(e -> e.getIRI().getShortForm(), String.CASE_INSENSITIVE_ORDER))
                .collect(Collectors.toList());
    }

    /**
     * Returns a sorted list of individuals.
     */
    public List<OWLNamedIndividual> getIndividuals(Set<OWLNamedIndividual> individuals) {
        return individuals.stream()
                .sorted(Comparator.comparing(i -> i.getIRI().getShortForm(), String.CASE_INSENSITIVE_ORDER))
                .collect(Collectors.toList());
    }
}
