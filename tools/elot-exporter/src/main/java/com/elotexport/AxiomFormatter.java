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

public class AxiomFormatter {

    private final FormattingContext context;
    private final AnnotationFormatter annotationFormatter;

    public AxiomFormatter(FormattingContext context, AnnotationFormatter annotationFormatter) {
        this.context = context;
        this.annotationFormatter = annotationFormatter;
    }

    public String formatAdditionalSuperEntities(OWLEntity entity) {
        StringBuilder sb = new StringBuilder();
        OWLEntity directParent = context.taxonomy().getDirectParent(entity);
        String header = context.renderer().render(entity);

        if (entity instanceof OWLClass) {
            OWLClass cls = (OWLClass) entity;
            // Handle SubClassOf axioms
            List<OWLSubClassOfAxiom> subClassAxioms = context.ontology().subClassAxiomsForSubClass(cls)
                .collect(Collectors.toList());
            List<String> superClasses = subClassAxioms.stream()
                .map(ax -> context.renderer().render(ax.getSuperClass()))
                .sorted()
                .collect(Collectors.toList());
            if (directParent != null) {
                superClasses.removeIf(sc -> sc.equals(context.renderer().render(directParent)) || sc.equals(header));
            } else {
                superClasses.removeIf(sc -> sc.equals(header));
            }
            for (String sc : superClasses) {
                String indentedCls = sc.replaceAll("\\r?\\n", "\n      ");
                sb.append(" - SubClassOf :: ").append(indentedCls).append("\n");
                subClassAxioms.stream()
                    .filter(ax -> context.renderer().render(ax.getSuperClass()).equals(sc))
                    .forEach(ax -> sb.append(annotationFormatter.formatMetaAnnotations(ax.annotations(), 4)));
            }

            // Handle DisjointClasses axioms
            List<OWLDisjointClassesAxiom> disjointAxioms = context.ontology().disjointClassesAxioms(cls)
                .collect(Collectors.toList());

            // Filter out axioms that are handled by the separate "Disjointness clauses" section (size > 2)
            List<OWLDisjointClassesAxiom> localDisjointAxioms = disjointAxioms.stream()
                .filter(ax -> ax.classExpressions().count() <= 2)
                .collect(Collectors.toList());

            for (OWLDisjointClassesAxiom ax : localDisjointAxioms) {
                ax.classExpressions()
                    .filter(e -> !e.equals(cls))
                    .map(e -> context.renderer().render(e))
                    .sorted()
                    .forEach(rendered -> sb.append(" - DisjointWith :: ").append(rendered).append("\n"));
                sb.append(annotationFormatter.formatMetaAnnotations(ax.annotations(), 4));
            }

            // Handle DisjointUnionOf axioms
            List<OWLDisjointUnionAxiom> disjointUnionAxioms = context.ontology().disjointUnionAxioms(cls)
                .collect(Collectors.toList());
            if (!disjointUnionAxioms.isEmpty()) {
                for (OWLDisjointUnionAxiom ax : disjointUnionAxioms) {
                    List<String> members = ax.classExpressions()
                        .map(e -> context.renderer().render(e).replaceAll("\\r?\\n", "\n                     "))
                        .sorted()
                        .collect(Collectors.toList());
                    sb.append(" - DisjointUnionOf :: ").append(String.join(", ", members)).append("\n");
                    sb.append(annotationFormatter.formatMetaAnnotations(ax.annotations(), 4));
                }
            }

            // Handle EquivalentClasses axioms
            List<OWLEquivalentClassesAxiom> equivalentAxioms = context.ontology().equivalentClassesAxioms(cls)
                .collect(Collectors.toList());

            // Filter out axioms that are handled by the separate "Equivalence clauses" section (size > 2)
            List<OWLEquivalentClassesAxiom> localEquivalentAxioms = equivalentAxioms.stream()
                .filter(ax -> ax.classExpressions().count() <= 2)
                .collect(Collectors.toList());

            for (OWLEquivalentClassesAxiom ax : localEquivalentAxioms) {
                ax.classExpressions()
                    .filter(e -> !e.equals(cls))
                    .map(e -> context.renderer().render(e).replaceAll("\\r?\\n", "\n          "))
                    .filter(s -> !s.isEmpty())
                    .sorted()
                    .forEach(rendered -> sb.append(" - EquivalentTo :: ").append(rendered).append("\n"));
                sb.append(annotationFormatter.formatMetaAnnotations(ax.annotations(), 4));
            }

            // Handle HasKey axioms
            List<OWLHasKeyAxiom> hasKeyAxioms = context.ontology().hasKeyAxioms(cls)
                .collect(Collectors.toList());
            for (OWLHasKeyAxiom ax : hasKeyAxioms) {
                List<String> properties = ax.getPropertyExpressions().stream()
                    .map(pe -> context.renderer().render(pe))
                    .sorted()
                    .collect(Collectors.toList());
                if (!properties.isEmpty()) {
                    sb.append(" - HasKey :: ").append(String.join(", ", properties)).append("\n");
                    sb.append(annotationFormatter.formatMetaAnnotations(ax.annotations(), 4));
                }
            }
        } else if (entity instanceof OWLObjectProperty) {
            OWLObjectProperty prop = (OWLObjectProperty) entity;
            // Handle ObjectProperty axioms
            List<OWLSubObjectPropertyOfAxiom> subPropAxioms = context.ontology().objectSubPropertyAxiomsForSubProperty(prop)
                .collect(Collectors.toList());
            List<String> superProperties = subPropAxioms.stream()
                .map(ax -> context.renderer().render(ax.getSuperProperty()))
                .sorted()
                .collect(Collectors.toList());
            if (!superProperties.isEmpty()) {
                for (String superProp : superProperties) {
                    sb.append(" - SubPropertyOf :: ").append(superProp).append("\n");
                    subPropAxioms.stream()
                        .filter(ax -> context.renderer().render(ax.getSuperProperty()).equals(superProp))
                        .forEach(ax -> sb.append(annotationFormatter.formatMetaAnnotations(ax.annotations(), 4)));
                }
            }
            sb.append(formatAdditionalPropertyAxioms(entity));
        } else if (entity instanceof OWLDataProperty) {
            OWLDataProperty prop = (OWLDataProperty) entity;
            // Handle DataProperty axioms
            List<OWLSubDataPropertyOfAxiom> subPropAxioms = context.ontology().dataSubPropertyAxiomsForSubProperty(prop)
                .collect(Collectors.toList());
            List<String> superProperties = subPropAxioms.stream()
                .map(ax -> context.renderer().render(ax.getSuperProperty()))
                .sorted()
                .collect(Collectors.toList());
            if (!superProperties.isEmpty()) {
                for (String superProp : superProperties) {
                    sb.append(" - SubPropertyOf :: ").append(superProp).append("\n");
                    subPropAxioms.stream()
                        .filter(ax -> context.renderer().render(ax.getSuperProperty()).equals(superProp))
                        .forEach(ax -> sb.append(annotationFormatter.formatMetaAnnotations(ax.annotations(), 4)));
                }
            }
            sb.append(formatAdditionalPropertyAxioms(entity));
        } else if (entity instanceof OWLAnnotationProperty) {
            OWLAnnotationProperty prop = (OWLAnnotationProperty) entity;
            // Handle AnnotationProperty axioms
            List<String> superProperties = context.ontology().subAnnotationPropertyOfAxioms(prop)
                .map(OWLSubAnnotationPropertyOfAxiom::getSuperProperty)
                .map(ap -> context.renderer().render(ap))
                .sorted()
                .collect(Collectors.toList());
            if (!superProperties.isEmpty()) {
                for (String superProp : superProperties) {
                    sb.append(" - SubPropertyOf :: ").append(superProp).append("\n");
                }
            }
            // Handle Domains (annotation property domains are IRIs, rendered as full URIs)
            List<OWLAnnotationPropertyDomainAxiom> domainAxioms = context.ontology().annotationPropertyDomainAxioms(prop)
                .sorted(Comparator.comparing(ax -> ax.getDomain().toString()))
                .collect(Collectors.toList());
            for (OWLAnnotationPropertyDomainAxiom ax : domainAxioms) {
                sb.append(" - Domain :: <").append(ax.getDomain().toString()).append(">\n");
                sb.append(annotationFormatter.formatMetaAnnotations(ax.annotations(), 4));
            }
            // Handle Ranges (annotation property ranges are IRIs, rendered as full URIs)
            List<OWLAnnotationPropertyRangeAxiom> rangeAxioms = context.ontology().annotationPropertyRangeAxioms(prop)
                .sorted(Comparator.comparing(ax -> ax.getRange().toString()))
                .collect(Collectors.toList());
            for (OWLAnnotationPropertyRangeAxiom ax : rangeAxioms) {
                sb.append(" - Range :: <").append(ax.getRange().toString()).append(">\n");
                sb.append(annotationFormatter.formatMetaAnnotations(ax.annotations(), 4));
            }
        } else if (entity instanceof OWLDatatype) {
            OWLDatatype dt = (OWLDatatype) entity;
            context.ontology().axioms(AxiomType.DATATYPE_DEFINITION)
                .filter(ax -> ax.getDatatype().equals(dt))
                .forEach(ax -> {
                    String renderedRange = context.renderer().render(ax.getDataRange());
                    sb.append(" - EquivalentTo :: ").append(renderedRange).append("\n");
                    sb.append(annotationFormatter.formatMetaAnnotations(ax.annotations(), 4));
                });
        }
        return sb.toString();
    }

    public String formatAdditionalPropertyAxioms(OWLEntity entity) {
        StringBuilder sb = new StringBuilder();
        if (entity instanceof OWLObjectProperty) {
            OWLObjectProperty prop = (OWLObjectProperty) entity;
            // Handle Domains
            List<OWLObjectPropertyDomainAxiom> domainAxioms = context.ontology().objectPropertyDomainAxioms(prop)
                .sorted(Comparator.comparing(ax -> context.renderer().render(ax.getDomain())))
                .collect(Collectors.toList());
            for (OWLObjectPropertyDomainAxiom ax : domainAxioms) {
                String rendered = context.renderer().render(ax.getDomain()).replaceAll("\\r?\\n", "\n          ");
                sb.append(" - Domain :: ").append(rendered).append("\n");
                sb.append(annotationFormatter.formatMetaAnnotations(ax.annotations(), 4));
            }
            // Handle Ranges
            List<OWLObjectPropertyRangeAxiom> rangeAxioms = context.ontology().objectPropertyRangeAxioms(prop)
                .sorted(Comparator.comparing(ax -> context.renderer().render(ax.getRange())))
                .collect(Collectors.toList());
            for (OWLObjectPropertyRangeAxiom ax : rangeAxioms) {
                String rendered = context.renderer().render(ax.getRange()).replaceAll("\\r?\\n", "\n          ");
                sb.append(" - Range :: ").append(rendered).append("\n");
                sb.append(annotationFormatter.formatMetaAnnotations(ax.annotations(), 4));
            }
            // Handle InverseOf
            List<OWLInverseObjectPropertiesAxiom> inverseAxioms = context.ontology().inverseObjectPropertyAxioms(prop)
                .collect(Collectors.toList());
            List<String> inverses = inverseAxioms.stream()
                .map(ax -> ax.getFirstProperty().equals(prop)
                          ? context.renderer().render(ax.getSecondProperty()).replaceAll("\\r?\\n", "\n          ")
                          : context.renderer().render(ax.getFirstProperty()).replaceAll("\\r?\\n", "\n          "))
                .distinct()
                .sorted()
                .collect(Collectors.toList());
            for (int i = 0; i < inverses.size(); i++) {
                sb.append(" - InverseOf :: ").append(inverses.get(i)).append("\n");
                final int idx = i;
                inverseAxioms.stream()
                    .filter(ax -> {
                        String rendered = ax.getFirstProperty().equals(prop)
                            ? context.renderer().render(ax.getSecondProperty()).replaceAll("\\r?\\n", "\n          ")
                            : context.renderer().render(ax.getFirstProperty()).replaceAll("\\r?\\n", "\n          ");
                        return rendered.equals(inverses.get(idx));
                    })
                    .forEach(ax -> sb.append(annotationFormatter.formatMetaAnnotations(ax.annotations(), 4)));
            }
            // Handle EquivalentObjectProperties
            List<OWLEquivalentObjectPropertiesAxiom> equivalentAxioms = context.ontology().equivalentObjectPropertiesAxioms(prop)
                .filter(ax -> ax.properties().count() <= 2)
                .collect(Collectors.toList());
            for (OWLEquivalentObjectPropertiesAxiom ax : equivalentAxioms) {
                ax.properties()
                    .filter(p -> !p.equals(prop))
                    .map(p -> context.renderer().render(p).replaceAll("\\r?\\n", "\n          "))
                    .sorted()
                    .forEach(rendered -> sb.append(" - EquivalentTo :: ").append(rendered).append("\n"));
                sb.append(annotationFormatter.formatMetaAnnotations(ax.annotations(), 4));
            }
            // Handle Property Chains
            List<OWLSubPropertyChainOfAxiom> chainAxioms = context.ontology().axioms(AxiomType.SUB_PROPERTY_CHAIN_OF)
                .filter(ax -> ax.getSuperProperty().equals(prop))
                .collect(Collectors.toList());
            List<String> propertyChains = chainAxioms.stream()
                .map(ax -> ax.getPropertyChain().stream()
                     .map(ope -> context.renderer().render(ope).replaceAll("\\r?\\n", "\n          "))
                     .collect(Collectors.joining(" o ")))
                .distinct()
                .sorted()
                .collect(Collectors.toList());
            if (!propertyChains.isEmpty()) {
                for (String chain : propertyChains) {
                    sb.append(" - SubPropertyChain :: ").append(chain).append("\n");
                    chainAxioms.stream()
                        .filter(ax -> {
                            String rendered = ax.getPropertyChain().stream()
                                .map(ope -> context.renderer().render(ope).replaceAll("\\r?\\n", "\n          "))
                                .collect(Collectors.joining(" o "));
                            return rendered.equals(chain);
                        })
                        .forEach(ax -> sb.append(annotationFormatter.formatMetaAnnotations(ax.annotations(), 4)));
                }
            }

            // Handle Characteristics
            List<String> characteristics = new ArrayList<>();
            if (context.ontology().functionalObjectPropertyAxioms(prop).findAny().isPresent()) {
                characteristics.add("Functional");
            }
            if (context.ontology().inverseFunctionalObjectPropertyAxioms(prop).findAny().isPresent()) {
                characteristics.add("InverseFunctional");
            }
            if (context.ontology().transitiveObjectPropertyAxioms(prop).findAny().isPresent()) {
                characteristics.add("Transitive");
            }
            if (context.ontology().symmetricObjectPropertyAxioms(prop).findAny().isPresent()) {
                characteristics.add("Symmetric");
            }
            if (context.ontology().asymmetricObjectPropertyAxioms(prop).findAny().isPresent()) {
                characteristics.add("Asymmetric");
            }
            if (context.ontology().reflexiveObjectPropertyAxioms(prop).findAny().isPresent()) {
                characteristics.add("Reflexive");
            }
            if (context.ontology().irreflexiveObjectPropertyAxioms(prop).findAny().isPresent()) {
                characteristics.add("Irreflexive");
            }
            if (!characteristics.isEmpty()) {
                sb.append(" - Characteristics :: ").append(String.join(", ", characteristics)).append("\n");
            }

            // Handle DisjointObjectProperties
            List<OWLDisjointObjectPropertiesAxiom> disjointAxioms = context.ontology().disjointObjectPropertiesAxioms(prop)
                .filter(ax -> ax.properties().count() <= 2)
                .collect(Collectors.toList());
            for (OWLDisjointObjectPropertiesAxiom ax : disjointAxioms) {
                ax.properties()
                    .filter(p -> !p.equals(prop))
                    .map(p -> context.renderer().render(p))
                    .sorted()
                    .forEach(rendered -> sb.append(" - DisjointWith :: ").append(rendered).append("\n"));
                sb.append(annotationFormatter.formatMetaAnnotations(ax.annotations(), 4));
            }
        } else if (entity instanceof OWLDataProperty) {
            OWLDataProperty prop = (OWLDataProperty) entity;
            // Handle Domains
            List<OWLDataPropertyDomainAxiom> domainAxioms = context.ontology().dataPropertyDomainAxioms(prop)
                .sorted(Comparator.comparing(ax -> context.renderer().render(ax.getDomain())))
                .collect(Collectors.toList());
            for (OWLDataPropertyDomainAxiom ax : domainAxioms) {
                String rendered = context.renderer().render(ax.getDomain()).replaceAll("\\r?\\n", "\n      ");
                sb.append(" - Domain :: ").append(rendered).append("\n");
                sb.append(annotationFormatter.formatMetaAnnotations(ax.annotations(), 4));
            }
            // Handle Ranges
            List<OWLDataPropertyRangeAxiom> rangeAxioms = context.ontology().dataPropertyRangeAxioms(prop)
                .sorted(Comparator.comparing(ax -> context.renderer().render(ax.getRange())))
                .collect(Collectors.toList());
            for (OWLDataPropertyRangeAxiom ax : rangeAxioms) {
                String rendered = context.renderer().render(ax.getRange()).replaceAll("\\r?\\n", "\n      ");
                sb.append(" - Range :: ").append(rendered).append("\n");
                sb.append(annotationFormatter.formatMetaAnnotations(ax.annotations(), 4));
            }
            // Handle EquivalentDataProperties
            List<OWLEquivalentDataPropertiesAxiom> equivalentAxioms = context.ontology().equivalentDataPropertiesAxioms(prop)
                .filter(ax -> ax.properties().count() <= 2)
                .collect(Collectors.toList());
            for (OWLEquivalentDataPropertiesAxiom ax : equivalentAxioms) {
                ax.properties()
                    .filter(p -> !p.equals(prop))
                    .map(p -> context.renderer().render(p).replaceAll("\\r?\\n", "\n          "))
                    .sorted()
                    .forEach(rendered -> sb.append(" - EquivalentTo :: ").append(rendered).append("\n"));
                sb.append(annotationFormatter.formatMetaAnnotations(ax.annotations(), 4));
            }
            // Handle Characteristics
            List<String> characteristics = new ArrayList<>();
            if (context.ontology().functionalDataPropertyAxioms(prop).findAny().isPresent()) {
                characteristics.add("Functional");
            }
            if (!characteristics.isEmpty()) {
                sb.append(" - Characteristics :: ").append(String.join(", ", characteristics)).append("\n");
            }

            // Handle DisjointDataProperties
            List<OWLDisjointDataPropertiesAxiom> disjointAxioms = context.ontology().disjointDataPropertiesAxioms(prop)
                .filter(ax -> ax.properties().count() <= 2)
                .collect(Collectors.toList());
            for (OWLDisjointDataPropertiesAxiom ax : disjointAxioms) {
                ax.properties()
                    .filter(p -> !p.equals(prop))
                    .map(p -> context.renderer().render(p))
                    .sorted()
                    .forEach(rendered -> sb.append(" - DisjointWith :: ").append(rendered).append("\n"));
                sb.append(annotationFormatter.formatMetaAnnotations(ax.annotations(), 4));
            }
        }

        return sb.toString();
    }

    public String generateEquivalenceClauses() {
        List<OWLEquivalentClassesAxiom> axioms = context.ontology().axioms(AxiomType.EQUIVALENT_CLASSES)
            .sorted(Comparator.comparing(ax -> context.renderer().render(ax)))
            .collect(Collectors.toList());
        LinkedHashMap<String, OWLEquivalentClassesAxiom> clauseToAxiom = new LinkedHashMap<>();
        for (OWLEquivalentClassesAxiom ax : axioms) {
            List<String> renderedExprs = ax.classExpressions()
                .map(e -> context.renderer().render(e))
                .collect(Collectors.toList());
            if (renderedExprs.size() > 2) {
                Collections.sort(renderedExprs);
                String clause = "EquivalentClasses :: " + String.join(", ", renderedExprs);
                clauseToAxiom.putIfAbsent(clause, ax);
            }
        }
        if (clauseToAxiom.isEmpty()) {
            return "";
        }
        List<String> sortedClauses = new ArrayList<>(clauseToAxiom.keySet());
        Collections.sort(sortedClauses);
        StringBuilder sb = new StringBuilder();
        sb.append("*** Equivalence clauses                                           :nodeclare:\n");
        for (String clause : sortedClauses) {
            sb.append(" - ").append(clause).append("\n");
            sb.append(annotationFormatter.formatMetaAnnotations(clauseToAxiom.get(clause).annotations(), 4));
        }
        return sb.toString();
    }

    public String generateDisjointnessClauses() {
        List<OWLDisjointClassesAxiom> axioms = context.ontology().axioms(AxiomType.DISJOINT_CLASSES)
            .sorted(Comparator.comparing(ax -> context.renderer().render(ax)))
            .collect(Collectors.toList());
        LinkedHashMap<String, OWLDisjointClassesAxiom> clauseToAxiom = new LinkedHashMap<>();
        for (OWLDisjointClassesAxiom ax : axioms) {
            List<String> renderedExprs = ax.classExpressions()
                .map(e -> context.renderer().render(e))
                .collect(Collectors.toList());
            if (renderedExprs.size() > 2) {
                Collections.sort(renderedExprs);
                String clause = "DisjointClasses :: " + String.join(", ", renderedExprs);
                clauseToAxiom.putIfAbsent(clause, ax);
            }
        }
        if (clauseToAxiom.isEmpty()) {
            return "";
        }
        List<String> sortedClauses = new ArrayList<>(clauseToAxiom.keySet());
        Collections.sort(sortedClauses);
        StringBuilder sb = new StringBuilder();
        sb.append("*** Disjointness clauses                                          :nodeclare:\n");
        for (String clause : sortedClauses) {
            sb.append(" - ").append(clause).append("\n");
            sb.append(annotationFormatter.formatMetaAnnotations(clauseToAxiom.get(clause).annotations(), 4));
        }
        return sb.toString();
    }

    public String generateDifferentnessClauses() {
        List<OWLDifferentIndividualsAxiom> axioms = context.ontology().axioms(AxiomType.DIFFERENT_INDIVIDUALS)
            .sorted(Comparator.comparing(ax -> context.renderer().render(ax)))
            .collect(Collectors.toList());
        LinkedHashMap<String, OWLDifferentIndividualsAxiom> clauseToAxiom = new LinkedHashMap<>();
        for (OWLDifferentIndividualsAxiom ax : axioms) {
            Set<String> indSet = ax.individuals()
                .filter(e -> !e.isAnonymous())
                .map(e -> context.renderer().render((OWLNamedIndividual) e))
                .collect(Collectors.toSet());
            if (indSet.size() > 2) {
                List<String> sortedInds = new ArrayList<>(indSet);
                Collections.sort(sortedInds);
                String clause = "DifferentIndividuals :: " + String.join(", ", sortedInds);
                clauseToAxiom.putIfAbsent(clause, ax);
            }
        }
        if (clauseToAxiom.isEmpty()) {
            return "";
        }
        List<String> sortedClauses = new ArrayList<>(clauseToAxiom.keySet());
        Collections.sort(sortedClauses);
        StringBuilder sb = new StringBuilder();
        sb.append("*** Differentness clauses                                          :nodeclare:\n");
        for (String clause : sortedClauses) {
            sb.append(" - ").append(clause).append("\n");
            sb.append(annotationFormatter.formatMetaAnnotations(clauseToAxiom.get(clause).annotations(), 4));
        }
        return sb.toString();
    }

    public String generateEquivalentObjectPropertiesClauses() {
        List<OWLEquivalentObjectPropertiesAxiom> axioms = context.ontology().axioms(AxiomType.EQUIVALENT_OBJECT_PROPERTIES)
            .sorted(Comparator.comparing(ax -> context.renderer().render(ax)))
            .collect(Collectors.toList());
        LinkedHashMap<String, OWLEquivalentObjectPropertiesAxiom> clauseToAxiom = new LinkedHashMap<>();
        for (OWLEquivalentObjectPropertiesAxiom ax : axioms) {
            List<String> renderedProps = ax.properties()
                .map(p -> context.renderer().render(p))
                .collect(Collectors.toList());
            if (renderedProps.size() > 2) {
                Collections.sort(renderedProps);
                String clause = "EquivalentProperties :: " + String.join(", ", renderedProps);
                clauseToAxiom.putIfAbsent(clause, ax);
            }
        }
        if (clauseToAxiom.isEmpty()) {
            return "";
        }
        List<String> sortedClauses = new ArrayList<>(clauseToAxiom.keySet());
        Collections.sort(sortedClauses);
        StringBuilder sb = new StringBuilder();
        sb.append("*** Equivalence clauses                                           :nodeclare:\n");
        for (String clause : sortedClauses) {
            sb.append(" - ").append(clause).append("\n");
            sb.append(annotationFormatter.formatMetaAnnotations(clauseToAxiom.get(clause).annotations(), 4));
        }
        return sb.toString();
    }

    public String generateDisjointObjectPropertiesClauses() {
        List<OWLDisjointObjectPropertiesAxiom> axioms = context.ontology().axioms(AxiomType.DISJOINT_OBJECT_PROPERTIES)
            .sorted(Comparator.comparing(ax -> context.renderer().render(ax)))
            .collect(Collectors.toList());
        LinkedHashMap<String, OWLDisjointObjectPropertiesAxiom> clauseToAxiom = new LinkedHashMap<>();
        for (OWLDisjointObjectPropertiesAxiom ax : axioms) {
            List<String> renderedProps = ax.properties()
                .map(p -> context.renderer().render(p))
                .collect(Collectors.toList());
            if (renderedProps.size() > 2) {
                Collections.sort(renderedProps);
                String clause = "DisjointProperties :: " + String.join(", ", renderedProps);
                clauseToAxiom.putIfAbsent(clause, ax);
            }
        }
        if (clauseToAxiom.isEmpty()) {
            return "";
        }
        List<String> sortedClauses = new ArrayList<>(clauseToAxiom.keySet());
        Collections.sort(sortedClauses);
        StringBuilder sb = new StringBuilder();
        sb.append("*** Disjointness clauses                                          :nodeclare:\n");
        for (String clause : sortedClauses) {
            sb.append(" - ").append(clause).append("\n");
            sb.append(annotationFormatter.formatMetaAnnotations(clauseToAxiom.get(clause).annotations(), 4));
        }
        return sb.toString();
    }

    public String generateEquivalentDataPropertiesClauses() {
        List<OWLEquivalentDataPropertiesAxiom> axioms = context.ontology().axioms(AxiomType.EQUIVALENT_DATA_PROPERTIES)
            .sorted(Comparator.comparing(ax -> context.renderer().render(ax)))
            .collect(Collectors.toList());
        LinkedHashMap<String, OWLEquivalentDataPropertiesAxiom> clauseToAxiom = new LinkedHashMap<>();
        for (OWLEquivalentDataPropertiesAxiom ax : axioms) {
            List<String> renderedProps = ax.properties()
                .map(p -> context.renderer().render(p))
                .collect(Collectors.toList());
            if (renderedProps.size() > 2) {
                Collections.sort(renderedProps);
                String clause = "EquivalentProperties :: " + String.join(", ", renderedProps);
                clauseToAxiom.putIfAbsent(clause, ax);
            }
        }
        if (clauseToAxiom.isEmpty()) {
            return "";
        }
        List<String> sortedClauses = new ArrayList<>(clauseToAxiom.keySet());
        Collections.sort(sortedClauses);
        StringBuilder sb = new StringBuilder();
        sb.append("*** Equivalence clauses                                           :nodeclare:\n");
        for (String clause : sortedClauses) {
            sb.append(" - ").append(clause).append("\n");
            sb.append(annotationFormatter.formatMetaAnnotations(clauseToAxiom.get(clause).annotations(), 4));
        }
        return sb.toString();
    }

    public String generateDisjointDataPropertiesClauses() {
        List<OWLDisjointDataPropertiesAxiom> axioms = context.ontology().axioms(AxiomType.DISJOINT_DATA_PROPERTIES)
            .sorted(Comparator.comparing(ax -> context.renderer().render(ax)))
            .collect(Collectors.toList());
        LinkedHashMap<String, OWLDisjointDataPropertiesAxiom> clauseToAxiom = new LinkedHashMap<>();
        for (OWLDisjointDataPropertiesAxiom ax : axioms) {
            List<String> renderedProps = ax.properties()
                .map(p -> context.renderer().render(p))
                .collect(Collectors.toList());
            if (renderedProps.size() > 2) {
                Collections.sort(renderedProps);
                String clause = "DisjointProperties :: " + String.join(", ", renderedProps);
                clauseToAxiom.putIfAbsent(clause, ax);
            }
        }
        if (clauseToAxiom.isEmpty()) {
            return "";
        }
        List<String> sortedClauses = new ArrayList<>(clauseToAxiom.keySet());
        Collections.sort(sortedClauses);
        StringBuilder sb = new StringBuilder();
        sb.append("*** Disjointness clauses                                          :nodeclare:\n");
        for (String clause : sortedClauses) {
            sb.append(" - ").append(clause).append("\n");
            sb.append(annotationFormatter.formatMetaAnnotations(clauseToAxiom.get(clause).annotations(), 4));
        }
        return sb.toString();
    }

    public String generateSameIndividualClauses() {
        List<OWLSameIndividualAxiom> axioms = context.ontology().axioms(AxiomType.SAME_INDIVIDUAL)
            .sorted(Comparator.comparing(ax -> context.renderer().render(ax)))
            .collect(Collectors.toList());
        LinkedHashMap<String, OWLSameIndividualAxiom> clauseToAxiom = new LinkedHashMap<>();
        for (OWLSameIndividualAxiom ax : axioms) {
            Set<String> indSet = ax.individuals()
                .filter(e -> !e.isAnonymous())
                .map(e -> context.renderer().render((OWLNamedIndividual) e))
                .collect(Collectors.toSet());
            if (indSet.size() > 2) {
                List<String> sortedInds = new ArrayList<>(indSet);
                Collections.sort(sortedInds);
                String clause = "SameIndividual :: " + String.join(", ", sortedInds);
                clauseToAxiom.putIfAbsent(clause, ax);
            }
        }
        if (clauseToAxiom.isEmpty()) {
            return "";
        }
        List<String> sortedClauses = new ArrayList<>(clauseToAxiom.keySet());
        Collections.sort(sortedClauses);
        StringBuilder sb = new StringBuilder();
        sb.append("*** Sameness clauses                                              :nodeclare:\n");
        for (String clause : sortedClauses) {
            sb.append(" - ").append(clause).append("\n");
            sb.append(annotationFormatter.formatMetaAnnotations(clauseToAxiom.get(clause).annotations(), 4));
        }
        return sb.toString();
    }

    public String generateGCIClauses() {
        List<OWLSubClassOfAxiom> subClassAxioms = context.ontology().axioms(AxiomType.SUBCLASS_OF)
            .filter(ax -> !ax.getSubClass().isOWLClass())
            .sorted(Comparator.comparing(ax -> context.renderer().render(ax)))
            .collect(Collectors.toList());

        List<OWLEquivalentClassesAxiom> eqAxioms = context.ontology().axioms(AxiomType.EQUIVALENT_CLASSES)
            .filter(ax -> ax.classExpressions().noneMatch(OWLClassExpression::isOWLClass))
            .sorted(Comparator.comparing(ax -> context.renderer().render(ax)))
            .collect(Collectors.toList());

        if (subClassAxioms.isEmpty() && eqAxioms.isEmpty()) {
            return "";
        }

        StringBuilder sb = new StringBuilder();
        sb.append("*** GCI clauses                                                   :nodeclare:\n");
        for (OWLSubClassOfAxiom ax : subClassAxioms) {
            String sub = context.renderer().render(ax.getSubClass()).replaceAll("\\r?\\n", "\n  ");
            String sup = context.renderer().render(ax.getSuperClass()).replaceAll("\\r?\\n", "\n              ");
            sb.append("#+begin_src omn\n");
            sb.append("Class: ").append(sub).append("\n");
            sb.append("  SubClassOf: ").append(sup).append("\n");
            sb.append("#+end_src\n");
            sb.append(annotationFormatter.formatMetaAnnotations(ax.annotations(), 4));
        }
        for (OWLEquivalentClassesAxiom ax : eqAxioms) {
            List<OWLClassExpression> exprs = ax.classExpressions()
                .sorted(Comparator.comparing(e -> context.renderer().render(e)))
                .collect(Collectors.toList());
            if (exprs.size() >= 2) {
                String first = context.renderer().render(exprs.get(0)).replaceAll("\\r?\\n", "\n  ");
                sb.append("#+begin_src omn\n");
                sb.append("Class: ").append(first).append("\n");
                for (int i = 1; i < exprs.size(); i++) {
                    String other = context.renderer().render(exprs.get(i)).replaceAll("\\r?\\n", "\n                ");
                    sb.append("  EquivalentTo: ").append(other).append("\n");
                }
                sb.append("#+end_src\n");
                sb.append(annotationFormatter.formatMetaAnnotations(ax.annotations(), 4));
            }
        }
        return sb.toString();
    }
}
