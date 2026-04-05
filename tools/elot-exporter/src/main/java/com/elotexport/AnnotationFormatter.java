/*
 * Copyright (C) 2024-2025 Johan W. Klüwer <johan.w.kluwer@gmail.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 */

package com.elotexport;

import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.manchestersyntax.renderer.ManchesterOWLSyntaxOWLObjectRendererImpl;
import org.semanticweb.owlapi.util.DefaultPrefixManager;

public class AnnotationFormatter {

    public record LabelInfo(String label, org.semanticweb.owlapi.model.IRI sourceOntologyIRI) {}

    private final FormattingContext context;
    private final java.util.Map<OWLEntity, java.util.Optional<LabelInfo>> labelCache = new java.util.concurrent.ConcurrentHashMap<>();

    public AnnotationFormatter(FormattingContext context) {
        this.context = context;
    }

    public String getEntityLabel(OWLEntity entity) {
        LabelInfo li = getEntityLabelWithSource(entity);
        return li != null ? li.label() : null;
    }
    
    private String getLabelFromOntology(OWLEntity entity, OWLOntology ontology) {
        for (String labelIRI : OntologyConstants.LABEL_IRIS) {
            List<OWLAnnotationAssertionAxiom> labelAxioms = ontology.annotationAssertionAxioms(entity.getIRI())
                    .filter(ax -> ax.getProperty().getIRI().toString().equals(labelIRI))
                    .collect(Collectors.toList());
            String nonEnCandidate = null;
            for (OWLAnnotationAssertionAxiom ax : labelAxioms) {
                OWLAnnotationValue value = ax.getValue();
                if (value instanceof OWLLiteral) {
                    OWLLiteral lit = (OWLLiteral) value;
                    String lang = lit.getLang();
                    if (lang != null && !lang.isEmpty() && lang.toLowerCase().startsWith("en")) {
                        return "\"" + lit.getLiteral() + "\"@" + lang;
                    } else if (nonEnCandidate == null) {
                        nonEnCandidate = (lang != null && !lang.isEmpty()) ? ("\"" + lit.getLiteral() + "\"@" + lang) : lit.getLiteral();
                    }
                }
            }
            if (nonEnCandidate != null) {
                return nonEnCandidate;
            }
        }
        return null;
    }

    private LabelInfo findLabelDFS(OWLEntity entity, OWLOntology ontology, java.util.Set<OWLOntology> visited) {
        if (visited.contains(ontology)) {
            return null;
        }
        visited.add(ontology);
        String label = getLabelFromOntology(entity, ontology);
        if (label != null) {
            org.semanticweb.owlapi.model.IRI source = ontology.getOntologyID().getOntologyIRI().orElse(org.semanticweb.owlapi.model.IRI.create("unknown"));
            return new LabelInfo(label, source);
        }
        for (OWLOntology directImport : ontology.getDirectImports()) {
            LabelInfo li = findLabelDFS(entity, directImport, visited);
            if (li != null) {
                return li;
            }
        }
        return null;
    }

    public LabelInfo getEntityLabelWithSource(OWLEntity entity) {
        return labelCache.computeIfAbsent(entity, e -> {
            java.util.Set<OWLOntology> visited = new java.util.HashSet<>();
            return java.util.Optional.ofNullable(findLabelDFS(e, context.ontology(), visited));
        }).orElse(null);
    }

    public String formatAnnotationValue(OWLAnnotationValue value) {
        return formatValue(value, false);
    }

    public String formatDataPropertyValue(OWLLiteral value) {
        return formatValue(value, true);
    }

    private String formatValue(OWLObject value, boolean quoteStrings) {
        if (value instanceof OWLLiteral) {
            OWLLiteral lit = (OWLLiteral) value;
            String literalValue = formatMultilineLiteral(lit.getLiteral());
            if (lit.getLang() != null && !lit.getLang().isEmpty()) {
                literalValue = literalValue.replace("\"", "\\\"");
                return "\"" + literalValue + "\"@" + lit.getLang();
            } else {
                String dt = context.renderer().render(lit.getDatatype());
                if(dt.startsWith("<") && dt.endsWith(">")) {
                    dt = dt.substring(1, dt.length()-1);
                }
                if(dt.equals("http://www.w3.org/2001/XMLSchema#string") || dt.equals("xsd:string")) {
                    if (quoteStrings) {
                        return "\"" + literalValue.replace("\"", "\\\"") + "\"";
                    } else {
                        return literalValue;
                    }
                } else if ((dt.equals("http://www.w3.org/2001/XMLSchema#boolean") || dt.equals("xsd:boolean"))
                    && (literalValue.equals("true") || literalValue.equals("false"))) {
                    return literalValue;
                } else if (isNumericDatatype(dt)) {
                    return literalValue;
                } else {
                    if(dt.startsWith("http://www.w3.org/2001/XMLSchema#")) {
                        dt = "xsd:" + dt.substring("http://www.w3.org/2001/XMLSchema#".length());
                    }
                    return "\"" + literalValue.replace("\"", "\\\"") + "\"^^" + dt;
                }
            }
        }
        if (value instanceof IRI) {
            IRI iri = (IRI) value;
            String iriStr = iri.toString();
            if (iriStr.startsWith("urn:")) {
                return "<" + iriStr + ">";
            }
            if(context.prefixManager() != null) {
                String shortForm = getShortFormForIRI(iri);
                if (shortForm.equals(iriStr) && (iriStr.startsWith("http://") || iriStr.startsWith("https://"))) {
                    return iriStr;
                }
                return shortForm;
            } else {
                String result = context.renderer().render(iri);
                if(result.startsWith("<") && result.endsWith(">")) {
                    result = result.substring(1, result.length()-1);
                }
                return result;
            }
        } else {
            String result = context.renderer().render(value);
            if(result.startsWith("<") && result.endsWith(">")) {
                result = result.substring(1, result.length()-1);
            }
            return result;
        }
    }

    private String getShortFormForIRI(IRI iri) {
        String iriStr = iri.toString();
        if (iriStr.startsWith("urn:")) {
            return "<" + iriStr + ">";
        }
        Map<String, String> prefixMap = context.prefixManager().getPrefixName2PrefixMap();
        for (Map.Entry<String, String> entry : prefixMap.entrySet()) {
            String prefixName = entry.getKey();
            String prefixIRI = entry.getValue();
            if (iriStr.startsWith(prefixIRI)) {
                if (prefixName.endsWith(":")) {
                    return prefixName + iriStr.substring(prefixIRI.length());
                } else {
                    return prefixName + ":" + iriStr.substring(prefixIRI.length());
                }
            }
        }
        return iriStr;
    }

    public String formatAnnotations(OWLEntity entity) {
        StringBuilder sb = new StringBuilder();
        String headingLabel = getEntityLabel(entity);
        context.ontology().annotationAssertionAxioms(entity.getIRI())
            .sorted((ax1, ax2) -> {
                int res = context.renderer().render(ax1.getProperty()).compareTo(context.renderer().render(ax2.getProperty()));
                if (res != 0) return res;
                return context.renderer().render(ax1.getValue()).compareTo(context.renderer().render(ax2.getValue()));
            })
            .forEach(ax -> {
                String prop = context.renderer().render(ax.getProperty());
                String value = formatAnnotationValue(ax.getValue());

                // Skip if this annotation is exactly RDFS_LABEL and matches headingLabel
                if (OntologyConstants.RDFS_LABEL.equals(ax.getProperty().getIRI().toString())) {
                    if (headingLabel != null && value.equals(headingLabel)) {
                        return;
                    }
                }

                sb.append(" - ").append(prop).append(" :: ").append(value).append("\n");

                sb.append(formatMetaAnnotations(ax.annotations(), 4));
            });
        return sb.toString();
    }
    
    public String formatMetaAnnotations(java.util.stream.Stream<OWLAnnotation> annotations, int indentLevel) {
        StringBuilder sb = new StringBuilder();
        String indent = " ".repeat(indentLevel);
        annotations
            .sorted((m1, m2) -> {
                int res = context.renderer().render(m1.getProperty()).compareTo(context.renderer().render(m2.getProperty()));
                if (res != 0) return res;
                return context.renderer().render(m1.getValue()).compareTo(context.renderer().render(m2.getValue()));
            })
            .forEach(metaAx -> {
                String metaProp = context.renderer().render(metaAx.getProperty());
                String metaValue = formatAnnotationValue(metaAx.getValue());
                sb.append(indent).append("- ").append(metaProp).append(" :: ").append(metaValue).append("\n");
            });
        return sb.toString();
    }

    private boolean isNumericDatatype(String dt) {
        return dt.endsWith("#integer") || dt.endsWith("#decimal") || dt.endsWith("#float") || dt.endsWith("#double") ||
               dt.endsWith("#long") || dt.endsWith("#int") || dt.endsWith("#short") || dt.endsWith("#byte") ||
               dt.endsWith("#nonNegativeInteger") || dt.endsWith("#nonPositiveInteger") ||
               dt.endsWith("#positiveInteger") || dt.endsWith("#negativeInteger") ||
               dt.endsWith("#unsignedLong") || dt.endsWith("#unsignedInt") || dt.endsWith("#unsignedShort") || dt.endsWith("#unsignedByte") ||
               dt.equals("xsd:integer") || dt.equals("xsd:decimal") || dt.equals("xsd:float") || dt.equals("xsd:double") ||
               dt.equals("xsd:long") || dt.equals("xsd:int") || dt.equals("xsd:short") || dt.equals("xsd:byte") ||
               dt.equals("xsd:nonNegativeInteger") || dt.equals("xsd:nonPositiveInteger") ||
               dt.equals("xsd:positiveInteger") || dt.equals("xsd:negativeInteger") ||
               dt.equals("xsd:unsignedLong") || dt.equals("xsd:unsignedInt") || dt.equals("xsd:unsignedShort") || dt.equals("xsd:unsignedByte");
    }

    private String formatMultilineLiteral(String literal) {
        if (literal.contains("\n")) {
            String[] lines = literal.split("\\r?\\n", -1);
            StringBuilder sb = new StringBuilder();
            sb.append(lines[0].stripTrailing());
            for (int i = 1; i < lines.length; i++) {
                sb.append("\n").append("          ").append(lines[i]);
            }
            return sb.toString();
        }
        return literal;
    }
}
