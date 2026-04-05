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
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.stream.Collectors;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.manchestersyntax.renderer.ManchesterOWLSyntaxOWLObjectRendererImpl;
import org.semanticweb.owlapi.util.DefaultPrefixManager;

public class OrgModeFormatter {
    private final FormattingContext context;
    private final String ontologySource;

    private final AnnotationFormatter annotationFormatter;
    private final AxiomFormatter axiomFormatter;
    private final HeaderGenerator headerGenerator;
    private final IndividualFormatter individualFormatter;
    private final DatatypeFormatter datatypeFormatter;
    private final OntologyDeclarationFormatter ontologyDeclarationFormatter;
    private final PrefixSectionGenerator prefixSectionGenerator;
    private final HeaderSectionGenerator headerSectionGenerator;
    private final HierarchyFormatter hierarchyFormatter;
    
    public OrgModeFormatter(OntologyLoader loader, String tangleTarget) {
        OWLOntology ontology = loader.getOntology();
        DefaultPrefixManager prefixManager = OntologyUtils.createPrefixManager(ontology);
        
        TaxonomyBuilder taxonomy = new TaxonomyBuilder(loader);

        if (tangleTarget == null || tangleTarget.isEmpty()) {
            String localName = OntologyUtils.getOntologyLocalName(ontology);
            tangleTarget = "./" + localName + ".omn";
        }

        this.context = new FormattingContext(ontology, prefixManager, taxonomy, tangleTarget);
        this.ontologySource = loader.getOntologySource();

        // Initialize components
        this.annotationFormatter = new AnnotationFormatter(context);
        this.axiomFormatter = new AxiomFormatter(context, annotationFormatter);
        this.headerGenerator = new HeaderGenerator(context, annotationFormatter);
        this.individualFormatter = new IndividualFormatter(context, headerGenerator, annotationFormatter);
        this.datatypeFormatter = new DatatypeFormatter(context, annotationFormatter, headerGenerator, axiomFormatter);
        this.ontologyDeclarationFormatter = new OntologyDeclarationFormatter(context, annotationFormatter);
        this.prefixSectionGenerator = new PrefixSectionGenerator(context);
        this.headerSectionGenerator = new HeaderSectionGenerator(context);
        this.hierarchyFormatter = new HierarchyFormatter(context, annotationFormatter, axiomFormatter, headerGenerator);
    }
    
    public String generateFullOutput() {
        String localName = OntologyUtils.getOntologyLocalName(context.ontology());

        // Fast non-parallelized parts
        String header = headerSectionGenerator.generateFullHeader(ontologySource);
        String contextHeader = headerSectionGenerator.generateContextHeader(localName, localName);
        String prefixSection = prefixSectionGenerator.generatePrefixSection();
        String ontologyDecl = ontologyDeclarationFormatter.formatOntologyDeclaration(localName);

        // Parallelized sections
        CompletableFuture<String> datatypesF = CompletableFuture.supplyAsync(() -> 
            HeaderGenerator.formatTopLevelHeading("Datatypes", "datatypes", localName) + "\n" +
            datatypeFormatter.generateDatatypeList(context.ontology().datatypesInSignature().collect(Collectors.toSet()), 3) + "\n"
        );

        CompletableFuture<String> classesF = CompletableFuture.supplyAsync(() -> {
            Set<OWLClass> allClasses = context.ontology().classesInSignature().collect(Collectors.toSet());
            return hierarchyFormatter.generateEntitySection("Classes", "class-hierarchy", localName, allClasses, 3);
        });

        CompletableFuture<String> objPropsF = CompletableFuture.supplyAsync(() -> {
            Set<OWLObjectProperty> allObjProps = context.ontology().objectPropertiesInSignature().collect(Collectors.toSet());
            return hierarchyFormatter.generateEntitySection("Object properties", "object-property-hierarchy", localName, allObjProps, 3);
        });

        CompletableFuture<String> dataPropsF = CompletableFuture.supplyAsync(() -> {
            Set<OWLDataProperty> allDataProps = context.ontology().dataPropertiesInSignature().collect(Collectors.toSet());
            return hierarchyFormatter.generateEntitySection("Data properties", "data-property-hierarchy", localName, allDataProps, 3);
        });

        CompletableFuture<String> annPropsF = CompletableFuture.supplyAsync(() -> {
            Set<OWLAnnotationProperty> allAnnProps = context.ontology().annotationPropertiesInSignature().collect(Collectors.toSet());
            return hierarchyFormatter.generateEntitySection("Annotation properties", "annotation-property-hierarchy", localName, allAnnProps, 3);
        });

        CompletableFuture<String> individualsF = CompletableFuture.supplyAsync(() -> {
            Set<OWLNamedIndividual> individuals = context.ontology().individualsInSignature().collect(Collectors.toSet());
            return individualFormatter.generateIndividualSection(localName, individuals);
        });

        StringBuilder sb = new StringBuilder();
        sb.append(header).append("\n");
        sb.append(contextHeader).append("\n");
        sb.append(prefixSection).append("\n");
        sb.append(ontologyDecl).append("\n");

        try {
            sb.append(datatypesF.get());
            sb.append(classesF.get());
            sb.append(objPropsF.get());
            sb.append(dataPropsF.get());
            sb.append(annPropsF.get());
            sb.append(individualsF.get());
        } catch (InterruptedException | ExecutionException e) {
            throw new RuntimeException("Error during parallel generation of ontology sections", e);
        }

        return sb.toString();
    }
    
}
