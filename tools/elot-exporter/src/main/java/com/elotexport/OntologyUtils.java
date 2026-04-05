package com.elotexport;

import org.semanticweb.owlapi.formats.PrefixDocumentFormat;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLDocumentFormat;
import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.model.OWLLiteral;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.util.DefaultPrefixManager;

public class OntologyUtils {
    public static DefaultPrefixManager createPrefixManager(OWLOntology ontology) {
        DefaultPrefixManager prefixManager = new DefaultPrefixManager();
        OWLDocumentFormat format = ontology.getOWLOntologyManager().getOntologyFormat(ontology);
        if (format instanceof PrefixDocumentFormat) {
            PrefixDocumentFormat prefixFormat = (PrefixDocumentFormat) format;
            prefixManager.copyPrefixesFrom(prefixFormat);
        }
        return prefixManager;
    }

    public static boolean isDeprecated(OWLEntity entity, OWLOntology ontology) {
        return ontology.annotationAssertionAxioms(entity.getIRI())
            .filter(ax -> ax.getProperty().isDeprecated())
            .anyMatch(ax -> ax.getValue().asLiteral()
                .map(OWLLiteral::parseBoolean)
                .orElse(false));
    }

    public static String getOntologyLocalName(OWLOntology ontology) {
        IRI ontologyIRI = ontology.getOntologyID().getOntologyIRI().orElse(IRI.create("unknown"));
        // If a non-empty fragment is available, use it.
        if (ontologyIRI.getFragment() != null && !ontologyIRI.getFragment().isEmpty()) {
            return ontologyIRI.getFragment();
        }
        // Otherwise, remove any trailing "/" or "#" from the IRI string.
        String iriStr = ontologyIRI.toString();
        while (iriStr.endsWith("/") || iriStr.endsWith("#")) {
            iriStr = iriStr.substring(0, iriStr.length() - 1);
        }
        int lastSlash = iriStr.lastIndexOf("/");
        if (lastSlash != -1 && lastSlash < iriStr.length() - 1) {
            return iriStr.substring(lastSlash + 1);
        }
        return iriStr;
    }
}
