/*
 * Copyright (C) 2024-2025 Johan W. Klüwer <johan.w.kluwer@gmail.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 */

package com.elotexport;

import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.manchestersyntax.renderer.ManchesterOWLSyntaxOWLObjectRendererImpl;
import org.semanticweb.owlapi.util.DefaultPrefixManager;

/**
 * Encapsulates the core objects and state needed for formatting OWL entities into Org-mode.
 */
public final class FormattingContext {
    private final OWLOntology ontology;
    private final DefaultPrefixManager prefixManager;
    private final TaxonomyBuilder taxonomy;
    private final ThreadLocal<ManchesterOWLSyntaxOWLObjectRendererImpl> renderers;
    private final String tangleTarget;

    public FormattingContext(OWLOntology ontology, DefaultPrefixManager prefixManager, TaxonomyBuilder taxonomy, String tangleTarget) {
        this.ontology = ontology;
        this.prefixManager = prefixManager;
        this.taxonomy = taxonomy;
        this.tangleTarget = tangleTarget;
        this.renderers = ThreadLocal.withInitial(() -> {
            ManchesterOWLSyntaxOWLObjectRendererImpl r = new ManchesterOWLSyntaxOWLObjectRendererImpl();
            r.setShortFormProvider(prefixManager);
            return r;
        });
    }

    public OWLOntology ontology() { return ontology; }
    public DefaultPrefixManager prefixManager() { return prefixManager; }
    public TaxonomyBuilder taxonomy() { return taxonomy; }
    public ManchesterOWLSyntaxOWLObjectRendererImpl renderer() { return renderers.get(); }
    public String tangleTarget() { return tangleTarget; }
}
