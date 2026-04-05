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
import java.util.stream.Collectors;

public class PrefixSectionGenerator {

    private final FormattingContext context;

    public PrefixSectionGenerator(FormattingContext context) {
        this.context = context;
    }

    public String generatePrefixSection() {
        StringBuilder sb = new StringBuilder();
        sb.append("** Prefixes\n");
        sb.append(":PROPERTIES:\n");
        sb.append(":prefixdefs: yes\n");
        sb.append(":END:\n");
        sb.append("The ontology document in OWL employs the namespace prefixes of table [[prefix-table]].\n\n");
        sb.append("#+name: prefix-table\n");
        sb.append("#+attr_latex: :align lp{.8\\textwidth} :font small\n");
        sb.append("#+caption: OWL ontology prefixes\n");
        sb.append("| prefix   | uri |\n");
        sb.append("|----------+-----|\n");
        // Get a sorted list of prefix names.
        List<String> prefixes = context.prefixManager().getPrefixNames().stream()
                .sorted(String.CASE_INSENSITIVE_ORDER)
                .collect(Collectors.toList());
        for (String prefixName : prefixes) {
            String uri = context.prefixManager().getPrefix(prefixName);
            sb.append("| ").append(prefixName).append(" | ").append(uri).append(" |\n");
        }
        return sb.toString();
    }
    
    public String generatePrefixSourceBlocks() {
        return "";
    }
}
