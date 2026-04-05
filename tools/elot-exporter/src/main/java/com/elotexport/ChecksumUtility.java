/*
 * Copyright (C) 2024-2025 Johan W. Klüwer <johan.w.kluwer@gmail.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 */

package com.elotexport;

import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.HexFormat;
import java.util.List;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

public final class ChecksumUtility {

    private static final String CHECKSUM_PREFIX = "#+ELOT_EXPORTER_CHECKSUM: ";
    private static final Pattern CHECKSUM_PATTERN = Pattern.compile("^" + Pattern.quote(CHECKSUM_PREFIX) + "([a-fA-F0-9]+)$");

    private ChecksumUtility() {}

    /**
     * Calculates the SHA-256 checksum of the content, excluding any lines that match the checksum format.
     */
    public static String calculateChecksum(String content) {
        String dataToHash = filterChecksumLines(content);
        try {
            MessageDigest digest = MessageDigest.getInstance("SHA-256");
            byte[] hash = digest.digest(dataToHash.getBytes(StandardCharsets.UTF_8));
            return HexFormat.of().formatHex(hash);
        } catch (NoSuchAlgorithmException e) {
            throw new RuntimeException("SHA-256 algorithm not found", e);
        }
    }

    /**
     * Extracts the checksum from the given content, if present.
     */
    public static String extractChecksum(String content) {
        return content.lines()
                .map(line -> CHECKSUM_PATTERN.matcher(line))
                .filter(matcher -> matcher.matches())
                .map(matcher -> matcher.group(1))
                .findFirst()
                .orElse(null);
    }

    /**
     * Returns the content without the checksum line(s).
     * It tries to preserve other line endings and whitespace as much as possible,
     * but since elot-exporter normalizes to \n, we assume \n.
     */
    public static String filterChecksumLines(String content) {
        StringBuilder sb = new StringBuilder();
        String[] lines = content.split("\\R", -1); // Split preserving trailing empty strings
        for (String line : lines) {
            if (!CHECKSUM_PATTERN.matcher(line).matches()) {
                if (sb.length() > 0) {
                    sb.append("\n");
                }
                sb.append(line);
            }
        }
        return sb.toString();
    }

    /**
     * Formats a checksum line.
     */
    public static String formatChecksumLine(String checksum) {
        return CHECKSUM_PREFIX + checksum;
    }

    /**
     * Verifies if the file's content (minus the checksum line) matches its stored checksum.
     * Returns true if the file is "pristine" (not modified by user).
     * Returns false if the file is "dirty" or doesn't have a checksum.
     */
    public static boolean isPristine(String existingContent) {
        String storedChecksum = extractChecksum(existingContent);
        if (storedChecksum == null) {
            return false; // No checksum, assume dirty or unknown
        }
        String currentChecksum = calculateChecksum(existingContent);
        return storedChecksum.equalsIgnoreCase(currentChecksum);
    }
}
