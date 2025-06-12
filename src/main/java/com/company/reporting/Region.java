package com.company.reporting;

/**
 * Enumerates the geographical regions used for reporting.
 * Each region has an associated integer code. Includes methods for converting
 * from and to these codes.
 */
public enum Region {
    /** North region, code 1. */
    NORTH(1),
    /** South region, code 2. */
    SOUTH(2),
    /** East region, code 3. */
    EAST(3),
    /** West region, code 4. */
    WEST(4),
    /** Represents an invalid or unrecognized region code. */
    INVALID(0);

    private final int code;

    /**
     * Constructor for the enum.
     * @param code The integer code associated with the region.
     */
    Region(int code) {
        this.code = code;
    }

    /**
     * Gets the integer code for this region.
     * @return The integer code.
     */
    public int getCode() {
        return code;
    }

    /**
     * Converts an integer code to its corresponding Region enum constant.
     * If the code is not recognized, {@link Region#INVALID} is returned.
     *
     * @param code The integer code to convert.
     * @return The matching Region, or {@link Region#INVALID} if no match is found.
     */
    public static Region fromCode(int code) {
        for (Region region : values()) {
            if (region.code == code) {
                return region;
            }
        }
        return INVALID;
    }

    /**
     * Converts a string representation of a region code to its corresponding Region enum constant.
     * The input string is trimmed, and if it's null, empty, or not a valid integer representation
     * of a known region code, {@link Region#INVALID} is returned.
     *
     * @param codeStr The string representation of the region code.
     * @return The matching Region, or {@link Region#INVALID} if the string is invalid or no match is found.
     */
    public static Region fromCodeString(String codeStr) {
        if (codeStr == null || codeStr.trim().isEmpty()) {
            return INVALID;
        }
        try {
            int code = Integer.parseInt(codeStr.trim());
            return fromCode(code);
        } catch (NumberFormatException e) {
            return INVALID;
        }
    }
}
