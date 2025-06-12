package com.company.reporting;

/**
 * Enumerates the types of employees within the company.
 * Each type may have different compensation structures and roles.
 */
public enum EmployeeType {
    /** Employee paid on an hourly basis. Eligible for overtime. */
    HOURLY,
    /** Employee working in sales, typically compensated with a base salary plus commission. */
    SALES,
    /** Employee in a management role, potentially eligible for management commission based on regional sales. */
    MANAGEMENT,
    /** Represents an unknown or invalid employee type, often due to parsing errors or unrecognized codes. */
    UNKNOWN
}
