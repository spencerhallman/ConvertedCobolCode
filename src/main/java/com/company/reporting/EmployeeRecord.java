package com.company.reporting;

/**
 * Represents a raw, unprocessed record of an employee as read directly from the input file.
 * This class holds the string values for each field in the fixed-width employee data file.
 * These raw string fields are then parsed and processed to populate an {@link Employee} object.
 * Fields are public for direct access during the parsing stage.
 */
public class EmployeeRecord {
    /** Raw string representation of the employee's name (up to 15 characters). */
    public String rawEmpName;
    /** Raw string representation of the employee's hire date (expected format YYYYMMDD, 8 characters). */
    public String rawHireDate;
    /** Raw string representation of the employee's type (1 character, e.g., 'H', 'S', 'M'). */
    public String rawEmpType;
    /** Raw string representation of the employee's region code (5 characters). */
    public String rawEmpRegionCode;
    /**
     * Raw string representation of salary or base pay (8 characters, format NNNNN.NN).
     * For Hourly employees, this field might be unused or zero.
     * For Sales employees, this is their base salary.
     * For Management employees, this is their management salary.
     */
    public String rawSalaryOrBasePay;
    /**
     * Raw string representation of hours worked or sales amount (8 characters, format NNNNN.NN).
     * For Hourly employees, this represents hours worked.
     * For Sales employees, this represents their sales amount for commission calculation.
     * For Management employees, this field might be unused or zero.
     */
    public String rawHoursOrSalesAmount;
    /**
     * Raw string representation of the hourly rate (8 characters, format NNNNN.NN, though effectively 7 due to file parsing adjustment).
     * Primarily used for Hourly employees.
     * For Sales and Management employees, this field might be unused or zero.
     */
    public String rawRate;

    /**
     * Default constructor. Fields are populated by the {@link EmployeeFileParser}.
     */
    public EmployeeRecord() {
        // Fields are set directly by the parser.
    }
}
