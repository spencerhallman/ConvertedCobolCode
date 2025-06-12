package com.company.reporting;

import java.time.LocalDate;

/**
 * Represents a line item in the Employee Compensation Report.
 * This class holds all necessary data for an employee to be displayed in that report,
 * including identifying information and various pay components.
 * Fields are public for direct access by the {@link ReportGenerator}.
 */
public class CompensationData {
    /** The name of the employee. */
    public String employeeName;
    /** The string representation of the employee's region (e.g., "NORTH"). */
    public String employeeRegion;
    /** The string representation of the employee's type (e.g., "HOURLY"). */
    public String employeeType;
    /** The date the employee was hired. */
    public LocalDate hireDate;
    /** The calculated years of service for the employee. */
    public int yearsOfService;
    /**
     * The base wage or salary component.
     * For Hourly employees, this is their regular (non-overtime) pay.
     * For Sales employees, this is their base salary.
     */
    public double wages;
    /** Overtime pay, primarily applicable to Hourly employees. */
    public double overtime;
    /** Commission earned, primarily applicable to Sales employees. */
    public double commission;
    /** The total compensation for the employee (sum of wages, overtime, and commission as applicable). */
    public double totalCompensation;

    /**
     * Constructs a CompensationData object.
     * All monetary values are assumed to be pre-calculated and rounded.
     *
     * @param employeeName Name of the employee.
     * @param employeeRegion String representation of the employee's region.
     * @param employeeType String representation of the employee's type.
     * @param hireDate Employee's hire date.
     * @param yearsOfService Calculated years of service.
     * @param wages Base wages or salary.
     * @param overtime Overtime pay.
     * @param commission Commission earned.
     * @param totalCompensation Total compensation.
     */
    public CompensationData(String employeeName, String employeeRegion, String employeeType,
                            LocalDate hireDate, int yearsOfService, double wages,
                            double overtime, double commission, double totalCompensation) {
        this.employeeName = employeeName;
        this.employeeRegion = employeeRegion;
        this.employeeType = employeeType;
        this.hireDate = hireDate;
        this.yearsOfService = yearsOfService;
        this.wages = wages;
        this.overtime = overtime;
        this.commission = commission;
        this.totalCompensation = totalCompensation;
    }
    // Getters could be added if needed, but public fields are used for simplicity.
}
