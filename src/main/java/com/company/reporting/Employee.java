package com.company.reporting;

import java.time.LocalDate;

/**
 * Represents an employee with processed and calculated data.
 * This class holds information after raw {@link EmployeeRecord} data has been parsed
 * and combined with calculated values like years of service and compensation components.
 * Fields are generally public for direct access within the reporting logic, simplifying
 * data transfer.
 */
public class Employee {
    /** Employee's full name. */
    public String name;
    /** Date the employee was hired. */
    public LocalDate hireDate;
    /** Type of the employee (e.g., HOURLY, SALES, MANAGEMENT). */
    public EmployeeType type;
    /** Region where the employee works. */
    public Region region;
    /** Calculated years of service as of the run date. */
    public int yearsOfService;

    // Fields specific to employee types
    /** Hours worked by an hourly employee. */
    public double hoursWorked;
    /** Hourly rate for an hourly employee. */
    public double hourlyRate;

    /** Total sales amount for a sales employee. */
    public double salesAmount;
    /** Base salary for a sales employee. */
    public double salesSalary;

    /** Salary for a management employee. */
    public double managementSalary;

    // Calculated fields (populated during processing)
    /** Regular pay component (e.g., base hourly pay, sales salary). */
    public double regularPay;
    /** Overtime pay component, primarily for hourly employees. */
    public double overtimePay;
    /** Commission earned, primarily for sales and management employees. */
    public double commission;
    /** Total compensation for the employee (sum of applicable pay components). */
    public double totalCompensation;

    /**
     * Constructs an Employee object with essential details.
     * Other fields related to pay and specific types are populated during processing.
     *
     * @param name The employee's name.
     * @param hireDate The employee's hire date.
     * @param type The employee's type (HOURLY, SALES, MANAGEMENT).
     * @param region The employee's region.
     */
    public Employee(String name, LocalDate hireDate, EmployeeType type, Region region) {
        this.name = name;
        this.hireDate = hireDate;
        this.type = type;
        this.region = region;
        // yearsOfService and other pay fields are calculated and set later.
    }
    // Getters and setters could be added if more complex logic or encapsulation is needed later.
}
