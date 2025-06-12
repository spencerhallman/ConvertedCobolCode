package com.company.reporting;

/**
 * Represents a line item or summary for a specific region in the Regional Sales Report.
 * This class holds data related to a region's sales performance, its manager's details,
 * and manager's compensation.
 * Fields are public for direct access by the {@link ReportGenerator}.
 */
public class RegionalSalesData {
    /** The name of the region (e.g., "NORTH"). */
    public String regionName;
    /** The name of the manager for this region. Can be null if no manager is assigned or found. */
    public String managerName;
    /** The base salary of the manager. */
    public double managerSalary;
    /** The total sales amount for the region. */
    public double totalRegionalSales;
    /** Commission earned by the manager based on total regional sales (calculated if End of Month). */
    public double managerCommission;
    /** Total compensation for the manager (salary + commission). */
    public double managerTotalCompensation;
    /**
     * A comment field, typically used to indicate special conditions.
     * For example, "UH-OH" if a sales employee in the region had zero sales.
     */
    public String comment;

    /**
     * Constructs a RegionalSalesData object for a given region name.
     * Initializes numeric fields to 0.0 and comment to an empty string.
     *
     * @param regionName The name of the region.
     */
    public RegionalSalesData(String regionName) {
        this.regionName = regionName;
        this.managerName = null; // Explicitly null, though default for String
        this.managerSalary = 0.0;
        this.totalRegionalSales = 0.0;
        this.managerCommission = 0.0;
        this.managerTotalCompensation = 0.0;
        this.comment = "";
    }
    // Getters and setters could be added if needed.
}
