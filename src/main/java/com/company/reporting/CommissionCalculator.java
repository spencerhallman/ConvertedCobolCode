package com.company.reporting;

import java.math.BigDecimal;
import java.math.RoundingMode;

/**
 * Calculates commissions for employees based on their type (Sales or Management)
 * and sales amount. The commission rates and tiers are derived from the
 * CWXTSUBC COBOL program logic.
 */
public class CommissionCalculator {

    /**
     * Calculates the commission for an employee.
     * Uses BigDecimal for precise currency calculations and rounds the final commission
     * to two decimal places using HALF_UP rounding.
     * <p>
     * Sales commission rates (based on CWXTSUBC.md - SALES-RATE-TABLE):
     * <ul>
     *   <li>1.00 - 20000.00: 2%</li>
     *   <li>20000.01 - 40000.00: 4%</li>
     *   <li>40000.01 - 60000.00: 6%</li>
     *   <li>60000.01 - 80000.00: 8%</li>
     *   <li>80000.01 and above: 10%</li>
     * </ul>
     * Management commission rates (based on CWXTSUBC.md - MGMT-RATE-TABLE):
     * <ul>
     *   <li>1.00 - 100000.00: 2.0%</li>
     *   <li>100000.01 - 200000.00: 2.5%</li>
     *   <li>200000.01 - 300000.00: 3.0%</li>
     *   <li>300000.01 - 400000.00: 3.5%</li>
     *   <li>400000.01 and above: 4.0%</li>
     * </ul>
     *
     * @param employeeType The type of the employee (SALES, MANAGEMENT, etc.).
     *                     Only SALES and MANAGEMENT types are eligible for commission.
     * @param salesAmount  The total sales amount based on which commission is calculated.
     *                     For management, this is total regional sales.
     * @return The calculated commission as a double, rounded to two decimal places.
     *         Returns 0.0 if the employee type is not eligible for commission or if salesAmount is zero or negative.
     */
    public double calculateCommission(EmployeeType employeeType, double salesAmount) {
        BigDecimal salesBd = BigDecimal.valueOf(salesAmount);
        BigDecimal commissionRate;

        if (salesAmount <= 0) { // No commission for zero or negative sales
            return 0.0;
        }

        if (employeeType == EmployeeType.SALES) {
            if (salesBd.compareTo(new BigDecimal("20000.00")) <= 0) {
                commissionRate = new BigDecimal("0.02");
            } else if (salesBd.compareTo(new BigDecimal("40000.00")) <= 0) {
                commissionRate = new BigDecimal("0.04");
            } else if (salesBd.compareTo(new BigDecimal("60000.00")) <= 0) {
                commissionRate = new BigDecimal("0.06");
            } else if (salesBd.compareTo(new BigDecimal("80000.00")) <= 0) {
                commissionRate = new BigDecimal("0.08");
            } else { // 80000.01 and above
                commissionRate = new BigDecimal("0.10");
            }
        } else if (employeeType == EmployeeType.MANAGEMENT) {
            if (salesBd.compareTo(new BigDecimal("100000.00")) <= 0) {
                commissionRate = new BigDecimal("0.020");
            } else if (salesBd.compareTo(new BigDecimal("200000.00")) <= 0) {
                commissionRate = new BigDecimal("0.025");
            } else if (salesBd.compareTo(new BigDecimal("300000.00")) <= 0) {
                commissionRate = new BigDecimal("0.030");
            } else if (salesBd.compareTo(new BigDecimal("400000.00")) <= 0) {
                commissionRate = new BigDecimal("0.035");
            } else { // 400000.01 and above
                commissionRate = new BigDecimal("0.040");
            }
        } else {
            // Not a sales or management employee, or unknown type
            return 0.0;
        }

        BigDecimal commission = salesBd.multiply(commissionRate);
        // COMM-TOTAL (PIC 9(5)V99 COMP-3) implies rounding or truncation.
        // Standard practice is to round to 2 decimal places for currency.
        return commission.setScale(2, RoundingMode.HALF_UP).doubleValue();
    }
}
