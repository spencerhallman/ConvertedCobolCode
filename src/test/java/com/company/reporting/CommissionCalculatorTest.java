package com.company.reporting;

import org.junit.Test;
import static org.junit.Assert.*;

public class CommissionCalculatorTest {

    private final CommissionCalculator calculator = new CommissionCalculator();
    private static final double DELTA = 0.001; // For double comparisons

    // --- Tests for Sales Commission ---
    @Test
    public void testSalesCommission_Tier1_Low() {
        assertEquals(200.00, calculator.calculateCommission(EmployeeType.SALES, 10000.00), DELTA);
    }

    @Test
    public void testSalesCommission_Tier1_Boundary() {
        assertEquals(400.00, calculator.calculateCommission(EmployeeType.SALES, 20000.00), DELTA);
    }

    @Test
    public void testSalesCommission_Tier2_Low() {
        assertEquals(800.04, calculator.calculateCommission(EmployeeType.SALES, 20001.00), DELTA);
    }

    @Test
    public void testSalesCommission_Tier2_Boundary() {
        assertEquals(1600.00, calculator.calculateCommission(EmployeeType.SALES, 40000.00), DELTA);
    }

    @Test
    public void testSalesCommission_Tier3_Low() {
        assertEquals(2400.06, calculator.calculateCommission(EmployeeType.SALES, 40001.00), DELTA);
    }

    @Test
    public void testSalesCommission_Tier3_Boundary() {
        assertEquals(3600.00, calculator.calculateCommission(EmployeeType.SALES, 60000.00), DELTA);
    }

    @Test
    public void testSalesCommission_Tier4_Low() {
        assertEquals(4800.08, calculator.calculateCommission(EmployeeType.SALES, 60001.00), DELTA);
    }

    @Test
    public void testSalesCommission_Tier4_Boundary() {
        assertEquals(6400.00, calculator.calculateCommission(EmployeeType.SALES, 80000.00), DELTA);
    }

    @Test
    public void testSalesCommission_Tier5_Low() {
        assertEquals(8000.10, calculator.calculateCommission(EmployeeType.SALES, 80001.00), DELTA);
    }

    @Test
    public void testSalesCommission_Tier5_High() {
        assertEquals(12000.00, calculator.calculateCommission(EmployeeType.SALES, 120000.00), DELTA);
    }

    @Test
    public void testSalesCommission_ZeroSales() {
        assertEquals(0.00, calculator.calculateCommission(EmployeeType.SALES, 0.00), DELTA);
    }

    @Test
    public void testSalesCommission_NegativeSales() {
        assertEquals(0.00, calculator.calculateCommission(EmployeeType.SALES, -100.00), DELTA);
    }


    // --- Tests for Management Commission ---
    @Test
    public void testManagementCommission_Tier1_Low() {
        assertEquals(1000.00, calculator.calculateCommission(EmployeeType.MANAGEMENT, 50000.00), DELTA);
    }

    @Test
    public void testManagementCommission_Tier1_Boundary() {
        assertEquals(2000.00, calculator.calculateCommission(EmployeeType.MANAGEMENT, 100000.00), DELTA);
    }

    @Test
    public void testManagementCommission_Tier2_Low() {
        // 100000.01 * 0.025 = 2500.00025 -> 2500.00
        assertEquals(2500.00, calculator.calculateCommission(EmployeeType.MANAGEMENT, 100000.01), DELTA);
    }

    @Test
    public void testManagementCommission_Tier2_Boundary() {
        assertEquals(5000.00, calculator.calculateCommission(EmployeeType.MANAGEMENT, 200000.00), DELTA);
    }

    @Test
    public void testManagementCommission_Tier3_Low() {
        // 200000.01 * 0.030 = 6000.0003 -> 6000.00
        assertEquals(6000.00, calculator.calculateCommission(EmployeeType.MANAGEMENT, 200000.01), DELTA);
    }

    @Test
    public void testManagementCommission_Tier3_Boundary() {
        assertEquals(9000.00, calculator.calculateCommission(EmployeeType.MANAGEMENT, 300000.00), DELTA);
    }

    @Test
    public void testManagementCommission_Tier4_Low() {
        // 300000.01 * 0.035 = 10500.00035 -> 10500.00
        assertEquals(10500.00, calculator.calculateCommission(EmployeeType.MANAGEMENT, 300000.01), DELTA);
    }

    @Test
    public void testManagementCommission_Tier4_Boundary() {
        assertEquals(14000.00, calculator.calculateCommission(EmployeeType.MANAGEMENT, 400000.00), DELTA);
    }

    @Test
    public void testManagementCommission_Tier5_Low() {
        // 400000.01 * 0.040 = 16000.0004 -> 16000.00
        assertEquals(16000.00, calculator.calculateCommission(EmployeeType.MANAGEMENT, 400000.01), DELTA);
    }

    @Test
    public void testManagementCommission_Tier5_High() {
        assertEquals(24000.00, calculator.calculateCommission(EmployeeType.MANAGEMENT, 600000.00), DELTA);
    }

    @Test
    public void testManagementCommission_ZeroSales() {
        assertEquals(0.00, calculator.calculateCommission(EmployeeType.MANAGEMENT, 0.00), DELTA);
    }


    // --- Test for Non-Commissionable Employee Type ---
    @Test
    public void testNonCommissionableType() {
        assertEquals(0.00, calculator.calculateCommission(EmployeeType.HOURLY, 50000.00), DELTA);
        assertEquals(0.00, calculator.calculateCommission(EmployeeType.UNKNOWN, 50000.00), DELTA);
    }
}
