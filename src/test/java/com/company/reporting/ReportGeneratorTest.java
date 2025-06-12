package com.company.reporting;

import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.io.IOException;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static org.junit.Assert.*;
import static org.mockito.Mockito.*;

public class ReportGeneratorTest {

    @Mock private EmployeeFileParser mockParser;
    @Mock private DateCalculator mockDateCalculator;
    @Mock private CommissionCalculator mockCommissionCalculator;

    private ReportGenerator reportGenerator;
    private final LocalDate testRunDate = LocalDate.of(2023, 10, 31); // EOM
    private final LocalDate testRunDateNotEOM = LocalDate.of(2023, 10, 30); // Not EOM

    @Before
    public void setUp() {
        MockitoAnnotations.initMocks(this);
    }

    private List<EmployeeRecord> createSampleRawRecords() {
        List<EmployeeRecord> records = new ArrayList<>();
        // Name(15), HireDate(8), Type(1), RegionCode(5), Salary/Base(8), Hours/Sales(8), Rate(8)
        EmployeeRecord r1 = new EmployeeRecord(); // Hourly
        r1.rawEmpName = "Hourly Emp1"; r1.rawHireDate = "20200101"; r1.rawEmpType = "H"; r1.rawEmpRegionCode = "1";
        r1.rawSalaryOrBasePay = "00000.00"; r1.rawHoursOrSalesAmount = "0040.00"; r1.rawRate = "0010.00";
        records.add(r1);

        EmployeeRecord r2 = new EmployeeRecord(); // Sales Emp, has sales
        r2.rawEmpName = "Sales Emp1"; r2.rawHireDate = "20190315"; r2.rawEmpType = "S"; r2.rawEmpRegionCode = "2";
        r2.rawSalaryOrBasePay = "02000.00"; r2.rawHoursOrSalesAmount = "10000.00"; r2.rawRate = "0000.00";
        records.add(r2);

        EmployeeRecord r3 = new EmployeeRecord(); // Management Emp
        r3.rawEmpName = "Mgr Emp1"; r3.rawHireDate = "20150620"; r3.rawEmpType = "M"; r3.rawEmpRegionCode = "1";
        r3.rawSalaryOrBasePay = "05000.00"; r3.rawHoursOrSalesAmount = "00000.00"; r3.rawRate = "0000.00";
        records.add(r3);

        EmployeeRecord r4 = new EmployeeRecord(); // Hourly Emp, Overtime
        r4.rawEmpName = "Hourly OT"; r4.rawHireDate = "20210210"; r4.rawEmpType = "H"; r4.rawEmpRegionCode = "1";
        r4.rawSalaryOrBasePay = "00000.00"; r4.rawHoursOrSalesAmount = "0050.00"; r4.rawRate = "0020.00";
        records.add(r4);

        EmployeeRecord r5 = new EmployeeRecord(); // Sales Emp, ZERO sales -> UH-OH
        r5.rawEmpName = "Sales Zero"; r5.rawHireDate = "20220101"; r5.rawEmpType = "S"; r5.rawEmpRegionCode = "1";
        r5.rawSalaryOrBasePay = "01500.00"; r5.rawHoursOrSalesAmount = "00000.00"; r5.rawRate = "0000.00";
        records.add(r5);

        EmployeeRecord r6 = new EmployeeRecord(); // Invalid type
        r6.rawEmpName = "InvalidTypeEmp"; r6.rawHireDate = "20200101"; r6.rawEmpType = "X"; r6.rawEmpRegionCode = "3";
        records.add(r6);

        EmployeeRecord r7 = new EmployeeRecord(); // Invalid region for storage
        r7.rawEmpName = "InvalidRegionEmp"; r7.rawHireDate = "20200101"; r7.rawEmpType = "H"; r7.rawEmpRegionCode = "9";
        r7.rawSalaryOrBasePay = "00000.00"; r7.rawHoursOrSalesAmount = "0040.00"; r7.rawRate = "0010.00";
        records.add(r7);

        return records;
    }

    @Test
    public void testProcess_SuccessfulRun_EOM() throws IOException {
        reportGenerator = new ReportGenerator(mockParser, mockDateCalculator, mockCommissionCalculator, testRunDate);
        List<EmployeeRecord> sampleRecords = createSampleRawRecords();
        when(mockParser.parseFile(anyString())).thenReturn(sampleRecords);
        when(mockDateCalculator.isEndOfMonth(testRunDate)).thenReturn(true);
        when(mockDateCalculator.calculateYearsOfService(any(LocalDate.class), eq(testRunDate))).thenReturn(3); // Example YOS

        // Mock commission calculations
        // Sales Emp1 (10000 sales) -> 10000 * 0.02 = 200
        when(mockCommissionCalculator.calculateCommission(EmployeeType.SALES, 10000.00)).thenReturn(200.00);
        // Regional Sales for Region NORTH (Mgr Emp1 is in North, Sales Zero is in North)
        // Sales Zero has 0 sales. So total sales for North for Mgmt commission is effectively 0.
        // Mgr Emp1 is manager for North.
        // Regional Sales for Region SOUTH (Sales Emp1 is in South) -> 10000
        when(mockCommissionCalculator.calculateCommission(EmployeeType.MANAGEMENT, 10000.00)).thenReturn(200.00); // For region SOUTH
        when(mockCommissionCalculator.calculateCommission(EmployeeType.MANAGEMENT, 0.00)).thenReturn(0.00);    // For region NORTH


        reportGenerator.process("dummy.dat", "");

        Map<Region, List<CompensationData>> compData = reportGenerator.getEmployeeCompensationData();
        //assertEquals(2, compData.get(Region.NORTH).size()); // Hourly Emp1, Hourly OT (Sales Zero not stored if basepay is 0 and comm is 0?)
                                                            // Sales Zero: salary 1500, comm 0. Total 1500. Stored.
        assertEquals(3, compData.get(Region.NORTH).size()); // Hourly Emp1, Hourly OT, Sales Zero
        assertEquals(1, compData.get(Region.SOUTH).size()); // Sales Emp1

        CompensationData hourlyEmp1Comp = compData.get(Region.NORTH).get(0);
        assertEquals("Hourly Emp1", hourlyEmp1Comp.employeeName);
        assertEquals(400.00, hourlyEmp1Comp.totalCompensation, 0.001); // 40*10

        CompensationData hourlyOTComp = compData.get(Region.NORTH).get(1);
        assertEquals("Hourly OT", hourlyOTComp.employeeName); // 40*20 + 10*20*1.5 = 800 + 300 = 1100
        assertEquals(1100.00, hourlyOTComp.totalCompensation, 0.001);

        CompensationData salesZeroComp = compData.get(Region.NORTH).get(2);
        assertEquals("Sales Zero", salesZeroComp.employeeName);
        assertEquals(1500.00, salesZeroComp.totalCompensation, 0.001); // Salary 1500, Comm 0


        Map<Region, RegionalSalesData> regionalData = reportGenerator.getRegionalSalesDataMap();
        assertTrue(reportGenerator.isEndOfMonthDetermined());
        assertEquals("Mgr Emp1", regionalData.get(Region.NORTH).managerName);
        assertEquals(5000.00, regionalData.get(Region.NORTH).managerSalary, 0.001);
        assertEquals(0.00, regionalData.get(Region.NORTH).totalRegionalSales, 0.001); // Sales Zero had 0 sales
        assertEquals("UH-OH", regionalData.get(Region.NORTH).comment); // From Sales Zero
        assertEquals(0.00, regionalData.get(Region.NORTH).managerCommission, 0.001); // Comm on 0 sales is 0
        assertEquals(5000.00, regionalData.get(Region.NORTH).managerTotalCompensation, 0.001);


        assertNull(regionalData.get(Region.SOUTH).managerName); // No manager for South in sample
        assertEquals(10000.00, regionalData.get(Region.SOUTH).totalRegionalSales, 0.001);
        assertEquals(200.00, regionalData.get(Region.SOUTH).managerCommission, 0.001); // Comm on 10k sales
        assertEquals(200.00, regionalData.get(Region.SOUTH).managerTotalCompensation, 0.001); // Salary 0 + Comm 200

        // Verify "InvalidTypeEmp" (X) and "InvalidRegionEmp" (H, region 9) were skipped or handled
        // InvalidTypeEmp: not stored in compData
        // InvalidRegionEmp: not stored in compData for region 9
        assertTrue(compData.get(Region.EAST).isEmpty()); // No valid employees for East
        assertTrue(compData.get(Region.WEST).isEmpty()); // No valid employees for West

        verify(mockParser).parseFile("dummy.dat");
        verify(mockDateCalculator, times(sampleRecords.size() - 1)).calculateYearsOfService(any(LocalDate.class), eq(testRunDate)); // -1 for invalid type
    }

    @Test
    public void testProcess_SkipRecordsWithParmData() throws IOException {
        reportGenerator = new ReportGenerator(mockParser, mockDateCalculator, mockCommissionCalculator, testRunDate);
        List<EmployeeRecord> sampleRecords = createSampleRawRecords(); // 7 records
        when(mockParser.parseFile(anyString())).thenReturn(sampleRecords);
        when(mockDateCalculator.isEndOfMonth(testRunDate)).thenReturn(false); // EOM not important here

        reportGenerator.process("dummy.dat", "00003"); // Start from record 3

        // calculateYearsOfService will be called for records 3, 4, 5 (valid types)
        // Record 6 (idx 5) is invalid type 'X', record 7 (idx 6) is invalid region
        // So, for R3, R4, R5, R7 (valid type H)
        verify(mockDateCalculator, times(4)).calculateYearsOfService(any(LocalDate.class), eq(testRunDate));

        Map<Region, List<CompensationData>> compData = reportGenerator.getEmployeeCompensationData();
        // R1 (Hourly, N) and R2 (Sales, S) should be skipped.
        // R3 (Mgr, N) - not in comp report
        // R4 (Hourly OT, N) - stored
        // R5 (Sales Zero, N) - stored
        // R7 (InvalidRegionEmp H) - not stored
        assertEquals(2, compData.get(Region.NORTH).size());
        assertEquals("Hourly OT", compData.get(Region.NORTH).get(0).employeeName);
    }

    @Test
    public void testProcess_BadParmData_SkipsProcessing() throws IOException {
         reportGenerator = new ReportGenerator(mockParser, mockDateCalculator, mockCommissionCalculator, testRunDate);
         reportGenerator.process("dummy.dat", "ABCDE"); // Invalid PARM-DATA

         verify(mockParser, never()).parseFile(anyString());
         Map<Region, List<CompensationData>> compData = reportGenerator.getEmployeeCompensationData();
         assertTrue(compData.get(Region.NORTH).isEmpty()); // No data processed
    }

    @Test
    public void testProcess_NotEOM_NoManagementCommissions() throws IOException {
        reportGenerator = new ReportGenerator(mockParser, mockDateCalculator, mockCommissionCalculator, testRunDateNotEOM);
        List<EmployeeRecord> records = new ArrayList<>();
        EmployeeRecord r1 = new EmployeeRecord(); // Management Emp
        r1.rawEmpName = "Mgr Only"; r1.rawHireDate = "20150620"; r1.rawEmpType = "M"; r1.rawEmpRegionCode = "1";
        r1.rawSalaryOrBasePay = "06000.00"; r1.rawHoursOrSalesAmount = "00000.00"; r1.rawRate = "0000.00";
        records.add(r1);

        EmployeeRecord r2 = new EmployeeRecord(); // Sales Emp for regional sales
        r2.rawEmpName = "Sales Only"; r2.rawHireDate = "20190315"; r2.rawEmpType = "S"; r2.rawEmpRegionCode = "1";
        r2.rawSalaryOrBasePay = "02000.00"; r2.rawHoursOrSalesAmount = "20000.00"; r2.rawRate = "0000.00";
        records.add(r2);


        when(mockParser.parseFile(anyString())).thenReturn(records);
        when(mockDateCalculator.isEndOfMonth(testRunDateNotEOM)).thenReturn(false);
        when(mockDateCalculator.calculateYearsOfService(any(LocalDate.class), eq(testRunDateNotEOM))).thenReturn(1);
        when(mockCommissionCalculator.calculateCommission(EmployeeType.SALES, 20000.00)).thenReturn(400.00);


        reportGenerator.process("dummy.dat", "");
        assertFalse(reportGenerator.isEndOfMonthDetermined());

        Map<Region, RegionalSalesData> regionalData = reportGenerator.getRegionalSalesDataMap();
        assertEquals("Mgr Only", regionalData.get(Region.NORTH).managerName);
        assertEquals(6000.00, regionalData.get(Region.NORTH).managerSalary, 0.001);
        assertEquals(20000.00, regionalData.get(Region.NORTH).totalRegionalSales, 0.001);
        // Management commission should NOT be calculated
        assertEquals(0.00, regionalData.get(Region.NORTH).managerCommission, 0.001);
        assertEquals(6000.00, regionalData.get(Region.NORTH).managerTotalCompensation, 0.001); // Salary only

        verify(mockCommissionCalculator, never()).calculateCommission(eq(EmployeeType.MANAGEMENT), anyDouble());
    }

    @Test
    public void testGenerateEmployeeCompensationReport_OutputFormat() throws IOException {
        reportGenerator = new ReportGenerator(mockParser, mockDateCalculator, mockCommissionCalculator, testRunDate); // EOM
        List<EmployeeRecord> sampleRecords = createSampleRawRecords(); // Use existing helper
        when(mockParser.parseFile(anyString())).thenReturn(sampleRecords);
        when(mockDateCalculator.isEndOfMonth(testRunDate)).thenReturn(true);
        when(mockDateCalculator.calculateYearsOfService(any(LocalDate.class), eq(testRunDate))).thenReturn(3); // Example YOS

        // Mock commissions for sales and management as needed for data consistency
        when(mockCommissionCalculator.calculateCommission(EmployeeType.SALES, 10000.00)).thenReturn(200.00); // For Sales Emp1
        when(mockCommissionCalculator.calculateCommission(EmployeeType.MANAGEMENT, 10000.00)).thenReturn(200.00); // For Region SOUTH total sales
        when(mockCommissionCalculator.calculateCommission(EmployeeType.MANAGEMENT, 0.00)).thenReturn(0.00);    // For Region NORTH total sales


        reportGenerator.process("dummy.dat", "");
        String report = reportGenerator.generateEmployeeCompensationReport();

        assertTrue(report.contains("EMPLOYEE COMPENSATION REPORT"));
        assertTrue(report.contains("RUN DATE: " + testRunDate.toString()));
        assertTrue(report.contains("REGION: NORTH"));
        // Check for one of the employees from createSampleRawRecords() - e.g., Hourly Emp1
        // Hourly Emp1: 40hrs * 10/hr = 400. Name: "Hourly Emp1", Date: 2020-01-01, YOS: 3, Type: HOURLY
        assertTrue(report.contains(String.format("%-18s %-10s %8d %-10s %15s %12s %15s %15s",
                "Hourly Emp1", "2020-01-01", 3, "HOURLY", "400.00", "0.00", "0.00", "400.00")));

        // Sales Emp1: Salary 2000, Sales 10000, Comm 200. Total 2200. Name: "Sales Emp1", Date: 2019-03-15, YOS: 3, Type: SALES
         assertTrue(report.contains(String.format("%-18s %-10s %8d %-10s %15s %12s %15s %15s",
                "Sales Emp1", "2019-03-15", 3, "SALES", "2,000.00", "0.00", "200.00", "2,200.00")));

        assertTrue(report.contains("REGION: SOUTH"));
    }

    @Test
    public void testGenerateRegionalSalesReport_EOM_OutputFormat() throws IOException {
        reportGenerator = new ReportGenerator(mockParser, mockDateCalculator, mockCommissionCalculator, testRunDate); // EOM
        List<EmployeeRecord> sampleRecords = createSampleRawRecords();
        when(mockParser.parseFile(anyString())).thenReturn(sampleRecords);
        when(mockDateCalculator.isEndOfMonth(testRunDate)).thenReturn(true);
        when(mockDateCalculator.calculateYearsOfService(any(LocalDate.class), eq(testRunDate))).thenReturn(3);

        when(mockCommissionCalculator.calculateCommission(EmployeeType.SALES, 10000.00)).thenReturn(200.00); // For Sales Emp1
        // Region NORTH: Mgr Emp1 (salary 5000), Sales Zero (0 sales) -> total sales 0. Mgr Comm 0.
        when(mockCommissionCalculator.calculateCommission(EmployeeType.MANAGEMENT, 0.00)).thenReturn(0.00);
        // Region SOUTH: No manager, Sales Emp1 (sales 10000) -> total sales 10000. Mgr Comm 200.
        when(mockCommissionCalculator.calculateCommission(EmployeeType.MANAGEMENT, 10000.00)).thenReturn(200.00);

        reportGenerator.process("dummy.dat", "");
        String report = reportGenerator.generateRegionalSalesReport();

        assertTrue(report.contains("REGIONAL SALES REPORT"));
        assertTrue(report.contains("RUN DATE: " + testRunDate.toString()));
        assertTrue(report.contains("REGION: NORTH"));
        assertTrue(report.contains(String.format("  MANAGER:          %s", "Mgr Emp1")));
        assertTrue(report.contains(String.format("  MANAGER SALARY:   %15s", "5,000.00")));
        assertTrue(report.contains(String.format("  TOTAL SALES:      %15s", "0.00")));
        assertTrue(report.contains(String.format("  MANAGER COMM:     %15s", "0.00")));
        assertTrue(report.contains(String.format("  MANAGER TOTAL:    %15s", "5,000.00")));
        assertTrue(report.contains(String.format("  COMMENT:          %s", "UH-OH"))); // From Sales Zero in North

        assertTrue(report.contains("REGION: SOUTH"));
        assertTrue(report.contains(String.format("  MANAGER:          %s", "N/A")));
        assertTrue(report.contains(String.format("  TOTAL SALES:      %15s", "10,000.00")));
         assertTrue(report.contains(String.format("  MANAGER COMM:     %15s", "200.00")));
    }

    @Test
    public void testGenerateRegionalSalesReport_NotEOM_OutputFormat() throws IOException {
        reportGenerator = new ReportGenerator(mockParser, mockDateCalculator, mockCommissionCalculator, testRunDateNotEOM); // Not EOM
        List<EmployeeRecord> sampleRecords = createSampleRawRecords();
        when(mockParser.parseFile(anyString())).thenReturn(sampleRecords);
        when(mockDateCalculator.isEndOfMonth(testRunDateNotEOM)).thenReturn(false); // Not EOM
        // No need to mock management commission calls as they shouldn't happen for report

        reportGenerator.process("dummy.dat", "");
        String report = reportGenerator.generateRegionalSalesReport();

        assertTrue(report.contains("REGIONAL SALES REPORT - NOT GENERATED (NOT END OF MONTH)"));
        assertTrue(report.contains("RUN DATE: " + testRunDateNotEOM.toString()));
        assertFalse(report.contains("REGION: NORTH")); // Content should not be there
    }
}
