package com.company.reporting;

import java.io.IOException;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.EnumMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.text.NumberFormat;
import java.util.Locale;
import java.util.Comparator; // For sorting compensation data by name

/**
 * Orchestrates the generation of employee compensation and regional sales reports.
 * This class uses various helper classes to parse input data, perform calculations,
 * and format the final reports.
 * It processes employee records from a file, calculates pay and commissions,
 * aggregates regional data, and then generates string representations of the reports.
 */
public class ReportGenerator {

    private static final Logger LOGGER = Logger.getLogger(ReportGenerator.class.getName());
    private static final NumberFormat CURRENCY_FORMAT = NumberFormat.getCurrencyInstance(Locale.US); // For $ formatting
    private final EmployeeFileParser employeeFileParser;
    private final DateCalculator dateCalculator;
    private final CommissionCalculator commissionCalculator;
    private LocalDate runDate;

    private Map<Region, List<CompensationData>> employeeCompensationData;
    private Map<Region, RegionalSalesData> regionalSalesDataMap;
    private boolean isEOM; // End of Month status

    /**
     * Constructs a ReportGenerator.
     *
     * @param employeeFileParser   The parser for reading employee records.
     * @param dateCalculator       The calculator for date-related logic (YOS, EOM).
     * @param commissionCalculator The calculator for sales and management commissions.
     * @param runDate              The date for which the reports are being generated.
     */
    public ReportGenerator(EmployeeFileParser employeeFileParser,
                                 DateCalculator dateCalculator,
                                 CommissionCalculator commissionCalculator,
                                 LocalDate runDate) {
        this.employeeFileParser = employeeFileParser;
        this.dateCalculator = dateCalculator;
        this.commissionCalculator = commissionCalculator;
        this.runDate = runDate;

        this.employeeCompensationData = new EnumMap<>(Region.class);
        for (Region r : Region.values()) {
            if (r != Region.INVALID) {
                employeeCompensationData.put(r, new ArrayList<>());
            }
        }

        this.regionalSalesDataMap = new EnumMap<>(Region.class);
        for (Region r : Region.values()) {
             if (r != Region.INVALID) {
                regionalSalesDataMap.put(r, new RegionalSalesData(r.name()));
            }
        }
    }

    /**
     * Processes the employee data file and prepares data for report generation.
     * This method reads records, performs calculations for each employee,
     * and aggregates data for regional summaries.
     *
     * @param inputFilePath The path to the employee data file.
     * @param parmData      Optional 5-digit string to start processing from a specific record number.
     *                      If invalid and not empty, main processing is skipped.
     */
    public void process(String inputFilePath, String parmData) {
        LOGGER.info("ReportGenerator processing started for file: " + inputFilePath + " with Run Date: " + runDate);
        long startRecordNum = 1;
        if (parmData != null && parmData.matches("\\d{5}")) {
            try {
                startRecordNum = Long.parseLong(parmData);
                LOGGER.info("PARM-DATA provided: Processing will start from record number " + startRecordNum);
            } catch (NumberFormatException e) {
                LOGGER.warning("PARM-DATA is numeric but too large or invalid: " + parmData + ". Starting from record 1.");
                startRecordNum = 1;
            }
        } else if (parmData != null && !parmData.trim().isEmpty()) {
            LOGGER.severe("PARAMETER LENGTH OR DATA IS INCORRECT: " + parmData + ". Main processing skipped.");
            return;
        }

        List<EmployeeRecord> rawRecords;
        try {
            rawRecords = employeeFileParser.parseFile(inputFilePath);
        } catch (IOException e) {
            LOGGER.severe("Error reading employee file: " + e.getMessage());
            return;
        }

        this.isEOM = dateCalculator.isEndOfMonth(this.runDate);
        LOGGER.info("Is End of Month: " + this.isEOM);

        long currentRecordCount = 0;
        for (int i = 0; i < rawRecords.size(); i++) {
            currentRecordCount++;
            if (currentRecordCount < startRecordNum) {
                continue;
            }

            EmployeeRecord rawRecord = rawRecords.get(i);
            int recordLineNum = i + 1;

            LocalDate hireDate = EmployeeFileParser.parseDate(rawRecord.rawHireDate, recordLineNum);
            EmployeeType empType = EmployeeFileParser.parseEmployeeType(rawRecord.rawEmpType, recordLineNum);
            Region region = EmployeeFileParser.parseRegion(rawRecord.rawEmpRegionCode, recordLineNum);

            if (hireDate == null || empType == EmployeeType.UNKNOWN) {
                LOGGER.warning("Skipping record " + recordLineNum + " due to parsing errors (date/type). Name: " + rawRecord.rawEmpName);
                continue;
            }

            Employee employee = new Employee(rawRecord.rawEmpName, hireDate, empType, region);
            employee.yearsOfService = dateCalculator.calculateYearsOfService(hireDate, this.runDate);

            switch (empType) {
                case HOURLY:
                    processHourlyEmployee(employee, rawRecord, recordLineNum);
                    break;
                case SALES:
                    processSalesEmployee(employee, rawRecord, recordLineNum);
                    break;
                case MANAGEMENT:
                    processManagementEmployee(employee, rawRecord, recordLineNum);
                    break;
                default:
                    LOGGER.warning("Record " + recordLineNum + ": Unhandled employee type '" + empType + "' for " + employee.name);
                    break;
            }
        }

        for (RegionalSalesData rsd : regionalSalesDataMap.values()) {
            if (rsd != null) {
                 rsd.managerTotalCompensation = rsd.managerSalary;
            }
        }

        if (this.isEOM) {
            calculateManagementCommissionsForRegionalReport();
        }

        LOGGER.info("ReportGenerator processing finished.");
    }

    private void processHourlyEmployee(Employee employee, EmployeeRecord rawRecord, int lineNum) {
        employee.hoursWorked = EmployeeFileParser.parseDouble(rawRecord.rawHoursOrSalesAmount, "HoursWorked", lineNum);
        employee.hourlyRate = EmployeeFileParser.parseDouble(rawRecord.rawRate, "HourlyRate", lineNum);

        double regularHours = Math.min(employee.hoursWorked, 40.0);
        double overtimeHours = Math.max(0, employee.hoursWorked - 40.0);

        employee.regularPay = regularHours * employee.hourlyRate;
        employee.overtimePay = overtimeHours * (employee.hourlyRate * 1.5);
        employee.totalCompensation = employee.regularPay + employee.overtimePay;

        employee.regularPay = round(employee.regularPay);
        employee.overtimePay = round(employee.overtimePay);
        employee.totalCompensation = round(employee.totalCompensation);

        storeCompensationData(employee, employee.regularPay, employee.overtimePay, 0.0);
    }

    private void processSalesEmployee(Employee employee, EmployeeRecord rawRecord, int lineNum) {
        employee.salesAmount = EmployeeFileParser.parseDouble(rawRecord.rawHoursOrSalesAmount, "SalesAmount", lineNum);
        employee.salesSalary = EmployeeFileParser.parseDouble(rawRecord.rawSalaryOrBasePay, "SalesSalary", lineNum);

        double commission = 0.0;
        if (employee.salesAmount > 0) {
            commission = commissionCalculator.calculateCommission(EmployeeType.SALES, employee.salesAmount);
            if (employee.region != Region.INVALID && regionalSalesDataMap.containsKey(employee.region)) {
               RegionalSalesData rsd = regionalSalesDataMap.get(employee.region);
               rsd.totalRegionalSales += employee.salesAmount;
               rsd.totalRegionalSales = round(rsd.totalRegionalSales);
            }
        } else {
            if (employee.region != Region.INVALID && regionalSalesDataMap.containsKey(employee.region)) {
                regionalSalesDataMap.get(employee.region).comment = "UH-OH";
                LOGGER.info("Sales employee " + employee.name + " in region " + employee.region + " has 0 sales. Setting UH-OH.");
            }
        }
        employee.commission = round(commission);
        employee.totalCompensation = round(employee.salesSalary + employee.commission);

        storeCompensationData(employee, employee.salesSalary, 0.0, employee.commission);
    }

    private void processManagementEmployee(Employee employee, EmployeeRecord rawRecord, int lineNum) {
        employee.managementSalary = EmployeeFileParser.parseDouble(rawRecord.rawSalaryOrBasePay, "ManagementSalary", lineNum);
        if (employee.region != Region.INVALID && regionalSalesDataMap.containsKey(employee.region)) {
            RegionalSalesData rsd = regionalSalesDataMap.get(employee.region);
            rsd.managerName = employee.name;
            rsd.managerSalary = round(employee.managementSalary);
        } else {
             LOGGER.warning("Management employee " + employee.name + " has an invalid region: " + employee.region + ". Cannot assign to regional report.");
        }
    }

    private void storeCompensationData(Employee emp, double basePay, double overtime, double commission) {
        if (emp.region == Region.INVALID) {
            LOGGER.warning("Employee " + emp.name + " has an invalid region. Not storing for compensation report.");
            return;
        }
        List<CompensationData> regionList = employeeCompensationData.get(emp.region);
        if (regionList != null && regionList.size() < 20) {
            CompensationData cd = new CompensationData(
                emp.name,
                emp.region.name(),
                emp.type.name(),
                emp.hireDate,
                emp.yearsOfService,
                basePay,
                overtime,
                commission,
                emp.totalCompensation
            );
            regionList.add(cd);
        } else if (regionList != null) {
            LOGGER.warning("Region " + emp.region + " compensation data store is full (20 employees). Cannot add " + emp.name);
        } else {
            LOGGER.severe("Region " + emp.region + " list not found in employeeCompensationData map.");
        }
    }

    private void calculateManagementCommissionsForRegionalReport() {
        LOGGER.info("Calculating management commissions for Regional Sales Report.");
        for (Region region : regionalSalesDataMap.keySet()) {
            if (region == Region.INVALID) continue;

            RegionalSalesData rsd = regionalSalesDataMap.get(region);
            if (rsd.totalRegionalSales > 0) {
                rsd.managerCommission = commissionCalculator.calculateCommission(EmployeeType.MANAGEMENT, rsd.totalRegionalSales);
                rsd.managerCommission = round(rsd.managerCommission);
            }
            rsd.managerTotalCompensation = round(rsd.managerSalary + rsd.managerCommission);
        }
    }

    private double round(double value) {
        return BigDecimal.valueOf(value).setScale(2, RoundingMode.HALF_UP).doubleValue();
    }

    /**
     * Retrieves the map of employee compensation data, keyed by region.
     * The list of {@link CompensationData} for each region is sorted by employee name.
     * This data is populated after the {@link #process(String, String)} method has been called.
     *
     * @return A map where each key is a {@link Region} and the value is a list of {@link CompensationData}
     *         objects for employees in that region.
     */
    public Map<Region, List<CompensationData>> getEmployeeCompensationData() {
        // Ensure data is sorted if accessed this way before report generation
        // However, sorting is done in generateEmployeeCompensationReport, which is typical for presentation.
        return employeeCompensationData;
    }

    /**
     * Retrieves the map of regional sales data, keyed by region.
     * This data is populated after the {@link #process(String, String)} method has been called,
     * and management commissions are finalized if it was an End of Month run.
     *
     * @return A map where each key is a {@link Region} and the value is a {@link RegionalSalesData} object.
     */
    public Map<Region, RegionalSalesData> getRegionalSalesDataMap() {
        return regionalSalesDataMap;
    }

    /**
     * Checks if the End of Month (EOM) status was determined during processing.
     * The EOM status is determined when the {@link #process(String, String)} method is called.
     *
     * @return True if EOM processing was applicable and determined, false otherwise.
     */
    public boolean isEndOfMonthDetermined() {
        return isEOM;
    }

    private String formatCurrency(double value) {
        return String.format("%,.2f", value);
    }

    /**
     * Generates the Employee Compensation Report as a formatted string.
     * This report lists employees by region, detailing their hire date, years of service,
     * type, and various pay components (wages/salary, overtime, commission, total).
     * Employee data within each region is sorted by name.
     * The {@link #process(String, String)} method must be called before this.
     *
     * @return A string containing the formatted Employee Compensation Report.
     */
    public String generateEmployeeCompensationReport() {
        LOGGER.info("Generating Employee Compensation Report...");
        StringBuilder report = new StringBuilder();
        report.append(String.format("%80s%n", "EMPLOYEE COMPENSATION REPORT"));
        report.append(String.format("RUN DATE: %s%n%n", this.runDate.toString()));

        for (Region region : Region.values()) {
            if (region == Region.INVALID || !employeeCompensationData.containsKey(region) || employeeCompensationData.get(region).isEmpty()) {
                continue;
            }
            List<CompensationData> dataList = employeeCompensationData.get(region);
            dataList.sort(Comparator.comparing(cd -> cd.employeeName));


            report.append(String.format("REGION: %s%n", region.name()));
            report.append("-".repeat(115)).append(String.format("%n"));
            report.append(String.format("%-18s %-10s %8s %-10s %15s %12s %15s %15s%n",
                    "NAME", "HIRE DATE", "YRS SERV", "TYPE", "WAGES/SALARY", "OVERTIME", "COMMISSION", "TOTAL COMP"));
            report.append("-".repeat(115)).append(String.format("%n"));

            for (CompensationData cd : dataList) {
                report.append(String.format("%-18s %-10s %8d %-10s %15s %12s %15s %15s%n",
                        cd.employeeName.substring(0, Math.min(cd.employeeName.length(), 18)),
                        cd.hireDate.toString(),
                        cd.yearsOfService,
                        cd.employeeType,
                        formatCurrency(cd.wages),
                        formatCurrency(cd.overtime),
                        formatCurrency(cd.commission),
                        formatCurrency(cd.totalCompensation)
                ));
            }
            report.append("-".repeat(115)).append(String.format("%n%n"));
        }
        LOGGER.info("Employee Compensation Report generated.");
        return report.toString();
    }

    /**
     * Generates the Regional Sales Report as a formatted string.
     * This report is only generated if the run date was determined to be the End of Month (EOM).
     * If not EOM, it returns a message indicating that the report was not generated.
     * The report details manager information, total sales, and manager compensation for each region.
     * The {@link #process(String, String)} method must be called before this.
     *
     * @return A string containing the formatted Regional Sales Report, or a non-generation message if not EOM.
     */
    public String generateRegionalSalesReport() {
        LOGGER.info("Generating Regional Sales Report...");
        StringBuilder report = new StringBuilder();

        if (!this.isEOM) {
            String message = "REGIONAL SALES REPORT - NOT GENERATED (NOT END OF MONTH)";
            LOGGER.info(message);
            report.append(String.format("%n%80s%n", message));
            report.append(String.format("RUN DATE: %s%n", this.runDate.toString()));
            return report.toString();
        }

        report.append(String.format("%80s%n", "REGIONAL SALES REPORT"));
        report.append(String.format("RUN DATE: %s%n%n", this.runDate.toString()));

        for (Region region : Region.values()) {
             if (region == Region.INVALID || !regionalSalesDataMap.containsKey(region)) {
                continue;
            }
            RegionalSalesData rsd = regionalSalesDataMap.get(region);

            // Condition to print region block (original COBOL logic might differ slightly on empty regions)
            // if (rsd.managerName == null && rsd.totalRegionalSales == 0 && (rsd.comment == null || rsd.comment.isEmpty())) {
            //    continue; // Optionally skip completely empty regions
            // }

            report.append(String.format("REGION: %s%n", region.name()));
            report.append(String.format("  MANAGER:          %s%n", rsd.managerName != null ? rsd.managerName : "N/A"));
            report.append(String.format("  MANAGER SALARY:   %15s%n", formatCurrency(rsd.managerSalary)));
            report.append(String.format("  TOTAL SALES:      %15s%n", formatCurrency(rsd.totalRegionalSales)));
            report.append(String.format("  MANAGER COMM:     %15s%n", formatCurrency(rsd.managerCommission)));
            report.append(String.format("  MANAGER TOTAL:    %15s%n", formatCurrency(rsd.managerTotalCompensation)));
            if (rsd.comment != null && !rsd.comment.isEmpty()) {
                report.append(String.format("  COMMENT:          %s%n", rsd.comment));
            }
            report.append("-".repeat(60)).append(String.format("%n%n"));
        }
        LOGGER.info("Regional Sales Report generated.");
        return report.toString();
    }
}
