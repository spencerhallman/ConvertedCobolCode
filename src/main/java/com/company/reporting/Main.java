package com.company.reporting;

import java.time.LocalDate;
import java.time.format.DateTimeParseException;
import java.util.logging.ConsoleHandler;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.logging.SimpleFormatter;

/**
 * Main entry point for the Java Batch Reporting Application.
 * This class is responsible for:
 * <ul>
 *   <li>Parsing command-line arguments for input file path, run date, and optional start record parameter.</li>
 *   <li>Setting up basic console logging.</li>
 *   <li>Initializing and coordinating the core components (parser, calculators, report generator).</li>
 *   <li>Invoking the report generation process.</li>
 *   <li>Printing the generated reports to the standard output.</li>
 *   <li>Handling application-level errors and setting appropriate exit codes.</li>
 * </ul>
 * The application re-implements business logic from a set of COBOL programs
 * to produce an Employee Compensation Report and a Regional Sales Report.
 */
public class Main {
    private static final Logger APP_LOGGER = Logger.getLogger("com.company.reporting"); // Root logger for the app

    /**
     * Main method for the application.
     *
     * @param args Command-line arguments:
     *             <ul>
     *               <li>args[0] (Required): Path to the employee data file.</li>
     *               <li>args[1] (Optional): Run date in YYYY-MM-DD format. Defaults to current system date.</li>
     *               <li>args[2] (Optional): 5-digit numeric string (PARM-DATA) to start processing from a specific record.</li>
     *             </ul>
     */
    public static void main(String[] args) {
        // Setup basic console logging
        APP_LOGGER.setUseParentHandlers(false);
        Handler consoleHandler = new ConsoleHandler();
        consoleHandler.setFormatter(new SimpleFormatter());
        consoleHandler.setLevel(Level.INFO);
        APP_LOGGER.addHandler(consoleHandler);
        APP_LOGGER.setLevel(Level.INFO);


        if (args.length < 1) {
            APP_LOGGER.severe("Usage: java -jar <jarfile> <employeeFilePath> [runDate YYYY-MM-DD] [parmData NNNNN]");
            System.err.println("Usage: java -jar <jarfile> <employeeFilePath> [runDate YYYY-MM-DD] [parmData NNNNN]");
            System.exit(1);
            return;
        }

        String employeeFilePath = args[0];
        LocalDate runDate = LocalDate.now();
        String parmData = "";

        if (args.length > 1) {
            try {
                runDate = LocalDate.parse(args[1]);
                APP_LOGGER.info("Using provided run date: " + runDate);
            } catch (DateTimeParseException e) {
                APP_LOGGER.warning("Invalid runDate format: " + args[1] + ". Using current system date instead.");
            }
        } else {
             APP_LOGGER.info("No runDate provided. Using current system date: " + runDate);
        }

        if (args.length > 2) {
            parmData = args[2];
            if (parmData.matches("\\d{5}") || parmData.trim().isEmpty()) {
                 APP_LOGGER.info("Using parmData: '" + parmData + "'");
            } else {
                APP_LOGGER.severe("PARAMETER LENGTH OR DATA IS INCORRECT: " + parmData + ". Processing will be affected as per ReportGenerator logic.");
            }
        } else {
            APP_LOGGER.info("No parmData provided. Processing all records from the start.");
        }


        // Initialize components
        EmployeeFileParser employeeFileParser = new EmployeeFileParser();
        DateCalculator dateCalculator = new DateCalculator();
        CommissionCalculator commissionCalculator = new CommissionCalculator();

        ReportGenerator reportGenerator = new ReportGenerator(
                employeeFileParser,
                dateCalculator,
                commissionCalculator,
                runDate
        );

        try {
            APP_LOGGER.info("Starting report generation process...");
            reportGenerator.process(employeeFilePath, parmData);

            String empCompReport = reportGenerator.generateEmployeeCompensationReport();
            System.out.println("\n--- Employee Compensation Report ---");
            System.out.println(empCompReport);

            String regionalSalesReport = reportGenerator.generateRegionalSalesReport();
            System.out.println("\n--- Regional Sales Report ---");
            System.out.println(regionalSalesReport);

            APP_LOGGER.info("Report generation process completed successfully.");

        } catch (Exception e) {
            APP_LOGGER.log(Level.SEVERE, "An error occurred during report generation: " + e.getMessage(), e);
            System.err.println("Application Error: " + e.getMessage());
            System.exit(1);
        }
        System.exit(0);
    }
}
