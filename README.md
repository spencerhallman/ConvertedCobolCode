# Java Batch Reporting Application

## Overview

This Java application is a batch reporting tool designed to process employee data and generate two key reports: an Employee Compensation Report and a Regional Sales Report. It re-implements business logic originally defined in a set of COBOL programs (CWXTCOB, CWXTDATE, CWXTSUBC).

## Features

*   Parses fixed-width employee data files.
*   Calculates years of service for employees.
*   Determines End-of-Month (EOM) status for conditional report generation.
*   Calculates commissions for Sales and Management employees based on tiered rates.
*   Processes different employee types (Hourly, Sales, Management) with specific compensation logic:
    *   Hourly: Calculates regular and overtime pay.
    *   Sales: Calculates salary plus commission; notes "UH-OH" for zero sales in a region.
    *   Management: Assigns manager to a region and calculates commission on total regional sales (if EOM).
*   Generates an Employee Compensation Report detailing individual pay components.
*   Generates a Regional Sales Report (only if run on EOM) summarizing sales by region and manager compensation.
*   Supports an optional input parameter to start processing from a specific record number.
*   Provides console logging for application activity.

## Project Structure

The project is a standard Maven application:

*   `src/main/java/com/company/reporting/`: Contains all Java source code.
    *   `Main.java`: Entry point of the application.
    *   `ReportGenerator.java`: Core class orchestrating data processing and report generation.
    *   `EmployeeFileParser.java`: Handles parsing of the input employee file.
    *   `DateCalculator.java`: Implements logic from CWXTDATE (years of service, EOM check).
    *   `CommissionCalculator.java`: Implements logic from CWXTSUBC (commission calculations).
    *   Domain classes (`Employee.java`, `EmployeeRecord.java`, `CompensationData.java`, `RegionalSalesData.java`, `EmployeeType.java`, `Region.java`).
*   `src/main/resources/`: For application resources (currently none specific).
*   `src/test/java/com/company/reporting/`: Contains JUnit tests.
*   `src/test/resources/`: For test resources, like `test_empfile.dat`.
*   `pom.xml`: Maven project configuration.

## Input File (`EMPFILE.DAT`)

The application expects an employee data file, typically named `EMPFILE.DAT` (though any name can be passed as a command-line argument).

**Format:** Fixed-width text file. Each line represents an employee record.
The parser currently expects lines to be effectively **52 characters** long. (This was an adjustment made during development due to observed behavior in the test environment when reading lines defined as 53 characters).

**Record Layout (Assumed and Implemented):**

| Position(s) | Length | Field Name                  | Type   | Description / Example                 |
|-------------|--------|-----------------------------|--------|---------------------------------------|
| 1-15        | 15     | Employee Name               | String | `John Smith     `                    |
| 16-23       | 8      | Hire Date                   | String | `20200115` (YYYYMMDD)               |
| 24-24       | 1      | Employee Type               | Char   | 'H' (Hourly), 'S' (Sales), 'M' (Mgmt) |
| 25-29       | 5      | Employee Region Code        | String | `1    ` (Numeric part used for region) |
| 30-37       | 8      | Salary / Base Pay           | String | `03500.00` (Numeric)                |
| 38-45       | 8      | Hours Worked / Sales Amount | String | `0040.00` or `05000.00` (Numeric)   |
| 46-52       | 7      | Hourly Rate                 | String | `0010.75` (Numeric, 7 chars for this field due to line length) |

*(Note: The last field, Hourly Rate, is 7 characters due to the effective 52-character line length. All other fields are as per original plan for 53 chars, but the parser adapts.)*

## Building the Application

**Prerequisites:**
*   Java Development Kit (JDK) 8 or higher.
*   Apache Maven 3.x.

**Command:**
Open a terminal in the project root directory and run:
```bash
mvn clean package
```
This will compile the code, run tests, and create an executable JAR file in the `target/` directory (e.g., `target/java-reporting-app-1.0-SNAPSHOT.jar`).

## Running the Application

**Command:**
```bash
java -jar target/java-reporting-app-1.0-SNAPSHOT.jar <employeeFilePath> [runDate] [parmData]
```

**Arguments:**
1.  `<employeeFilePath>` (Required): Path to the employee data file (e.g., `data/EMPFILE.DAT`).
2.  `[runDate]` (Optional): The date for the report run, in `YYYY-MM-DD` format (e.g., `2023-10-31`). Defaults to the current system date if not provided or if format is invalid.
3.  `[parmData]` (Optional): A 5-digit numeric string (e.g., `00005`). If provided, processing starts from this record number in the input file. If this parameter is present but invalid (not 5 digits, not numeric, not empty/spaces), main processing is skipped as per original COBOL logic.

**Example:**
```bash
java -jar target/java-reporting-app-1.0-SNAPSHOT.jar EMPFILE.DAT 2023-12-31 00001
```
(Assuming `EMPFILE.DAT` is in the same directory or you provide a full path).

## Output

The application prints two reports to the console:
1.  **Employee Compensation Report:** Details individual employee compensation, including wages/salary, overtime, commission, and total compensation, grouped by region.
2.  **Regional Sales Report:** Summarizes sales by region, manager details, and manager's commission (this report is only fully generated if the run date is the end of the month).

## Original Business Rules

The business logic for this application is derived from the following COBOL program specifications:
*   `CWXTCOB.md` (Main processing logic)
*   `CWXTDATE.md` (Date calculations)
*   `CWXTSUBC.md` (Subprogram for commission calculations)
```
