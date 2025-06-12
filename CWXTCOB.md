# Business Rules for CWXTCOB Program

This document outlines the business rules implemented in the COBOL program `CWXTCOB.cbl`.

## Input Parameter Validation (`PARM-DATA`)

The program accepts an optional input parameter (`PARM-DATA`) which influences the starting record for processing from the employee file.

- **Length of 5 and Numeric**: If `PARM-DATA` has a length of 5 and contains a numeric value, processing starts from the record number specified by `PARM-DATA`.
- **Length of 0 (or spaces)**: If `PARM-DATA` has a length of 0 (e.g., all spaces, effectively an empty parameter from JCL), processing starts from record number 1.
- **Other Cases**: If `PARM-DATA` does not meet the above criteria (e.g., non-numeric, or length not 0 or 5), an error message "PARAMETER LENGTH OR DATA IS INCORRECT" is printed, and main processing is skipped.

## Employee Processing

The program processes employee records based on their type, indicated by a specific code.

### General Employee Types:

- **'H'**: Hourly Employee
- **'S'**: Sales Employee
- **'M'**: Management Employee

### 1. Hourly Employee ('H') Processing

- **Regular Pay**: Calculated as `WA-EMP-HOURS * WA-EMP-RATE` if hours are less than or equal to 40.
- **Overtime Calculation**: If `WA-EMP-HOURS` is greater than 40:
    - Regular pay is calculated for the first 40 hours: `WA-EMP-RATE * 40`.
    - Overtime hours are calculated: `OT-HOURS = WA-EMP-HOURS - 40`.
    - Overtime amount is calculated at 1.5 times the hourly rate: `OT-AMOUNT = OT-HOURS * (WA-EMP-RATE * 1.5)`.
- **Total Compensation**: `EMP-COMPENSATION = EMP-WAGES + OT-AMOUNT`.
- This compensation data is then stored for the Employee Compensation Report.

### 2. Sales Employee ('S') Processing

- **Commission Calculation**:
    - If `WA-SALES-AMOUNT` is greater than 0:
        - The `WA-SALES-AMOUNT` is added to the respective `REGION-SALES` total.
        - The program `CWXTSUBC` is called with `EMP-TYPE = 'S'`, `CALC-SALES = WA-SALES-AMOUNT` to calculate `CALC-COMMISSION`.
    - **Zero Sales**: If `WA-SALES-AMOUNT` is 0, the literal "UH-OH" is moved to `REGION-COMMENT` for the employee's region. This comment appears on the Regional Sales Report.
- **Total Compensation**: `EMP-COMPENSATION = WA-SALES-SALARY + CALC-COMMISSION`.
- This compensation data is then stored for the Employee Compensation Report.

### 3. Management Employee ('M') Processing

- The employee's name (`WA-EMP-NAME`) is stored as `REGION-MANAGER` for their respective `WA-EMP-REGION`.
- The employee's salary (`WA-MGMT-SALARY`) is stored as `REGION-SALARY` for their respective `WA-EMP-REGION`.
- Management employees are not part of the Employee Compensation Report data storage in the `HOLD-TABLE`. Their details are used directly for the Regional Sales Report.

### Handling Invalid Employee Types

- If an employee record has a type other than 'H', 'S', or 'M', an error message "INVALID EMPLOYEE TYPE" is written to the report file, and that record's processing for compensation is skipped.

## Data Storage for Employee Compensation Report

- Employee details (for Hourly and Sales types) are stored in a `HOLD-TABLE` before printing the Employee Compensation Report.
- **Regional Storage**: The table is structured by region (`REG-IX`).
- **Maximum Employees per Region**: The `HOLD-TABLE` can store a maximum of 20 employees per region (`HOLD-LINE OCCURS 20 TIMES`).
    - North: `NORTH-COUNT` tracks the number of employees.
    - South: `SOUTH-COUNT` tracks the number of employees.
    - East: `EAST-COUNT` tracks the number of employees.
    - West: `WEST-COUNT` tracks the number of employees.
- **Data Stored**: Includes name, region, type, hire date, years of service, wages, overtime (for hourly), commission (for sales), and total compensation.

### Handling of Invalid Regions when Storing Employee Details

- During the `5100-SET-INDEX` paragraph, if the `WA-EMP-REGION` is not one of the valid regions (1 for North, 2 for South, 3 for East, 4 for West):
    - `REGION-ERROR-SW` is set to 'Y' (`INVALID-REGION`).
    - The employee's details are **not** stored in the `HOLD-TABLE` because `5000-STORE-EMPLOYEE-DETAIL` checks `IF VALID-REGION` before moving data.
    - The program does not explicitly print an error message for an invalid region during this specific storage step, but the record won't appear in the Employee Compensation report for that region. (Note: The problem description mentions an S0C7 abend for an invalid region in record 00002 due to table overflow, implying this check might not prevent all issues if counts exceed table limits before this explicit check, or if the region itself is out of bounds for `REG-IX`.)

## Regional Sales Report

- **Printing Condition**: The Regional Sales Report is printed only if `END-OF-MONTH-SW` is 'Y'. This switch is set by the called program `CWXTDATE`.
- **Content**: This report summarizes sales and manager compensation for each of the 4 regions (North, South, East, West).
- **Management Commission Calculation**:
    - For each region, the program `CWXTSUBC` is called with `EMP-TYPE = 'M'` and `CALC-SALES = REGION-SALES (REGION-SUB)` (total sales for that region) to calculate `CALC-COMMISSION` for the manager.
    - Manager's total compensation is `MGMT-COMPENSATION = CALC-COMMISSION + REGION-SALARY (REGION-SUB)`.
- **"UH-OH" Comment**: If a sales employee in a region had 0 sales, the `REGION-COMMENT` field for that region will contain "UH-OH" and be printed on this report.
