# Business Rules for CWXTSUBC Program

This document outlines the business rules implemented in the COBOL subprogram `CWXTSUBC.cbl`. This program is called to calculate commissions based on an employee type and a sales amount.

## Program Purpose and Parameters

`CWXTSUBC` calculates commission (`COMM-TOTAL`) based on the provided `EMP-TYPE` (employee type) and `SALES-AMOUNT`.

**Linkage Section Parameters:**

-   `EMP-TYPE` (PIC X): Input, determines the type of commission processing.
    -   'S': Sales Employee
    -   'M': Management Employee
-   `SALES-AMOUNT` (PIC 9(6)V99): Input, the amount used to calculate commission. For sales employees, this is their individual sales. For management, this is the total regional sales.
-   `COMM-TOTAL` (PIC 9(5)V99 COMP-3): Output, the calculated commission.

## Main Logic Branching

The program's primary logic depends on the value of `EMP-TYPE`:

-   If `EMP-TYPE` is **'S'**: The program performs the `1000-PROCESS-SALES` paragraph to calculate sales commission.
-   If `EMP-TYPE` is **'M'**: The program performs the `2000-PROCESS-MANAGEMENT` paragraph to calculate management commission.

## Sales Commission Calculation (`1000-PROCESS-SALES`)

This section calculates commission for sales employees based on their individual `SALES-AMOUNT`.

1.  **Determine Commission Rate (`SALES-RATE`)**:
    The `SALES-AMOUNT` is checked against predefined ranges to determine the applicable `SALES-RATE`. The rates are stored in `SALES-RATE-TABLE`.

    | Sales Amount Range        | `SALES-RATE` | Index (`SALES-IX`) |
    | :------------------------ | :----------- | :----------------- |
    | 1      thru 20000.00      | 0.02         | 1                  |
    | 20000.01 thru 40000.00    | 0.04         | 2                  |
    | 40000.01 thru 60000.00    | 0.06         | 3                  |
    | 60000.01 thru 80000.00    | 0.08         | 4                  |
    | 80000.01 thru 100000.00   | 0.10         | 5                  |
    *(Note: The program uses `IF S-RANGE1... ELSE IF S-RANGE2...` which implies that any amount greater than 100000 would also fall into the last category with `SALES-IX TO 5` if the `SALES-RANGE` variable is simply moved from `SALES-AMOUNT`. However, the defined `88` levels only go up to 100000. Sales amounts exceeding 100000 will use the 0.10 rate based on the COBOL evaluation sequence.)*

2.  **Calculate Commission**:
    `COMM-TOTAL = SALES-AMOUNT * SALES-RATE(SALES-IX)`

## Management Commission Calculation (`2000-PROCESS-MANAGEMENT`)

This section calculates commission for management employees based on `SALES-AMOUNT`, which represents the total sales for their region.

1.  **Determine Commission Rate (`MGMT-RATE`)**:
    The `SALES-AMOUNT` (total regional sales) is checked against predefined ranges to determine the applicable `MGMT-RATE`. The rates are stored in `MGMT-RATE-TABLE`.

    | Sales Amount (Regional) Range | `MGMT-RATE` | Index (`MGMT-IX`) |
    | :---------------------------- | :---------- | :---------------- |
    | 1        thru 100000.00       | 0.020       | 1                 |
    | 100000.01 thru 200000.00      | 0.025       | 2                 |
    | 200000.01 thru 300000.00      | 0.030       | 3                 |
    | 300000.01 thru 400000.00      | 0.035       | 4                 |
    | 400000.01 thru 500000.00      | 0.040       | 5                 |
    *(Note: Similar to sales commission, the COBOL evaluation `IF M-RANGE1... ELSE IF M-RANGE2...` implies amounts exceeding 500000 will use the 0.040 rate.)*

2.  **Calculate Commission**:
    `COMM-TOTAL = SALES-AMOUNT * MGMT-RATE(MGMT-IX)`
