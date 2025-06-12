# Business Rules for CWXTDATE Program

This document outlines the business rules implemented in the COBOL program `CWXTDATE.cbl`. This program is called by other programs (e.g., `CWXTCOB`) to calculate years of service and determine if the current run date is the end of the month.

## Program Flow Control

The program uses a switch, `CHECKED-FOR-EOM-SW`, to control its execution flow:

- **Initial State**: `CHECKED-FOR-EOM-SW` is initialized to 'N'.
- **First Call**:
    - If `CHECKED-FOR-EOM-SW` is 'N' (meaning it's the first time the program is called in the current context or since initialization):
        - The `2000-CALC-END-OF-MONTH` paragraph is performed to determine if the run date is the end of the month and set the `EOM-SW` accordingly.
        - `CHECKED-FOR-EOM-SW` is then set to 'Y'.
- **Subsequent Calls**:
    - If `CHECKED-FOR-EOM-SW` is 'Y':
        - The `1000-CALC-YRS-OF-SERVICE` paragraph is performed to calculate the years of service.
        - The `2000-CALC-END-OF-MONTH` logic is skipped.

This ensures that the End of Month (EOM) status is determined only once (presumably on the first call with a specific run date or for the first employee processed in a batch), while Years of Service can be calculated multiple times (e.g., for different employees with different hire dates but the same run date).

## Calculation of Years of Service (`YRS-OF-SERVICE`)

This calculation is performed in the `1000-CALC-YRS-OF-SERVICE` paragraph using `RUN-DATE` (current system date when the calling program started) and `HIRE-DATE` (employee's hire date).

1.  **Initial Year Calculation**:
    - If `HIRE-YY` (Hire Year) > `RUN-YY` (Run Year):
        - This handles cases where the hire year is in the previous century (e.g., Hire Year '98', Run Year '02').
        - `YRS-OF-SERVICE = (100 + RUN-YY) - HIRE-YY`.
    - Else (Hire Year <= Run Year):
        - `YRS-OF-SERVICE = RUN-YY - HIRE-YY`.

2.  **Adjustment based on Month**:
    - If `HIRE-MM` (Hire Month) > `RUN-MM` (Run Month):
        - `YRS-OF-SERVICE` is decremented by 1. (The employee has not yet completed a full year in the current run year).

3.  **Adjustment based on Day (if Months are equal)**:
    - Else if `HIRE-MM` (Hire Month) = `RUN-MM` (Run Month):
        - If `HIRE-DD` (Hire Day) > `RUN-DD` (Run Day):
            - `YRS-OF-SERVICE` is decremented by 1. (The employee has not yet reached their anniversary day in the current month).

The final `YRS-OF-SERVICE` is passed back to the calling program.

## Determination of End of Month (`EOM-SW`)

This logic is performed in the `2000-CALC-END-OF-MONTH` paragraph and sets the `EOM-SW` to 'Y' if the `RUN-DATE` is the last day of the month. Otherwise, `EOM-SW` remains unchanged (presumably initialized to 'N' by the calling program or by default).

1.  **February Handling**:
    - If `RUN-MM` (Run Month) is 02 (February):
        - The program performs `3000-CALC-LEAP-YEAR`.
        - **Leap Year Check**:
            - `RUN-YY` is divided by 4.
            - If the remainder (`EXTRA-YEARS`) is 0, it's a leap year.
            - If it is a leap year AND `RUN-DD` (Run Day) is 29:
                - `EOM-SW` is set to 'Y'.
        - Note: If it's February, not a leap year, and day is 28, this specific logic doesn't set EOM-SW. This is covered by the general rule below using `DATE-TABLE` which has 28 for February. The leap year check specifically handles Feb 29th.

2.  **Other Months Handling**:
    - If `RUN-MM` is not 02:
        - The program checks if `RUN-DD` (Run Day) is equal to the last day of the `RUN-MM` (Run Month) by looking up `DATE-DD(RUN-MM)` in an internal `DATE-TABLE`.
        - `DATE-TABLE` stores the month and its corresponding last day (e.g., 0131 for Jan 31, 0228 for Feb 28, 0331 for Mar 31, etc.).
        - If `DATE-DD(RUN-MM) = RUN-DD`:
            - `EOM-SW` is set to 'Y'.

The `EOM-SW` is then available to the calling program.
