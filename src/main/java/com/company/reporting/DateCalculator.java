package com.company.reporting;

import java.time.LocalDate;
import java.time.YearMonth;

/**
 * Utility class for performing date-related calculations based on CWXTDATE COBOL program logic.
 * This includes calculating years of service and determining if a date is the end of the month.
 */
public class DateCalculator {

    /**
     * Calculates the years of service between a hire date and a run date.
     * The calculation logic is derived from the CWXTDATE COBOL program specifications.
     * It correctly handles year differences and adjusts based on month and day.
     *
     * @param hireDate The employee's hire date. Must not be null.
     * @param runDate The date against which service is calculated (typically the current run date). Must not be null.
     * @return The calculated number of full years of service. Can be negative if hireDate is after runDate.
     */
    public int calculateYearsOfService(LocalDate hireDate, LocalDate runDate) {
        int hireYear = hireDate.getYear();
        int hireMonth = hireDate.getMonthValue();
        int hireDay = hireDate.getDayOfMonth();

        int runYear = runDate.getYear();
        int runMonth = runDate.getMonthValue();
        int runDay = runDate.getDayOfMonth();

        int yearsOfService;

        yearsOfService = runYear - hireYear;

        if (hireMonth > runMonth) {
            yearsOfService--;
        } else if (hireMonth == runMonth) {
            if (hireDay > runDay) {
                yearsOfService--;
            }
        }
        return yearsOfService;
    }

    /**
     * Determines if the given date is the last day of its month.
     * The logic includes specific handling for leap years in February, as per CWXTDATE.
     *
     * @param runDate The date to check. Must not be null.
     * @return True if the date is the end of the month, false otherwise.
     */
    public boolean isEndOfMonth(LocalDate runDate) {
        int runMonth = runDate.getMonthValue();
        int runDay = runDate.getDayOfMonth();
        // int runYear = runDate.getYear(); // runYear variable was not used from original code

        // Check for February - original code had specific logic here, but YearMonth.lengthOfMonth() handles it.
        // if (runMonth == 2) { // February
        //     boolean isLeap = java.time.Year.isLeap(runYear);
        //     if (isLeap && runDay == 29) {
        //         return true;
        //     }
        // }

        // General check for all months using YearMonth
        YearMonth yearMonth = YearMonth.from(runDate);
        return runDay == yearMonth.lengthOfMonth();
    }
}
