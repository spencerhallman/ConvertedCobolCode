package com.company.reporting;

import org.junit.Test;
import java.time.LocalDate;
import static org.junit.Assert.*;

public class DateCalculatorTest {

    private final DateCalculator calculator = new DateCalculator();

    // --- Tests for calculateYearsOfService ---

    @Test
    public void testYearsOfService_SameYear_BeforeAnniversaryMonth() {
        LocalDate hireDate = LocalDate.of(2023, 10, 15);
        LocalDate runDate = LocalDate.of(2023, 9, 20);
        assertEquals(-1, calculator.calculateYearsOfService(hireDate, runDate)); // Or 0, depending on strict interpretation
    }

    @Test
    public void testYearsOfService_ExactlyOneYear() {
        LocalDate hireDate = LocalDate.of(2022, 8, 15);
        LocalDate runDate = LocalDate.of(2023, 8, 15);
        assertEquals(1, calculator.calculateYearsOfService(hireDate, runDate));
    }

    @Test
    public void testYearsOfService_AlmostOneYear_DayBefore() {
        LocalDate hireDate = LocalDate.of(2022, 8, 15);
        LocalDate runDate = LocalDate.of(2023, 8, 14);
        assertEquals(0, calculator.calculateYearsOfService(hireDate, runDate));
    }

    @Test
    public void testYearsOfService_AlmostOneYear_MonthBefore() {
        LocalDate hireDate = LocalDate.of(2022, 8, 15);
        LocalDate runDate = LocalDate.of(2023, 7, 20);
        assertEquals(0, calculator.calculateYearsOfService(hireDate, runDate));
    }

    @Test
    public void testYearsOfService_MultipleYears() {
        LocalDate hireDate = LocalDate.of(2000, 6, 10);
        LocalDate runDate = LocalDate.of(2023, 6, 10);
        assertEquals(23, calculator.calculateYearsOfService(hireDate, runDate));
    }

    @Test
    public void testYearsOfService_MultipleYears_JustPastAnniversary() {
        LocalDate hireDate = LocalDate.of(2000, 6, 10);
        LocalDate runDate = LocalDate.of(2023, 6, 11);
        assertEquals(23, calculator.calculateYearsOfService(hireDate, runDate));
    }

    @Test
    public void testYearsOfService_MultipleYears_JustBeforeAnniversaryMonth() {
        LocalDate hireDate = LocalDate.of(2000, 6, 10);
        LocalDate runDate = LocalDate.of(2023, 5, 20);
        assertEquals(22, calculator.calculateYearsOfService(hireDate, runDate));
    }

    @Test
    public void testYearsOfService_MultipleYears_JustBeforeAnniversaryDayInMonth() {
        LocalDate hireDate = LocalDate.of(2000, 6, 10);
        LocalDate runDate = LocalDate.of(2023, 6, 9);
        assertEquals(22, calculator.calculateYearsOfService(hireDate, runDate));
    }

    @Test
    public void testYearsOfService_CrossingCentury_COBOLExample() {
        // CWXTDATE.md: If HIRE-YY > RUN-YY (e.g. Hire 98, Run 02)
        // (100 + 02) - 98 = 4 years. E.g. 1998-MM-DD to 2002-MM-DD
        LocalDate hireDate = LocalDate.of(1998, 10, 1);
        LocalDate runDate = LocalDate.of(2002, 10, 1);
        assertEquals(4, calculator.calculateYearsOfService(hireDate, runDate));

        LocalDate hireDate2 = LocalDate.of(1998, 10, 1);
        LocalDate runDate2 = LocalDate.of(2002, 9, 30);
        assertEquals(3, calculator.calculateYearsOfService(hireDate2, runDate2));
    }

    // --- Tests for isEndOfMonth ---

    @Test
    public void testIsEndOfMonth_Jan31() {
        assertTrue(calculator.isEndOfMonth(LocalDate.of(2023, 1, 31)));
    }

    @Test
    public void testIsEndOfMonth_Jan30() {
        assertFalse(calculator.isEndOfMonth(LocalDate.of(2023, 1, 30)));
    }

    @Test
    public void testIsEndOfMonth_Feb28_NonLeapYear() {
        assertTrue(calculator.isEndOfMonth(LocalDate.of(2023, 2, 28))); // 2023 is not a leap year
    }

    @Test
    public void testIsEndOfMonth_Feb28_LeapYear() {
        assertFalse(calculator.isEndOfMonth(LocalDate.of(2024, 2, 28))); // 2024 is a leap year
    }

    @Test
    public void testIsEndOfMonth_Feb29_LeapYear() {
        assertTrue(calculator.isEndOfMonth(LocalDate.of(2024, 2, 29))); // 2024 is a leap year
    }

    @Test
    public void testIsEndOfMonth_Feb27_NonLeapYear() {
        assertFalse(calculator.isEndOfMonth(LocalDate.of(2023, 2, 27)));
    }

    @Test
    public void testIsEndOfMonth_Feb27_LeapYear() {
        assertFalse(calculator.isEndOfMonth(LocalDate.of(2024, 2, 27)));
    }

    @Test
    public void testIsEndOfMonth_April30() {
        assertTrue(calculator.isEndOfMonth(LocalDate.of(2023, 4, 30)));
    }

    @Test
    public void testIsEndOfMonth_April29() {
        assertFalse(calculator.isEndOfMonth(LocalDate.of(2023, 4, 29)));
    }
}
