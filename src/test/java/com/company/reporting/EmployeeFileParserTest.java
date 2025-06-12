package com.company.reporting;

import org.junit.Test;
import java.io.IOException;
import java.time.LocalDate;
import java.util.List;
import static org.junit.Assert.*;

public class EmployeeFileParserTest {

    private final EmployeeFileParser parser = new EmployeeFileParser();

    @Test
    public void testParseFile_Successful() throws IOException {
        List<EmployeeRecord> records = parser.parseFile("src/test/resources/test_empfile.dat");
        // All 11 lines in the revised test_empfile.dat are 53 chars.
        assertEquals("Expected 11 records from test_empfile.dat", 11, records.size());

        EmployeeRecord first = records.get(0);
        assertEquals("John Smith", first.rawEmpName);
        assertEquals("20200115", first.rawHireDate);
        assertEquals("H", first.rawEmpType);
        assertEquals("1", first.rawEmpRegionCode); // Trimmed
                assertEquals("00000000", first.rawSalaryOrBasePay); // Adjusted to 8 zeros
                assertEquals("00000000", first.rawHoursOrSalesAmount); // Adjusted to 8 zeros
                assertEquals("0000000", first.rawRate); // Adjusted to 7 zeros, due to line length issue

        EmployeeRecord fifth = records.get(4); // "Invalid Emp"
        assertEquals("Invalid Emp", fifth.rawEmpName);
        assertEquals("X", fifth.rawEmpType); // This is raw, so 'X' is expected
        assertEquals("5", fifth.rawEmpRegionCode); // This is raw, so '5' is expected

        EmployeeRecord last = records.get(10); // "Extra Line"
        assertEquals("Extra Line", last.rawEmpName);
        assertEquals("H", last.rawEmpType);
    }

    @Test(expected = IOException.class)
    public void testParseFile_NotFound() throws IOException {
        parser.parseFile("src/test/resources/non_existent_file.dat"); // Adjusted path for consistency
    }

    @Test
    public void testParseDate_Valid() {
        assertEquals(LocalDate.of(2020, 1, 15), EmployeeFileParser.parseDate("20200115", 1));
    }

    @Test
    public void testParseDate_Null() {
        // This test was causing an NPE. The code has a null check.
        // If it still fails, it points to a deeper issue.
        assertNull("Parsing a null date string should return null", EmployeeFileParser.parseDate(null, 0));
    }

    @Test
    public void testParseEmployeeType_Valid() {
        assertEquals(EmployeeType.HOURLY, EmployeeFileParser.parseEmployeeType("H",1));
    }

    @Test
    public void testParseEmployeeType_Null() {
        assertEquals(EmployeeType.UNKNOWN, EmployeeFileParser.parseEmployeeType(null,1));
    }

    @Test
    public void testParseRegion_Valid() {
        assertEquals(Region.NORTH, EmployeeFileParser.parseRegion("1",1));
        assertEquals(Region.SOUTH, EmployeeFileParser.parseRegion("2 ",1));
    }

    @Test
    public void testParseRegion_Null() {
         assertEquals(Region.INVALID, EmployeeFileParser.parseRegion(null,1));
    }

    @Test
    public void testParseDouble_Valid() {
        assertEquals(123.45, EmployeeFileParser.parseDouble("123.45", "test",1), 0.001);
    }

    @Test
    public void testParseDouble_Null() {
        assertEquals(0.0, EmployeeFileParser.parseDouble(null, "test",1), 0.001);
    }
}
