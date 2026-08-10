import XCTest

@testable import MacCalfwCore

final class RecurrenceRuleTests: XCTestCase {
    func testSimpleWeeklyRoundTrips() throws {
        let rule = RecurrenceRule(
            frequency: "WEEKLY", interval: 1,
            daysOfTheWeek: [RecurrenceDayOfWeek(weekday: "MO")])
        let s = rule.icalString()
        XCTAssertEqual(s, "INTERVAL=1;FREQ=WEEKLY;WKST=2;BYDAY=MO")
        XCTAssertEqual(try RecurrenceRule.parse(s), rule)
    }

    func testDailyWithNoExtrasRoundTrips() throws {
        let rule = RecurrenceRule(frequency: "DAILY", interval: 2)
        XCTAssertEqual(try RecurrenceRule.parse(rule.icalString()), rule)
    }

    func testUntilRoundTrips() throws {
        let until = Date(timeIntervalSince1970: 1_767_258_000)  // 2026-01-01T09:00:00Z
        let rule = RecurrenceRule(frequency: "DAILY", until: until)
        let s = rule.icalString()
        XCTAssertTrue(s.contains("UNTIL=20260101T090000Z"))
        let parsed = try RecurrenceRule.parse(s)
        XCTAssertEqual(parsed.until?.timeIntervalSince1970, until.timeIntervalSince1970)
    }

    func testCountRoundTrips() throws {
        let rule = RecurrenceRule(frequency: "DAILY", count: 5)
        let s = rule.icalString()
        XCTAssertTrue(s.contains("COUNT=5"))
        XCTAssertEqual(try RecurrenceRule.parse(s).count, 5)
    }

    func testZeroCountIsOmitted() {
        // Matches the ported behavior: COUNT is only meaningful if > 0.
        let rule = RecurrenceRule(frequency: "DAILY", count: 0)
        XCTAssertFalse(rule.icalString().contains("COUNT"))
    }

    func testOrdinalByDayRoundTrips() throws {
        let rule = RecurrenceRule(
            frequency: "MONTHLY",
            daysOfTheWeek: [
                RecurrenceDayOfWeek(weekday: "FR", weekNumber: 2),
                RecurrenceDayOfWeek(weekday: "SU", weekNumber: -1),
            ])
        let s = rule.icalString()
        XCTAssertTrue(s.contains("BYDAY=2FR,-1SU"))
        XCTAssertEqual(try RecurrenceRule.parse(s), rule)
    }

    func testAllByFieldsRoundTrip() throws {
        let rule = RecurrenceRule(
            frequency: "YEARLY", interval: 3,
            daysOfTheMonth: [1, 15], monthsOfTheYear: [1, 6],
            weeksOfTheYear: [10], daysOfTheYear: [100, 200],
            setPositions: [-1])
        XCTAssertEqual(try RecurrenceRule.parse(rule.icalString()), rule)
    }

    func testParseRejectsNonMondayWeekStart() {
        XCTAssertThrowsError(try RecurrenceRule.parse("FREQ=WEEKLY;WKST=1")) { error in
            XCTAssertEqual(
                error as? RecurrenceRuleError, .unsupportedWeekStart("1"))
        }
    }

    func testParseAcceptsMondayWeekStart() throws {
        let rule = try RecurrenceRule.parse("FREQ=WEEKLY;WKST=2")
        XCTAssertEqual(rule.frequency, "WEEKLY")
    }

    func testParseRejectsInvalidWeekday() {
        XCTAssertThrowsError(try RecurrenceRule.parse("FREQ=WEEKLY;BYDAY=XX"))
    }

    func testParseRejectsUnknownKey() {
        XCTAssertThrowsError(try RecurrenceRule.parse("FREQ=WEEKLY;BOGUS=1"))
    }

    func testParseRejectsNonIntegerInterval() {
        XCTAssertThrowsError(try RecurrenceRule.parse("FREQ=WEEKLY;INTERVAL=abc"))
    }

    func testParseUppercasesFrequency() throws {
        let rule = try RecurrenceRule.parse("FREQ=weekly")
        XCTAssertEqual(rule.frequency, "WEEKLY")
    }
}
