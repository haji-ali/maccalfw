import XCTest

@testable import MacCalfwCore

final class ICalDateFormattingTests: XCTestCase {
    // 2026-01-01 09:00:00 UTC
    static let sampleDate = Date(timeIntervalSince1970: 1_767_258_000)

    func testUTCDateTimeStringRoundTrips() {
        let s = ICalDateFormatting.utcDateTimeString(Self.sampleDate)
        XCTAssertEqual(s, "20260101T090000Z")
        XCTAssertEqual(
            ICalDateFormatting.parseUTCDateTimeString(s)?.timeIntervalSince1970,
            Self.sampleDate.timeIntervalSince1970)
    }

    func testAllDayPropertyValueUsesValueDateParam() throws {
        let (params, value) = ICalDateFormatting.propertyValue(
            for: Self.sampleDate, allDay: true, timeZoneId: nil)
        XCTAssertEqual(params, [ICalParam("VALUE", "DATE")])
        // All-day dates are calendar-day values in the *local* timezone
        // (see testAllDayPropertyValueUsesLocalTimezoneNotUTC for why),
        // so the expected string has to be computed the same way to
        // stay correct regardless of the test machine's own timezone.
        XCTAssertEqual(value, ICalDateFormatting.dateOnlyString(Self.sampleDate))

        let prop = ICalProperty("DTSTART", params: params, value: value)
        let decoded = try ICalDateFormatting.date(from: prop)
        XCTAssertTrue(decoded.isAllDay)
        XCTAssertNil(decoded.timeZoneId)
        XCTAssertEqual(
            ICalDateFormatting.dateOnlyString(decoded.date),
            ICalDateFormatting.dateOnlyString(Self.sampleDate))
    }

    func testAllDayPropertyValueUsesLocalTimezoneNotUTC() throws {
        // Regression test for a real bug: forcing UTC for all-day
        // dates shifted an event's start date backward by a day for
        // anyone in a positive-UTC-offset zone (e.g. BST, UTC+1),
        // because local midnight falls in the *previous* UTC calendar
        // day. All-day dates must be formatted/parsed in the local
        // timezone instead, matching the calendar day EventKit means.
        let original = NSTimeZone.default
        defer { NSTimeZone.default = original }
        NSTimeZone.default = TimeZone(identifier: "Europe/London")!

        // 2026-08-15 00:00:00 BST == 2026-08-14 23:00:00 UTC.
        let localMidnight = Date(timeIntervalSince1970: 1_786_748_400)
        let (params, value) = ICalDateFormatting.propertyValue(
            for: localMidnight, allDay: true, timeZoneId: nil)
        XCTAssertEqual(params, [ICalParam("VALUE", "DATE")])
        XCTAssertEqual(value, "20260815")

        // And the inverse: parsing "20260815" back must land on the
        // same local calendar day, not shift again.
        let prop = ICalProperty("DTSTART", params: params, value: "20260815")
        let decoded = try ICalDateFormatting.date(from: prop)
        XCTAssertEqual(ICalDateFormatting.dateOnlyString(decoded.date), "20260815")
    }

    func testZonedPropertyValueUsesTZIDParam() throws {
        let (params, value) = ICalDateFormatting.propertyValue(
            for: Self.sampleDate, allDay: false, timeZoneId: "America/New_York")
        XCTAssertEqual(params, [ICalParam("TZID", "America/New_York")])
        // 09:00 UTC == 04:00 EST (UTC-5) on this date.
        XCTAssertEqual(value, "20260101T040000")

        let prop = ICalProperty("DTSTART", params: params, value: value)
        let decoded = try ICalDateFormatting.date(from: prop)
        XCTAssertFalse(decoded.isAllDay)
        XCTAssertEqual(decoded.timeZoneId, "America/New_York")
        XCTAssertEqual(
            decoded.date.timeIntervalSince1970, Self.sampleDate.timeIntervalSince1970)
    }

    func testMissingTimeZoneFallsBackToSystemLocalNotUTC() throws {
        // No explicit zone means "local time" (e.g. a "9am" event with no
        // zone info is 9am here, not 9am UTC), so this must NOT produce
        // a bare Z-suffixed UTC value.
        let (params, value) = ICalDateFormatting.propertyValue(
            for: Self.sampleDate, allDay: false, timeZoneId: nil)
        XCTAssertEqual(params, [ICalParam("TZID", TimeZone.current.identifier)])
        XCTAssertFalse(value.hasSuffix("Z"))

        let prop = ICalProperty("DTSTART", params: params, value: value)
        let decoded = try ICalDateFormatting.date(from: prop)
        XCTAssertFalse(decoded.isAllDay)
        XCTAssertEqual(decoded.timeZoneId, TimeZone.current.identifier)
        XCTAssertEqual(
            decoded.date.timeIntervalSince1970, Self.sampleDate.timeIntervalSince1970)
    }

    func testBareUTCStillDecodesWhenNoParamsPresent() throws {
        // Bare Z-suffixed UTC values (e.g. from other tools, or older
        // data) must still decode correctly, even though this package no
        // longer produces them itself.
        let prop = ICalProperty("DTSTART", value: "20260101T090000Z")
        let decoded = try ICalDateFormatting.date(from: prop)
        XCTAssertFalse(decoded.isAllDay)
        XCTAssertNil(decoded.timeZoneId)
        XCTAssertEqual(
            decoded.date.timeIntervalSince1970, Self.sampleDate.timeIntervalSince1970)
    }

    func testInvalidAllDayDateThrows() {
        let prop = ICalProperty(
            "DTSTART", params: [ICalParam("VALUE", "DATE")], value: "not-a-date")
        XCTAssertThrowsError(try ICalDateFormatting.date(from: prop))
    }

    func testInvalidTimeZoneIdentifierThrows() {
        let prop = ICalProperty(
            "DTSTART", params: [ICalParam("TZID", "Not/A/Zone")], value: "20260101T090000")
        XCTAssertThrowsError(try ICalDateFormatting.date(from: prop))
    }

    func testInvalidBareUTCStringThrows() {
        let prop = ICalProperty("DTSTART", value: "garbage")
        XCTAssertThrowsError(try ICalDateFormatting.date(from: prop))
    }
}
