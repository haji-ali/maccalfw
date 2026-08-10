import Foundation

/// Date <-> iCalendar string conversion for DTSTART/DTEND/DUE-style
/// properties (all-day dates, zoned date-times, and bare UTC
/// date-times) and for plain UTC timestamps (DTSTAMP/LAST-MODIFIED/
/// COMPLETED). This has no EventKit dependency -- it only needs to
/// know the (params, value) shape of an `ICalProperty` -- so it's
/// fully testable without a calendar store.
public enum ICalDateFormatting {
    private static func utcFormatter(_ format: String) -> DateFormatter {
        let df = DateFormatter()
        df.locale = Locale(identifier: "en_US_POSIX")
        df.timeZone = TimeZone(secondsFromGMT: 0)
        df.dateFormat = format
        return df
    }

    private static func namedTZFormatter(_ format: String, timeZoneId: String) -> DateFormatter? {
        guard let tz = TimeZone(identifier: timeZoneId) else { return nil }
        let df = DateFormatter()
        df.locale = Locale(identifier: "en_US_POSIX")
        df.timeZone = tz
        df.dateFormat = format
        return df
    }

    /// A plain UTC date-time string with no params, e.g. for DTSTAMP,
    /// LAST-MODIFIED or COMPLETED: "20260101T090000Z".
    public static func utcDateTimeString(_ date: Date) -> String {
        utcFormatter("yyyyMMdd'T'HHmmss'Z'").string(from: date)
    }

    public static func parseUTCDateTimeString(_ s: String) -> Date? {
        utcFormatter("yyyyMMdd'T'HHmmss'Z'").date(from: s)
    }

    /// All-day dates (VALUE=DATE) are calendar-day values, not exact
    /// instants, so they're formatted/parsed in the system's local
    /// timezone rather than UTC (unlike everything else in this file).
    /// EKEvent's start/end for an all-day event aren't necessarily a
    /// clean pair of instants both anchored at local midnight, so
    /// forcing UTC can shift each one across a UTC day boundary
    /// independently of the other, corrupting the calendar-day value
    /// itself rather than just its time-of-day.
    private static func dateOnlyFormatter() -> DateFormatter {
        let df = DateFormatter()
        df.locale = Locale(identifier: "en_US_POSIX")
        df.dateFormat = "yyyyMMdd"
        return df
    }

    /// Formats DATE as a bare "yyyyMMdd" string in the system's local
    /// timezone, matching how `date(from:)` interprets a VALUE=DATE
    /// property.
    public static func dateOnlyString(_ date: Date) -> String {
        dateOnlyFormatter().string(from: date)
    }

    /// The (params, value) pair for a DTSTART/DTEND/DUE-style property:
    /// an all-day date (VALUE=DATE), a date-time in a named zone
    /// (TZID=...), or a bare UTC date-time (no params).
    public static func propertyValue(
        for date: Date, allDay: Bool, timeZoneId: String?
    ) -> (params: [ICalParam], value: String) {
        if allDay {
            return ([ICalParam("VALUE", "DATE")], dateOnlyString(date))
        }
        // No explicit zone means "local time", not UTC: an event at "9am"
        // with no zone info is 9am in the system's current timezone, not
        // 9am UTC, so fall back to that rather than bare UTC.
        let zone = timeZoneId ?? TimeZone.current.identifier
        if let df = namedTZFormatter("yyyyMMdd'T'HHmmss", timeZoneId: zone) {
            return ([ICalParam("TZID", zone)], df.string(from: date))
        }
        return ([], utcDateTimeString(date))
    }

    /// The inverse of `propertyValue(for:allDay:timeZoneId:)`: reads
    /// the params/value off an `ICalProperty` (e.g. DTSTART) and
    /// returns the decoded date, whether it's all-day, and the
    /// timezone identifier if one was given.
    public static func date(from property: ICalProperty) throws -> (
        date: Date, isAllDay: Bool, timeZoneId: String?
    ) {
        if let valueParam = property.params.first(where: { $0.name == "VALUE" }),
            valueParam.value == "DATE"
        {
            guard let date = dateOnlyFormatter().date(from: property.value) else {
                throw CLIError.invalidArgument(
                    "Invalid all-day date in \(property.name): \(property.value)")
            }
            return (date, true, nil)
        }
        if let tzidParam = property.params.first(where: { $0.name == "TZID" }) {
            guard let df = namedTZFormatter("yyyyMMdd'T'HHmmss", timeZoneId: tzidParam.value),
                let date = df.date(from: property.value)
            else {
                throw CLIError.invalidArgument(
                    "Invalid date in \(property.name) for timezone \(tzidParam.value): \(property.value)"
                )
            }
            return (date, false, tzidParam.value)
        }
        guard let date = parseUTCDateTimeString(property.value) else {
            throw CLIError.invalidArgument("Invalid date in \(property.name): \(property.value)")
        }
        return (date, false, nil)
    }
}
