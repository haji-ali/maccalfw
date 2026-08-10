import Foundation

/// One BYDAY entry, e.g. "MO" or "2FR" (2nd Friday) or "-1SU" (last
/// Sunday).
public struct RecurrenceDayOfWeek: Equatable, Sendable {
    /// Two-letter iCal weekday abbreviation: SU, MO, TU, WE, TH, FR, SA.
    public let weekday: String
    /// Ordinal within the period (e.g. 2 for "2nd Monday", -1 for
    /// "last Friday"); 0 if unspecified.
    public let weekNumber: Int

    public init(weekday: String, weekNumber: Int = 0) {
        self.weekday = weekday
        self.weekNumber = weekNumber
    }
}

public enum RecurrenceRuleError: Error, CustomStringConvertible, Equatable {
    case invalidFormat(String)
    case unsupportedWeekStart(String)

    public var description: String {
        switch self {
        case .invalidFormat(let s): return "Invalid recurrence rule format: \(s)"
        case .unsupportedWeekStart(let s): return "Cannot handle non-standard week start: \(s)"
        }
    }
}

/// A parsed RRULE value (the part after "RRULE:" -- this type doesn't
/// handle the property name/params, just the semicolon-separated
/// FREQ=...;INTERVAL=...;... value itself), independent of EventKit's
/// `EKRecurrenceRule` so the parsing/formatting logic is directly
/// testable. The EventKit-side conversion to/from `EKRecurrenceRule`
/// is then a trivial 1:1 field mapping.
public struct RecurrenceRule: Equatable, Sendable {
    public static let weekdays = ["SU", "MO", "TU", "WE", "TH", "FR", "SA"]

    public var frequency: String  // DAILY, WEEKLY, MONTHLY, YEARLY
    public var interval: Int
    public var daysOfTheWeek: [RecurrenceDayOfWeek]
    public var daysOfTheMonth: [Int]
    public var monthsOfTheYear: [Int]
    public var weeksOfTheYear: [Int]
    public var daysOfTheYear: [Int]
    public var setPositions: [Int]
    public var until: Date?
    public var count: Int?

    public init(
        frequency: String,
        interval: Int = 1,
        daysOfTheWeek: [RecurrenceDayOfWeek] = [],
        daysOfTheMonth: [Int] = [],
        monthsOfTheYear: [Int] = [],
        weeksOfTheYear: [Int] = [],
        daysOfTheYear: [Int] = [],
        setPositions: [Int] = [],
        until: Date? = nil,
        count: Int? = nil
    ) {
        self.frequency = frequency
        self.interval = interval
        self.daysOfTheWeek = daysOfTheWeek
        self.daysOfTheMonth = daysOfTheMonth
        self.monthsOfTheYear = monthsOfTheYear
        self.weeksOfTheYear = weeksOfTheYear
        self.daysOfTheYear = daysOfTheYear
        self.setPositions = setPositions
        self.until = until
        self.count = count
    }

    /// Renders as an RRULE value string, e.g. "FREQ=WEEKLY;BYDAY=MO".
    ///
    /// WKST is always emitted as "WKST=2" (Monday); `parse` only
    /// accepts that value back and rejects anything else as
    /// unsupported. Non-Monday week starts aren't supported.
    public func icalString() -> String {
        var parts: [String] = []
        if let until { parts.append("UNTIL=\(ICalDateFormatting.utcDateTimeString(until))") }
        if let count, count > 0 { parts.append("COUNT=\(count)") }
        parts.append("INTERVAL=\(interval)")
        parts.append("FREQ=\(frequency.uppercased())")
        parts.append("WKST=2")
        if !daysOfTheWeek.isEmpty {
            parts.append("BYDAY=" + daysOfTheWeek.map(Self.encodeDayOfWeek).joined(separator: ","))
        }
        if !daysOfTheMonth.isEmpty {
            parts.append("BYMONTHDAY=" + daysOfTheMonth.map(String.init).joined(separator: ","))
        }
        if !daysOfTheYear.isEmpty {
            parts.append("BYYEARDAY=" + daysOfTheYear.map(String.init).joined(separator: ","))
        }
        if !weeksOfTheYear.isEmpty {
            parts.append("BYWEEKNO=" + weeksOfTheYear.map(String.init).joined(separator: ","))
        }
        if !monthsOfTheYear.isEmpty {
            parts.append("BYMONTH=" + monthsOfTheYear.map(String.init).joined(separator: ","))
        }
        if !setPositions.isEmpty {
            parts.append("BYSETPOS=" + setPositions.map(String.init).joined(separator: ","))
        }
        return parts.joined(separator: ";")
    }

    private static func encodeDayOfWeek(_ d: RecurrenceDayOfWeek) -> String {
        d.weekNumber != 0 ? "\(d.weekNumber)\(d.weekday)" : d.weekday
    }

    public static func parse(_ s: String) throws -> RecurrenceRule {
        let fields: [[String]] = s.components(separatedBy: ";").map { component in
            let keyValue = component.split(separator: "=", maxSplits: 1).map(String.init)
            guard keyValue.count == 2 else { return keyValue }
            return [keyValue[0]] + keyValue[1].split(separator: ",").map(String.init)
        }

        var frequency = "DAILY"
        var interval = 1
        var daysOfTheWeek: [RecurrenceDayOfWeek] = []
        var daysOfTheMonth: [Int] = []
        var monthsOfTheYear: [Int] = []
        var weeksOfTheYear: [Int] = []
        var daysOfTheYear: [Int] = []
        var setPositions: [Int] = []
        var until: Date?
        var count: Int?

        for field in fields {
            guard let key = field.first else { continue }
            switch key {
            case "FREQ":
                guard field.count > 1 else { throw RecurrenceRuleError.invalidFormat(s) }
                frequency = field[1].uppercased()
            case "UNTIL":
                guard field.count > 1,
                    let date = ICalDateFormatting.parseUTCDateTimeString(field[1])
                else { throw RecurrenceRuleError.invalidFormat(s) }
                until = date
            case "BYDAY":
                daysOfTheWeek = try field.dropFirst().map(parseDayOfWeek)
            case "WKST":
                guard field.count > 1 else { throw RecurrenceRuleError.invalidFormat(s) }
                guard field[1] == "2" else {
                    throw RecurrenceRuleError.unsupportedWeekStart(field[1])
                }
            case "COUNT":
                count = try parseInt(field, whole: s)
            case "INTERVAL":
                interval = try parseInt(field, whole: s)
            case "BYMONTHDAY":
                daysOfTheMonth = try field.dropFirst().map { try parseInt($0, whole: s) }
            case "BYMONTH":
                monthsOfTheYear = try field.dropFirst().map { try parseInt($0, whole: s) }
            case "BYWEEKNO":
                weeksOfTheYear = try field.dropFirst().map { try parseInt($0, whole: s) }
            case "BYYEARDAY":
                daysOfTheYear = try field.dropFirst().map { try parseInt($0, whole: s) }
            case "BYSETPOS":
                setPositions = try field.dropFirst().map { try parseInt($0, whole: s) }
            default:
                throw RecurrenceRuleError.invalidFormat(s)
            }
        }

        return RecurrenceRule(
            frequency: frequency, interval: interval, daysOfTheWeek: daysOfTheWeek,
            daysOfTheMonth: daysOfTheMonth, monthsOfTheYear: monthsOfTheYear,
            weeksOfTheYear: weeksOfTheYear, daysOfTheYear: daysOfTheYear,
            setPositions: setPositions, until: until, count: count)
    }

    private static func parseInt(_ field: [String], whole s: String) throws -> Int {
        guard field.count > 1 else { throw RecurrenceRuleError.invalidFormat(s) }
        return try parseInt(field[1], whole: s)
    }

    private static func parseInt(_ value: String, whole s: String) throws -> Int {
        guard let n = Int(value) else { throw RecurrenceRuleError.invalidFormat(s) }
        return n
    }

    private static func parseDayOfWeek(_ s: String) throws -> RecurrenceDayOfWeek {
        // Optional leading signed integer ordinal, then a two-letter weekday.
        var chars = Substring(s)
        var digits = ""
        if chars.first == "-" || chars.first == "+" {
            digits.append(chars.removeFirst())
        }
        while let c = chars.first, c.isNumber {
            digits.append(chars.removeFirst())
        }
        let weekday = String(chars)
        guard weekdays.contains(weekday) else {
            throw RecurrenceRuleError.invalidFormat(s)
        }
        let weekNumber = digits.isEmpty ? 0 : (Int(digits) ?? 0)
        return RecurrenceDayOfWeek(weekday: weekday, weekNumber: weekNumber)
    }
}
