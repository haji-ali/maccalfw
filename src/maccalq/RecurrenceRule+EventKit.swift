#if canImport(EventKit)
    import EventKit
    import MacCalfwCore

    // Trivial 1:1 mappings -- the actual parsing/formatting logic lives in
    // MacCalfwCore's RecurrenceRule, which is directly tested. This is just
    // the glue between that and EventKit's own enums/types.

    extension EKRecurrenceFrequency {
        var icalString: String {
            switch self {
            case .daily: return "DAILY"
            case .weekly: return "WEEKLY"
            case .monthly: return "MONTHLY"
            case .yearly: return "YEARLY"
            @unknown default: return "DAILY"
            }
        }

        static func from(icalString s: String) throws -> EKRecurrenceFrequency {
            switch s.uppercased() {
            case "DAILY": return .daily
            case "WEEKLY": return .weekly
            case "MONTHLY": return .monthly
            case "YEARLY": return .yearly
            default: throw CLIError.invalidArgument("Unrecognized recurrence frequency: \(s)")
            }
        }
    }

    extension EKWeekday {
        var icalString: String {
            switch self {
            case .sunday: return "SU"
            case .monday: return "MO"
            case .tuesday: return "TU"
            case .wednesday: return "WE"
            case .thursday: return "TH"
            case .friday: return "FR"
            case .saturday: return "SA"
            @unknown default: return "MO"
            }
        }

        static func from(icalString s: String) throws -> EKWeekday {
            switch s {
            case "SU": return .sunday
            case "MO": return .monday
            case "TU": return .tuesday
            case "WE": return .wednesday
            case "TH": return .thursday
            case "FR": return .friday
            case "SA": return .saturday
            default: throw CLIError.invalidArgument("Unrecognized weekday: \(s)")
            }
        }
    }

    extension EKRecurrenceRule {
        var asRecurrenceRule: RecurrenceRule {
            RecurrenceRule(
                frequency: frequency.icalString,
                interval: interval,
                daysOfTheWeek: (daysOfTheWeek ?? []).map {
                    RecurrenceDayOfWeek(
                        weekday: $0.dayOfTheWeek.icalString, weekNumber: $0.weekNumber)
                },
                daysOfTheMonth: (daysOfTheMonth ?? []).map { $0.intValue },
                monthsOfTheYear: (monthsOfTheYear ?? []).map { $0.intValue },
                weeksOfTheYear: (weeksOfTheYear ?? []).map { $0.intValue },
                daysOfTheYear: (daysOfTheYear ?? []).map { $0.intValue },
                setPositions: (setPositions ?? []).map { $0.intValue },
                until: recurrenceEnd?.endDate,
                count: (recurrenceEnd?.occurrenceCount).flatMap { $0 > 0 ? $0 : nil })
        }

        convenience init(icalRule rule: RecurrenceRule) throws {
            let end: EKRecurrenceEnd?
            if let until = rule.until {
                end = EKRecurrenceEnd(end: until)
            } else if let count = rule.count {
                end = EKRecurrenceEnd(occurrenceCount: count)
            } else {
                end = nil
            }
            let daysOfTheWeek: [EKRecurrenceDayOfWeek]? =
                rule.daysOfTheWeek.isEmpty
                ? nil
                : try rule.daysOfTheWeek.map {
                    EKRecurrenceDayOfWeek(
                        try EKWeekday.from(icalString: $0.weekday), weekNumber: $0.weekNumber)
                }
            self.init(
                recurrenceWith: try EKRecurrenceFrequency.from(icalString: rule.frequency),
                interval: rule.interval,
                daysOfTheWeek: daysOfTheWeek,
                daysOfTheMonth: rule.daysOfTheMonth.isEmpty
                    ? nil : rule.daysOfTheMonth.map { NSNumber(value: $0) },
                monthsOfTheYear: rule.monthsOfTheYear.isEmpty
                    ? nil : rule.monthsOfTheYear.map { NSNumber(value: $0) },
                weeksOfTheYear: rule.weeksOfTheYear.isEmpty
                    ? nil : rule.weeksOfTheYear.map { NSNumber(value: $0) },
                daysOfTheYear: rule.daysOfTheYear.isEmpty
                    ? nil : rule.daysOfTheYear.map { NSNumber(value: $0) },
                setPositions: rule.setPositions.isEmpty
                    ? nil : rule.setPositions.map { NSNumber(value: $0) },
                end: end)
        }
    }
#endif
