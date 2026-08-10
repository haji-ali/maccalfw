#if canImport(EventKit)
    import EventKit
    import Foundation
    import MacCalfwCore

    extension EKEventAvailability {
        var icalString: String {
            switch self {
            case .tentative: return "TENTATIVE"
            case .free: return "FREE"
            case .busy: return "BUSY"
            case .unavailable: return "UNAVAILABLE"
            case .notSupported: return "NOTSUPPORTED"
            @unknown default: return "BUSY"
            }
        }

        static func from(icalString s: String) throws -> EKEventAvailability {
            switch s.uppercased() {
            case "TENTATIVE": return .tentative
            case "FREE": return .free
            case "BUSY": return .busy
            case "UNAVAILABLE": return .unavailable
            case "NOTSUPPORTED": return .notSupported
            default: throw CLIError.invalidArgument("Unrecognized availability: \(s)")
            }
        }
    }

    extension EKEventStatus {
        var icalString: String {
            switch self {
            case .none: return "NONE"
            case .confirmed: return "CONFIRMED"
            case .tentative: return "TENTATIVE"
            case .canceled: return "CANCELLED"
            @unknown default: return "NONE"
            }
        }
    }

    extension EKEvent {
        /// Builds the flat property list for an event: identity, summary,
        /// location/notes, start/end, timestamps, status, organizer, URL,
        /// availability, calendar linkage, and any recurrence rules.
        func toICalProperties() -> [ICalProperty] {
            var props: [ICalProperty] = []
            func add(_ name: String, _ value: String?) {
                guard let value else { return }
                props.append(ICalProperty(name, value: value))
            }

            add("UID", eventIdentifier)
            add("SUMMARY", title)
            if let location, !location.isEmpty { add("LOCATION", location) }
            if hasNotes, let notes { add("DESCRIPTION", notes) }

            if let startDate {
                let (params, value) = ICalDateFormatting.propertyValue(
                    for: startDate, allDay: isAllDay, timeZoneId: timeZone?.identifier)
                props.append(ICalProperty("DTSTART", params: params, value: value))
            }
            // EKEvent only has one timeZone for the whole event, so DTEND
            // reuses DTSTART's timezone/all-day-ness.
            if let endDate {
                let (params, value) = ICalDateFormatting.propertyValue(
                    for: endDate, allDay: isAllDay, timeZoneId: timeZone?.identifier)
                props.append(ICalProperty("DTEND", params: params, value: value))
            }
            if let creationDate {
                add("DTSTAMP", ICalDateFormatting.utcDateTimeString(creationDate))
            }
            if let lastModifiedDate {
                add("LAST-MODIFIED", ICalDateFormatting.utcDateTimeString(lastModifiedDate))
            }
            add("STATUS", status.icalString)
            add("ORGANIZER", organizer?.name)
            add("URL", url?.absoluteString)
            // TRANSP doesn't support tentative/notSupported.
            add("TRANSP", availability == .free ? "TRANSPARENT" : "OPAQUE")
            add("X-EMACS-AVAILABILITY", availability.icalString)
            add("X-EMACS-CALID", calendar.calendarIdentifier)
            if !calendar.allowsContentModifications {
                add("X-EMACS-READ-ONLY", "yes")
            }
            add("X-EMACS-OCCURENCE-DATE", ICalDateFormatting.utcDateTimeString(occurrenceDate))
            if isDetached {
                add("X-EMACS-DETACHED-P", "yes")
            }
            if !(organizer?.isCurrentUser ?? true) {
                add("X-EMACS-ORG-NOT-CUR-USER", "yes")
            }

            if hasRecurrenceRules, let rules = recurrenceRules {
                for rule in rules {
                    add("RRULE", rule.asRecurrenceRule.icalString())
                }
            }

            return props
        }

        /// Applies whichever properties are present in PROPERTIES; unlisted
        /// fields are left unchanged. Multiple RRULE entries all take
        /// effect: recurrenceRules is reset once and every RRULE value is
        /// added, rather than resetting on each one (which would leave
        /// only the last RRULE in effect).
        func applyICalProperties(_ properties: [ICalProperty], eventStore: EKEventStore) throws {
            var rruleStrings: [String] = []
            for prop in properties {
                switch prop.name {
                case "SUMMARY":
                    title = prop.value
                case "LOCATION":
                    location = prop.value.isEmpty ? nil : prop.value
                case "DESCRIPTION":
                    notes = prop.value
                case "DTSTART":
                    let (date, allDay, tz) = try ICalDateFormatting.date(from: prop)
                    startDate = date
                    isAllDay = allDay
                    timeZone = tz.flatMap { $0.isEmpty ? nil : TimeZone(identifier: $0) }
                case "DTEND":
                    // Only DTEND's instant is used; its timezone/all-day-ness
                    // are ignored (the event's timeZone/isAllDay come from
                    // DTSTART).
                    let (date, _, _) = try ICalDateFormatting.date(from: prop)
                    endDate = date
                case "X-EMACS-AVAILABILITY":
                    availability = try EKEventAvailability.from(icalString: prop.value)
                case "URL":
                    url = prop.value.isEmpty ? nil : URL(string: prop.value)
                case "X-EMACS-CALID":
                    guard let cal = eventStore.calendar(withIdentifier: prop.value) else {
                        throw CLIError.general("Cannot retrieve calendar: \(prop.value)")
                    }
                    calendar = cal
                case "RRULE":
                    rruleStrings.append(prop.value)
                default:
                    // Ignore unhandled keys, as before.
                    break
                }
            }
            if !rruleStrings.isEmpty {
                recurrenceRules = nil
                for ruleString in rruleStrings {
                    addRecurrenceRule(try EKRecurrenceRule(icalRule: RecurrenceRule.parse(ruleString)))
                }
            }
        }
    }

    /// Fetches an event by identifier. If START is given and doesn't
    /// match the fetched event's start date, searches a +/-30 minute
    /// window around it to disambiguate recurring-event instances that
    /// share an identifier.
    func getEKEvent(_ eventStore: EKEventStore, id: String, start: Date?) throws -> EKEvent {
        guard let eventData = eventStore.event(withIdentifier: id) else {
            throw CLIError.general("Failed to fetch event.")
        }
        if let start, eventData.startDate != start {
            let delta: TimeInterval = 60 * 60
            let predicate = eventStore.predicateForEvents(
                withStart: start - delta / 2, end: start + delta / 2, calendars: nil)
            let events = eventStore.events(matching: predicate)
            if let event = events.first(where: { $0.eventIdentifier == id && $0.startDate == start }) {
                return event
            }
        }
        return eventData
    }
#endif
