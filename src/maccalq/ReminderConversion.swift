#if canImport(EventKit)
    import EventKit
    import Foundation
    import MacCalfwCore

    extension EKReminder {
        /// Reminders (VTODO) as a flat property list, analogous to
        /// EKEvent.toICalProperties() but with VTODO-shaped fields: DUE
        /// instead of DTSTART/DTEND, COMPLETED/STATUS/PRIORITY instead of
        /// DTSTAMP/STATUS/availability.
        func toICalProperties() -> [ICalProperty] {
            var props: [ICalProperty] = []
            func add(_ name: String, _ value: String?) {
                guard let value else { return }
                props.append(ICalProperty(name, value: value))
            }

            add("UID", calendarItemIdentifier)
            add("SUMMARY", title)
            if let location, !location.isEmpty { add("LOCATION", location) }
            if hasNotes, let notes { add("DESCRIPTION", notes) }

            if let due = dueDateComponents, let date = Self.date(from: due) {
                // A DUE with no time-of-day is an all-day due date, same
                // convention as all-day DTSTART/DTEND.
                let allDay = due.hour == nil
                let (params, value) = ICalDateFormatting.propertyValue(
                    for: date, allDay: allDay, timeZoneId: due.timeZone?.identifier)
                props.append(ICalProperty("DUE", params: params, value: value))
            }
            if isCompleted, let completionDate {
                add("COMPLETED", ICalDateFormatting.utcDateTimeString(completionDate))
            }
            // RFC5545 VTODO status values, not VEVENT's.
            add("STATUS", isCompleted ? "COMPLETED" : "NEEDS-ACTION")
            if priority != 0 {
                add("PRIORITY", String(priority))
            }
            add("URL", url?.absoluteString)
            add("X-EMACS-CALID", calendar.calendarIdentifier)
            if !calendar.allowsContentModifications {
                add("X-EMACS-READ-ONLY", "yes")
            }

            if hasRecurrenceRules, let rules = recurrenceRules {
                for rule in rules {
                    add("RRULE", rule.asRecurrenceRule.icalString())
                }
            }

            return props
        }

        /// `DateComponents.date` returns nil unless a `.calendar` has
        /// been set on the components, which isn't documented as
        /// something EventKit guarantees on `dueDateComponents` -- so
        /// this builds the Date explicitly from an EKReminder's own
        /// timeZone (or the current calendar's) instead of relying on
        /// that.
        static func date(from components: DateComponents) -> Date? {
            var calendar = Calendar(identifier: .gregorian)
            if let timeZone = components.timeZone {
                calendar.timeZone = timeZone
            }
            return calendar.date(from: components)
        }

        /// The inverse of `date(from:)` above: builds the DateComponents
        /// `dueDateComponents` expects from a DUE property, matching how
        /// `toICalProperties()` reads one back -- no `.hour` component
        /// means an all-day due date.
        static func dateComponents(fromDue property: ICalProperty) throws -> DateComponents {
            let (date, isAllDay, timeZoneId) = try ICalDateFormatting.date(from: property)
            var calendar = Calendar(identifier: .gregorian)
            let timeZone = timeZoneId.flatMap { TimeZone(identifier: $0) }
            if let timeZone { calendar.timeZone = timeZone }
            var components = calendar.dateComponents(
                isAllDay
                    ? [.year, .month, .day]
                    : [.year, .month, .day, .hour, .minute, .second],
                from: date)
            components.timeZone = isAllDay ? nil : timeZone
            return components
        }

        /// Applies whichever properties are present in PROPERTIES; unlisted
        /// fields are left unchanged.
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
                case "DUE":
                    dueDateComponents = try Self.dateComponents(fromDue: prop)
                case "STATUS":
                    isCompleted = prop.value.uppercased() == "COMPLETED"
                case "URL":
                    url = prop.value.isEmpty ? nil : URL(string: prop.value)
                case "PRIORITY":
                    priority = Int(prop.value) ?? 0
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

    /// Fetches a reminder by identifier.
    func getEKReminder(_ eventStore: EKEventStore, id: String) throws -> EKReminder {
        guard let item = eventStore.calendarItem(withIdentifier: id) as? EKReminder else {
            throw CLIError.general("Failed to fetch reminder.")
        }
        return item
    }

    /// EKEventStore.fetchReminders(matching:completion:) is
    /// callback-based; block synchronously since the CLI is a one-shot
    /// process with no other work to interleave.
    func fetchReminders(_ eventStore: EKEventStore, predicate: NSPredicate) -> [EKReminder] {
        let semaphore = DispatchSemaphore(value: 0)
        var result: [EKReminder] = []
        _ = eventStore.fetchReminders(matching: predicate) { reminders in
            result = reminders ?? []
            semaphore.signal()
        }
        semaphore.wait()
        return result
    }
#endif
