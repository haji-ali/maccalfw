#if canImport(EventKit)
    import EventKit
    import Foundation
    import MacCalfwCore

    extension EKReminder {
        /// Reminders (VTODO) as a flat property list, analogous to
        /// EKEvent.toICalProperties() but with VTODO-shaped fields: DUE
        /// instead of DTSTART/DTEND, COMPLETED/STATUS/PRIORITY instead of
        /// DTSTAMP/STATUS/availability. Read-only: there is no
        /// applyICalProperties for reminders.
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
