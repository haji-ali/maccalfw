#if canImport(EventKit)
    import EventKit
    import Foundation
    import MacCalfwCore

    func parseISODate(_ s: String) throws -> Date {
        guard let date = ISO8601DateFormatter().date(from: s) else {
            throw CLIError.invalidArgument("Invalid ISO8601 date: \(s)")
        }
        return date
    }

    func resolveCalendars(_ eventStore: EKEventStore, ids: [String]) throws -> [EKCalendar]? {
        guard !ids.isEmpty else { return nil }
        return try ids.map {
            guard let cal = eventStore.calendar(withIdentifier: $0) else {
                throw CLIError.general("Unable to fetch calendar: \($0)")
            }
            return cal
        }
    }

    /// Runs COMMAND against EVENTSTORE and returns the already-encoded
    /// output text (in whichever format ARGUMENTS requested).
    func runCommand(_ command: String, arguments: CLIArguments, eventStore: EKEventStore) throws
        -> String
    {
        let format = arguments.value("format").flatMap(ICalFormat.init(rawValue:)) ?? .icalendar

        switch command {
        case "calendars":
            let type = arguments.value("type") ?? "event"
            var items: [(componentType: String, properties: [ICalProperty])] = []
            if type == "event" || type == "all" {
                do {
                    try authorizeCalendar(eventStore)
                    let defaultId = eventStore.defaultCalendarForNewEvents?.calendarIdentifier
                    for cal in eventStore.calendars(for: .event) {
                        items.append(
                            (
                                "CALENDAR",
                                calendarProperties(
                                    cal, entityType: "event",
                                    isDefault: cal.calendarIdentifier == defaultId)
                            ))
                    }
                } catch CLIError.notAuthorized(let message) where type == "all" {
                    // Missing Reminders access shouldn't stop "all" from
                    // returning the event calendars we do have access to,
                    // but the caller should still be told about it.
                    items.append(("WARNING", [ICalProperty("MESSAGE", value: message)]))
                }
            }
            if type == "reminder" || type == "all" {
                do {
                    try authorizeReminders(eventStore)
                    let defaultId = eventStore.defaultCalendarForNewReminders()?.calendarIdentifier
                    for cal in eventStore.calendars(for: .reminder) {
                        items.append(
                            (
                                "CALENDAR",
                                calendarProperties(
                                    cal, entityType: "reminder",
                                    isDefault: cal.calendarIdentifier == defaultId)
                            ))
                    }
                } catch CLIError.notAuthorized(let message) where type == "all" {
                    // Missing Calendar access shouldn't stop "all" from
                    // returning the reminder calendars we do have access to,
                    // but the caller should still be told about it.
                    items.append(("WARNING", [ICalProperty("MESSAGE", value: message)]))
                }
            }
            return try ICalListEncoder.encode(items, as: format)

        case "events":
            try authorizeCalendar(eventStore)
            guard let startStr = arguments.value("start"), let endStr = arguments.value("end")
            else {
                throw CLIError.invalidArgument("events requires --start and --end")
            }
            let start = try parseISODate(startStr)
            let end = try parseISODate(endStr)
            let calendars = try resolveCalendars(eventStore, ids: arguments.values("calendar"))
            let predicate = eventStore.predicateForEvents(
                withStart: start, end: end, calendars: calendars)
            let events = eventStore.events(matching: predicate)
            return try ICalListEncoder.encode(
                events.map { ("VEVENT", $0.toICalProperties()) }, as: format)

        case "reminders":
            try authorizeReminders(eventStore)
            let calendars = try resolveCalendars(eventStore, ids: arguments.values("calendar"))
            let predicate = eventStore.predicateForReminders(in: calendars)
            let reminders = fetchReminders(eventStore, predicate: predicate)
            let includeCompleted = arguments.flag("include-completed")
            let filtered = includeCompleted ? reminders : reminders.filter { !$0.isCompleted }
            return try ICalListEncoder.encode(
                filtered.map { ("VTODO", $0.toICalProperties()) }, as: format)

        case "update-reminder":
            try authorizeReminders(eventStore)
            let id = arguments.value("id")
            let inputText = String(decoding: FileHandle.standardInput.readDataToEndOfFile(), as: UTF8.self)
            let properties = try ICalDecoder.decode(inputText, as: format)

            let reminder: EKReminder
            if let id {
                reminder = try getEKReminder(eventStore, id: id)
            } else {
                reminder = EKReminder(eventStore: eventStore)
            }
            try reminder.applyICalProperties(properties, eventStore: eventStore)
            do {
                try eventStore.save(reminder, commit: true)
            } catch {
                throw CLIError.general("Failed to save reminder: \(error.localizedDescription)")
            }
            return try ICalEncoder.encode(
                reminder.toICalProperties(), as: format, componentType: "VTODO")

        case "remove-reminder":
            try authorizeReminders(eventStore)
            guard let id = arguments.value("id") else {
                throw CLIError.invalidArgument("remove-reminder requires --id")
            }
            let reminder = try getEKReminder(eventStore, id: id)
            do {
                try eventStore.remove(reminder, commit: true)
            } catch {
                throw CLIError.general("Failed to remove reminder: \(error.localizedDescription)")
            }
            return try ICalEncoder.encode([ICalProperty("REMOVED", value: "yes")], as: format)

        case "event":
            try authorizeCalendar(eventStore)
            guard let id = arguments.value("id") else {
                throw CLIError.invalidArgument("event requires --id")
            }
            let start = try arguments.value("start").map(parseISODate)
            let event = try getEKEvent(eventStore, id: id, start: start)
            return try ICalEncoder.encode(event.toICalProperties(), as: format, componentType: "VEVENT")

        case "update-event":
            try authorizeCalendar(eventStore)
            let id = arguments.value("id")
            let start = try arguments.value("start").map(parseISODate)
            let future = arguments.flag("future")
            let inputText = String(decoding: FileHandle.standardInput.readDataToEndOfFile(), as: UTF8.self)
            let properties = try ICalDecoder.decode(inputText, as: format)

            let event: EKEvent
            if let id {
                event = try getEKEvent(eventStore, id: id, start: start)
            } else {
                event = EKEvent(eventStore: eventStore)
            }
            try event.applyICalProperties(properties, eventStore: eventStore)
            do {
                try eventStore.save(event, span: future ? .futureEvents : .thisEvent, commit: true)
            } catch {
                throw CLIError.general("Failed to save event: \(error.localizedDescription)")
            }
            return try ICalEncoder.encode(event.toICalProperties(), as: format, componentType: "VEVENT")

        case "remove-event":
            try authorizeCalendar(eventStore)
            guard let id = arguments.value("id") else {
                throw CLIError.invalidArgument("remove-event requires --id")
            }
            let start = try arguments.value("start").map(parseISODate)
            let future = arguments.flag("future")
            let event = try getEKEvent(eventStore, id: id, start: start)
            do {
                try eventStore.remove(event, span: future ? .futureEvents : .thisEvent, commit: true)
            } catch {
                throw CLIError.general("Failed to remove event: \(error.localizedDescription)")
            }
            return try ICalEncoder.encode([ICalProperty("REMOVED", value: "yes")], as: format)

        case "timezones":
            let defaultTimeZone = NSTimeZone.default
            let items: [(componentType: String, properties: [ICalProperty])] =
                TimeZone.knownTimeZoneIdentifiers.map { identifier in
                    var props: [ICalProperty] = [ICalProperty("ID", value: identifier)]
                    let timezone =
                        defaultTimeZone.identifier == identifier
                        ? (defaultTimeZone as TimeZone) : TimeZone(identifier: identifier)
                    if let timezone {
                        props.append(
                            ICalProperty(
                                "NAME",
                                value: timezone.localizedName(for: .standard, locale: Locale.current)
                                    ?? ""))
                        props.append(ICalProperty("ABBREV", value: timezone.abbreviation() ?? ""))
                        props.append(
                            ICalProperty("OFFSET", value: String(timezone.secondsFromGMT(for: Date()))))
                    }
                    if defaultTimeZone.identifier == identifier {
                        props.append(ICalProperty("DEFAULT", value: "yes"))
                    }
                    return ("TIMEZONE", props)
                }
            return try ICalListEncoder.encode(items, as: format)

        case "refresh":
            try authorizeCalendar(eventStore)
            eventStore.refreshSourcesIfNecessary()
            return try ICalEncoder.encode([], as: format)

        default:
            throw CLIError.invalidArgument("Unknown command: \(command)")
        }
    }
#endif
