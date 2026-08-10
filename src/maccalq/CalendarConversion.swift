#if canImport(EventKit)
    import Cocoa
    import EventKit
    import MacCalfwCore

    extension NSColor {
        var icalHexString: String? {
            guard let color = usingColorSpace(.sRGB) else { return nil }
            let red = Int(color.redComponent * 255)
            let green = Int(color.greenComponent * 255)
            let blue = Int(color.blueComponent * 255)
            return String(format: "#%02X%02X%02X", red, green, blue)
        }
    }

    /// Calendars and reminder lists aren't iCalendar components (no
    /// RFC5545 VEVENT/VTODO concept applies), but they go through the
    /// same triple-based property shape as everything else for a
    /// consistent wire format: ID/TITLE/COLOR/EDITABLE/DEFAULT, plus TYPE
    /// ("event" or "reminder") so a combined `--type all` listing can
    /// tell them apart.
    func calendarProperties(
        _ calendar: EKCalendar, entityType: String, isDefault: Bool
    ) -> [ICalProperty] {
        var props: [ICalProperty] = [
            ICalProperty("ID", value: calendar.calendarIdentifier),
            ICalProperty("TITLE", value: calendar.title),
            ICalProperty("TYPE", value: entityType),
        ]
        // EKCalendar.color is `NSColor!` -- declared implicitly-unwrapped
        // but genuinely nilable in practice for some calendar sources, so
        // this uses optional chaining rather than an implicit force
        // unwrap.
        if let hex = calendar.color?.icalHexString {
            props.append(ICalProperty("COLOR", value: hex))
        }
        if calendar.allowsContentModifications {
            props.append(ICalProperty("EDITABLE", value: "yes"))
        }
        if isDefault {
            props.append(ICalProperty("DEFAULT", value: "yes"))
        }
        return props
    }
#endif
