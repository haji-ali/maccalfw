/// A single iCalendar parameter, e.g. `TZID=America/New_York` in
/// `DTSTART;TZID=America/New_York:...`.
public struct ICalParam: Equatable, Sendable, Codable {
    public let name: String
    public let value: String

    public init(_ name: String, _ value: String) {
        self.name = name
        self.value = value
    }
}

/// A single iCalendar property line, e.g. `SUMMARY:Party` or
/// `DTSTART;TZID=America/New_York:20260101T090000`.
///
/// This mirrors the `(NAME PARAMS VALUE)` triple that Emacs's stable
/// `icalendar.el` uses internally (see `icalendar--read-element`), so the
/// elisp encoding of a list of these is directly that shape.
public struct ICalProperty: Equatable, Sendable, Codable {
    public let name: String
    public let params: [ICalParam]
    public let value: String

    public init(_ name: String, params: [ICalParam] = [], value: String) {
        self.name = name
        self.params = params
        self.value = value
    }
}

/// An iCalendar component, e.g. a VEVENT or VTODO, as a flat list of
/// properties. Repeated properties (e.g. multiple RRULE or ATTENDEE
/// lines) simply appear multiple times in `properties`, matching how
/// both `icalendar.el` and maccalfw's own existing internal
/// representation handle them -- no special multi-value wrapping.
public struct ICalComponent: Sendable {
    public let type: String
    public let properties: [ICalProperty]

    public init(type: String, properties: [ICalProperty]) {
        self.type = type
        self.properties = properties
    }
}
