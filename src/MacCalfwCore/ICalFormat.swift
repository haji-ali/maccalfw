/// The three output formats the CLI supports for a component's
/// properties. Each is produced directly from the same `[ICalProperty]`
/// -- none is derived by parsing another, so there is no reparsing step
/// and no risk of drift between formats.
public enum ICalFormat: String, CaseIterable, Sendable {
    case icalendar
    case json
    case elisp
}

public enum ICalEncoder {
    public static func encode(
        _ properties: [ICalProperty],
        as format: ICalFormat,
        componentType: String = "VEVENT"
    ) throws -> String {
        switch format {
        case .icalendar:
            return ICalTextEncoder.encode([ICalComponent(type: componentType, properties: properties)])
        case .json:
            return try ICalJSONEncoder.encode(properties)
        case .elisp:
            return ICalElispEncoder.encode(properties)
        }
    }
}

public enum ICalDecodeError: Error, CustomStringConvertible {
    case notYetImplemented(ICalFormat)

    public var description: String {
        switch self {
        case .notYetImplemented(let format):
            return "Decoding input as \(format.rawValue) is not yet implemented; use json or elisp"
        }
    }
}

public enum ICalDecoder {
    /// Decodes properties previously encoded by `ICalEncoder.encode(_:as:)`.
    /// Note: unlike encoding, decoding icalendar text requires an actual
    /// RFC5545 parser, which doesn't exist yet -- only json and elisp
    /// are supported as input formats for now.
    public static func decode(_ text: String, as format: ICalFormat) throws -> [ICalProperty] {
        switch format {
        case .icalendar:
            throw ICalDecodeError.notYetImplemented(format)
        case .json:
            return try ICalJSONDecoder.decode(text)
        case .elisp:
            return try ICalElispDecoder.decode(text)
        }
    }
}
