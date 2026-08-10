import Foundation

/// Encodes a list of items (e.g. the results of `events`/`reminders`),
/// each with its own component type (VEVENT/VTODO) and properties, in
/// one of the three formats. Like `ICalEncoder`, each format is
/// produced directly from the same list, not derived from another.
public enum ICalListEncoder {
    public static func encode(
        _ items: [(componentType: String, properties: [ICalProperty])],
        as format: ICalFormat
    ) throws -> String {
        switch format {
        case .icalendar:
            return ICalTextEncoder.encode(
                items.map { ICalComponent(type: $0.componentType, properties: $0.properties) })
        case .json:
            let encoder = JSONEncoder()
            encoder.outputFormatting = [.sortedKeys]
            let data = try encoder.encode(items.map { $0.properties })
            return String(decoding: data, as: UTF8.self)
        case .elisp:
            let inner = items.map { ICalElispEncoder.encode($0.properties) }
                .joined(separator: "\n ")
            return "(\(inner))"
        }
    }
}
