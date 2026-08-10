import Foundation

/// Encodes `ICalProperty` lists as JSON, using the same
/// name/params/value shape as `ICalElispEncoder`, so all three output
/// formats carry identical information -- just printed differently.
public enum ICalJSONEncoder {
    public static func encode(_ properties: [ICalProperty]) throws -> String {
        let encoder = JSONEncoder()
        encoder.outputFormatting = [.sortedKeys]
        let data = try encoder.encode(properties)
        return String(decoding: data, as: UTF8.self)
    }
}

public enum ICalJSONDecoder {
    public static func decode(_ text: String) throws -> [ICalProperty] {
        try JSONDecoder().decode([ICalProperty].self, from: Data(text.utf8))
    }
}
