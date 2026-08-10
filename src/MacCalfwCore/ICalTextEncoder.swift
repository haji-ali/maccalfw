/// Encodes `ICalComponent`s as real RFC5545 iCalendar text (a
/// `BEGIN:VCALENDAR...END:VCALENDAR` block), including line folding and
/// value/parameter escaping.
public enum ICalTextEncoder {
    public static func encode(_ components: [ICalComponent]) -> String {
        var lines: [String] = []
        lines.append("BEGIN:VCALENDAR")
        lines.append("VERSION:2.0")
        lines.append("PRODID:-//maccalfw//EN")
        for component in components {
            lines.append(contentsOf: encodeComponent(component))
        }
        lines.append("END:VCALENDAR")
        return lines.map(fold).joined(separator: "\r\n") + "\r\n"
    }

    static func encodeComponent(_ component: ICalComponent) -> [String] {
        var lines = ["BEGIN:\(component.type)"]
        for prop in component.properties {
            lines.append(encodeProperty(prop))
        }
        lines.append("END:\(component.type)")
        return lines
    }

    static func encodeProperty(_ prop: ICalProperty) -> String {
        var line = prop.name
        for param in prop.params {
            line += ";\(param.name)=\(escapeParamValue(param.value))"
        }
        line += ":\(escapeText(prop.value))"
        return line
    }

    /// RFC5545 3.3.11 TEXT escaping: backslash, semicolon, comma and
    /// newline are backslash-escaped.
    static func escapeText(_ s: String) -> String {
        var out = ""
        out.reserveCapacity(s.count)
        for ch in s {
            switch ch {
            case "\\": out += "\\\\"
            case ";": out += "\\;"
            case ",": out += "\\,"
            case "\n": out += "\\n"
            default: out.append(ch)
            }
        }
        return out
    }

    /// RFC5545 3.2: a param-value containing ':', ';' or ',' must be
    /// double-quoted (and, per the grammar, a quoted param-value cannot
    /// itself contain a double quote).
    static func escapeParamValue(_ s: String) -> String {
        if s.contains(where: { ":;,".contains($0) }) {
            return "\"\(s)\""
        }
        return s
    }

    /// RFC5545 3.1 line folding: no logical line may exceed 75 octets
    /// (excluding the line break itself); longer lines are split with a
    /// CRLF followed by a single leading space, which unfolding
    /// implementations strip back out.
    static func fold(_ line: String) -> String {
        let bytes = Array(line.utf8)
        guard bytes.count > 75 else { return line }
        var result = ""
        var idx = 0
        var first = true
        while idx < bytes.count {
            let chunkSize = first ? 75 : 74
            var end = min(idx + chunkSize, bytes.count)
            // Don't split in the middle of a multi-byte UTF-8 sequence.
            while end < bytes.count && end > idx && (bytes[end] & 0xC0) == 0x80 {
                end -= 1
            }
            result += (first ? "" : "\r\n ") + String(decoding: bytes[idx..<end], as: UTF8.self)
            idx = end
            first = false
        }
        return result
    }
}
