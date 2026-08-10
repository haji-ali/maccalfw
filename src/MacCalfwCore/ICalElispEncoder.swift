/// Encodes `ICalProperty` lists as the elisp sexp shape used by Emacs's
/// stable `icalendar.el` internals (`icalendar--read-element`,
/// `icalendar--get-event-property`): a list of `(NAME PARAMS VALUE)`
/// triples, where PARAMS is nil or a flat list alternating param-name
/// symbols and param-value strings, e.g.:
///
///   ((SUMMARY nil "Party")
///    (DTSTART (TZID "America/New_York") "20260101T090000"))
public enum ICalElispEncoder {
    public static func encode(_ properties: [ICalProperty]) -> String {
        "(" + properties.map(encodeProperty).joined(separator: "\n ") + ")"
    }

    static func encodeProperty(_ prop: ICalProperty) -> String {
        let paramsSexp =
            prop.params.isEmpty
            ? "nil"
            : "(" + prop.params.map { "\($0.name) \(sexpString($0.value))" }
                .joined(separator: " ") + ")"
        return "(\(prop.name) \(paramsSexp) \(sexpString(prop.value)))"
    }

    /// Print STRING as an elisp string literal (backslash and
    /// double-quote escaped; everything else -- including newlines --
    /// is passed through as-is, exactly as `prin1` would print it).
    static func sexpString(_ s: String) -> String {
        var out = "\""
        out.reserveCapacity(s.count + 2)
        for ch in s {
            switch ch {
            case "\"": out += "\\\""
            case "\\": out += "\\\\"
            default: out.append(ch)
            }
        }
        out += "\""
        return out
    }
}
