/// Errors the CLI can report. Distinct from the encode/decode errors
/// above -- this is specifically what a *command* (calendars/events/
/// update-event/etc.) can fail with.
public enum CLIError: Error, CustomStringConvertible, Sendable {
    /// Calendar/reminders access wasn't granted. Reported with exit
    /// code 2 so callers can special-case it (e.g. prompt the user to
    /// grant access) without parsing the message.
    case notAuthorized(String)
    case invalidArgument(String)
    case general(String)

    public var exitCode: Int32 {
        switch self {
        case .notAuthorized: return 2
        case .invalidArgument, .general: return 1
        }
    }

    public var errorType: String {
        switch self {
        case .notAuthorized: return "not-authorized"
        case .invalidArgument: return "invalid-argument"
        case .general: return "error"
        }
    }

    public var message: String {
        switch self {
        case .notAuthorized(let m), .invalidArgument(let m), .general(let m):
            return m
        }
    }

    public var description: String { message }

    /// The structured error payload printed on stdout, in whichever
    /// format was requested, so callers can distinguish error kinds
    /// programmatically instead of scraping the human-readable message
    /// (which goes to stderr separately).
    public var properties: [ICalProperty] {
        [ICalProperty("ERROR", value: errorType), ICalProperty("MESSAGE", value: message)]
    }
}
