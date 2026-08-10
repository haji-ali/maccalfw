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

    public var message: String {
        switch self {
        case .notAuthorized(let m), .invalidArgument(let m), .general(let m):
            return m
        }
    }

    public var description: String { message }
}
