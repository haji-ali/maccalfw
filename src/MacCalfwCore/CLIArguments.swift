/// Minimal `--flag value` / repeated-flag command-line argument parser.
///
/// The first element of `arguments` is the subcommand name (e.g.
/// "events"); everything after it is parsed as `--name value` pairs,
/// except names listed in `booleanFlags`, which take no value (e.g.
/// `--future`). A flag may repeat (e.g. `--calendar a --calendar b`),
/// in which case `values(_:)` returns all of them in order.
public struct CLIArguments: Sendable {
    public let command: String?
    public let options: [String: [String]]

    public init(_ arguments: [String], booleanFlags: Set<String> = []) {
        guard !arguments.isEmpty else {
            command = nil
            options = [:]
            return
        }
        command = arguments[0]
        var opts: [String: [String]] = [:]
        var i = 1
        while i < arguments.count {
            let arg = arguments[i]
            guard arg.hasPrefix("--") else {
                i += 1
                continue
            }
            let name = String(arg.dropFirst(2))
            if booleanFlags.contains(name) {
                opts[name, default: []].append("t")
                i += 1
                continue
            }
            guard i + 1 < arguments.count else {
                opts[name, default: []].append("")
                i += 1
                continue
            }
            opts[name, default: []].append(arguments[i + 1])
            i += 2
        }
        options = opts
    }

    /// The value of the last occurrence of `--name`, if any.
    public func value(_ name: String) -> String? { options[name]?.last }

    /// All values passed for `--name`, in the order given.
    public func values(_ name: String) -> [String] { options[name] ?? [] }

    /// Whether `--name` was present at all (for boolean flags).
    public func flag(_ name: String) -> Bool { options[name] != nil }
}
