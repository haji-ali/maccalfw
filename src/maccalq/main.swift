import Foundation
import MacCalfwCore

let rawArguments = Array(CommandLine.arguments.dropFirst())
if rawArguments.isEmpty || rawArguments[0] == "help" || rawArguments[0] == "--help"
    || rawArguments[0] == "-h"
{
    print(helpText)
    exit(0)
}

#if canImport(EventKit)
    import EventKit

    let booleanFlags: Set<String> = ["future", "include-completed"]
    let arguments = CLIArguments(rawArguments, booleanFlags: booleanFlags)

    // Errors are a single plain-text line on stderr, not a --format-
    // encoded payload: --format governs successful command output,
    // not failure reporting. Exit code 2 vs 1 (see CLIError.exitCode)
    // is what lets callers distinguish not-authorized from other
    // failures without parsing the message.
    func fail(_ error: CLIError) -> Never {
        FileHandle.standardError.write(Data("maccalq: \(error.message)\n".utf8))
        exit(error.exitCode)
    }

    guard let command = arguments.command else {
        fail(.invalidArgument("Missing command"))
    }

    do {
        let eventStore = EKEventStore()
        let output = try runCommand(command, arguments: arguments, eventStore: eventStore)
        print(output)
    } catch let error as CLIError {
        fail(error)
    } catch {
        fail(.general("\(error)"))
    }
#else
    FileHandle.standardError.write(Data("maccalq: EventKit is only available on macOS\n".utf8))
    exit(1)
#endif
