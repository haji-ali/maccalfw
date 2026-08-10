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

    func fail(_ error: CLIError) -> Never {
        FileHandle.standardError.write(Data("maccalq: \(error.message)\n".utf8))
        let format = arguments.value("format").flatMap(ICalFormat.init(rawValue:)) ?? .icalendar
        if let encoded = try? ICalEncoder.encode(error.properties, as: format) {
            print(encoded)
        }
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
