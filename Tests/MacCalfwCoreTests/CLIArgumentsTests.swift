import XCTest

@testable import MacCalfwCore

final class CLIArgumentsTests: XCTestCase {
    func testParsesCommandAndSimpleFlags() {
        let args = CLIArguments(["events", "--start", "2026-01-01", "--end", "2026-01-02"])
        XCTAssertEqual(args.command, "events")
        XCTAssertEqual(args.value("start"), "2026-01-01")
        XCTAssertEqual(args.value("end"), "2026-01-02")
    }

    func testRepeatedFlagsAccumulateInOrder() {
        let args = CLIArguments(["events", "--calendar", "a", "--calendar", "b"])
        XCTAssertEqual(args.values("calendar"), ["a", "b"])
    }

    func testLastValueWinsForSingleValueAccessor() {
        let args = CLIArguments(["events", "--format", "json", "--format", "elisp"])
        XCTAssertEqual(args.value("format"), "elisp")
    }

    func testBooleanFlagsTakeNoValue() {
        let args = CLIArguments(
            ["remove-event", "--id", "abc", "--future"], booleanFlags: ["future"])
        XCTAssertEqual(args.value("id"), "abc")
        XCTAssertTrue(args.flag("future"))
    }

    func testMissingFlagReturnsNil() {
        let args = CLIArguments(["timezones"])
        XCTAssertNil(args.value("format"))
        XCTAssertFalse(args.flag("future"))
        XCTAssertEqual(args.values("calendar"), [])
    }

    func testEmptyArgumentsHaveNoCommand() {
        let args = CLIArguments([])
        XCTAssertNil(args.command)
    }

    func testCommandWithNoOptions() {
        let args = CLIArguments(["refresh"])
        XCTAssertEqual(args.command, "refresh")
        XCTAssertNil(args.value("anything"))
    }
}

final class CLIErrorTests: XCTestCase {
    func testNotAuthorizedHasExitCode2() {
        let err = CLIError.notAuthorized("nope")
        XCTAssertEqual(err.exitCode, 2)
        XCTAssertEqual(err.message, "nope")
    }

    func testGeneralAndInvalidArgumentHaveExitCode1() {
        XCTAssertEqual(CLIError.general("x").exitCode, 1)
        XCTAssertEqual(CLIError.invalidArgument("x").exitCode, 1)
    }
}
