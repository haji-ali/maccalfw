import XCTest

@testable import MacCalfwCore

final class ICalEncoderTests: XCTestCase {
    static let sampleProperties: [ICalProperty] = [
        ICalProperty("UID", value: "uid-1"),
        ICalProperty("SUMMARY", value: "Party"),
        ICalProperty("LOCATION", value: "Robot House"),
        ICalProperty(
            "DTSTART", params: [ICalParam("TZID", "America/New_York")],
            value: "20260101T090000"),
        ICalProperty("RRULE", value: "FREQ=WEEKLY;BYDAY=MO"),
    ]

    // MARK: - Elisp encoder (icalendar.el-compatible triples)

    func testElispEncodesFlatTripleShape() {
        let encoded = ICalElispEncoder.encode([
            ICalProperty("SUMMARY", value: "Party")
        ])
        XCTAssertEqual(encoded, "((SUMMARY nil \"Party\"))")
    }

    func testElispEncodesParamsAsFlatList() {
        let encoded = ICalElispEncoder.encode([
            ICalProperty(
                "DTSTART", params: [ICalParam("TZID", "America/New_York")],
                value: "20260101T090000")
        ])
        XCTAssertEqual(
            encoded,
            "((DTSTART (TZID \"America/New_York\") \"20260101T090000\"))")
    }

    func testElispEscapesQuotesAndBackslashes() {
        let encoded = ICalElispEncoder.encode([
            ICalProperty("SUMMARY", value: "Say \"hi\" \\ bye")
        ])
        XCTAssertEqual(
            encoded,
            "((SUMMARY nil \"Say \\\"hi\\\" \\\\ bye\"))")
    }

    func testElispRepeatsPropertyNameForMultipleOccurrences() {
        let encoded = ICalElispEncoder.encode([
            ICalProperty("RRULE", value: "FREQ=DAILY"),
            ICalProperty("RRULE", value: "FREQ=WEEKLY"),
        ])
        XCTAssertEqual(
            encoded,
            "((RRULE nil \"FREQ=DAILY\")\n (RRULE nil \"FREQ=WEEKLY\"))")
    }

    // MARK: - iCalendar text encoder (RFC5545)

    func testTextEncoderWrapsComponentInVCalendar() {
        let text = ICalTextEncoder.encode([
            ICalComponent(
                type: "VEVENT",
                properties: [ICalProperty("SUMMARY", value: "Party")])
        ])
        XCTAssertTrue(text.hasPrefix("BEGIN:VCALENDAR\r\n"))
        XCTAssertTrue(text.contains("BEGIN:VEVENT\r\nSUMMARY:Party\r\nEND:VEVENT\r\n"))
        XCTAssertTrue(text.hasSuffix("END:VCALENDAR\r\n"))
    }

    func testTextEncoderEmitsParams() {
        let text = ICalTextEncoder.encodeProperty(
            ICalProperty(
                "DTSTART", params: [ICalParam("TZID", "America/New_York")],
                value: "20260101T090000"))
        XCTAssertEqual(text, "DTSTART;TZID=America/New_York:20260101T090000")
    }

    func testTextEncoderEscapesCommasSemicolonsAndNewlines() {
        let text = ICalTextEncoder.encodeProperty(
            ICalProperty("DESCRIPTION", value: "a; b, c\nd\\e"))
        XCTAssertEqual(text, "DESCRIPTION:a\\; b\\, c\\nd\\\\e")
    }

    func testTextEncoderQuotesParamValuesWithReservedChars() {
        let text = ICalTextEncoder.encodeProperty(
            ICalProperty(
                "ORGANIZER", params: [ICalParam("CN", "Doe, John")],
                value: "mailto:john@example.com"))
        XCTAssertEqual(text, "ORGANIZER;CN=\"Doe, John\":mailto:john@example.com")
    }

    func testTextEncoderFoldsLongLines() {
        let longValue = String(repeating: "x", count: 200)
        let text = ICalTextEncoder.encodeProperty(
            ICalProperty("DESCRIPTION", value: longValue))
        let folded = ICalTextEncoder.fold(text)
        // Every physical line (split on the fold's own CRLF) must fit in
        // 75 octets, and continuation lines start with a single space.
        for (i, line) in folded.components(separatedBy: "\r\n").enumerated() {
            XCTAssertLessThanOrEqual(line.utf8.count, 75)
            if i > 0 {
                XCTAssertTrue(line.hasPrefix(" "))
            }
        }
        // Unfolding (strip "\r\n ") must reconstruct the original line.
        let unfolded = folded.replacingOccurrences(of: "\r\n ", with: "")
        XCTAssertEqual(unfolded, text)
    }

    func testTextEncoderFoldingPreservesMultibyteCharacters() {
        // Emoji/CJK are multi-byte in UTF-8; folding must never split
        // inside one, or decoding the fragment would crash/corrupt.
        let value = String(repeating: "\u{1F389}", count: 40)  // 4 bytes each
        let text = ICalTextEncoder.encodeProperty(
            ICalProperty("SUMMARY", value: value))
        let folded = ICalTextEncoder.fold(text)
        let unfolded = folded.replacingOccurrences(of: "\r\n ", with: "")
        XCTAssertEqual(unfolded, text)
    }

    // MARK: - JSON encoder

    func testJSONEncoderRoundTripsThroughDecoder() throws {
        let json = try ICalJSONEncoder.encode(Self.sampleProperties)
        let decoded = try JSONDecoder().decode(
            [ICalProperty].self, from: Data(json.utf8))
        XCTAssertEqual(decoded, Self.sampleProperties)
    }

    // MARK: - Cross-format: all three come from the same model, independently

    func testAllFormatsEncodeIndependentlyFromSameProperties() throws {
        // None of these should throw, and each should reflect the same
        // underlying data despite no format being derived from another.
        let icalText = try ICalEncoder.encode(Self.sampleProperties, as: .icalendar)
        let json = try ICalEncoder.encode(Self.sampleProperties, as: .json)
        let elisp = try ICalEncoder.encode(Self.sampleProperties, as: .elisp)

        XCTAssertTrue(icalText.contains("SUMMARY:Party"))
        XCTAssertTrue(json.contains("\"Party\""))
        XCTAssertTrue(elisp.contains("(SUMMARY nil \"Party\")"))
    }
}
