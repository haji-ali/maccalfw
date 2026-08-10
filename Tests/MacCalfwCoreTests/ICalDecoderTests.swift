import XCTest

@testable import MacCalfwCore

final class ICalDecoderTests: XCTestCase {
    static let sampleProperties: [ICalProperty] = [
        ICalProperty("UID", value: "uid-1"),
        ICalProperty("SUMMARY", value: "Party"),
        ICalProperty("LOCATION", value: "Robot House"),
        ICalProperty(
            "DTSTART", params: [ICalParam("TZID", "America/New_York")],
            value: "20260101T090000"),
        ICalProperty("RRULE", value: "FREQ=WEEKLY;BYDAY=MO"),
        ICalProperty("RRULE", value: "FREQ=DAILY"),
    ]

    // MARK: - Elisp decoder

    func testElispRoundTripsThroughEncoder() throws {
        let encoded = ICalElispEncoder.encode(Self.sampleProperties)
        let decoded = try ICalElispDecoder.decode(encoded)
        XCTAssertEqual(decoded, Self.sampleProperties)
    }

    func testElispDecodesNilParams() throws {
        let decoded = try ICalElispDecoder.decode("((SUMMARY nil \"Party\"))")
        XCTAssertEqual(decoded, [ICalProperty("SUMMARY", value: "Party")])
    }

    func testElispDecodesParams() throws {
        let decoded = try ICalElispDecoder.decode(
            "((DTSTART (TZID \"America/New_York\") \"20260101T090000\"))")
        XCTAssertEqual(
            decoded,
            [
                ICalProperty(
                    "DTSTART", params: [ICalParam("TZID", "America/New_York")],
                    value: "20260101T090000")
            ])
    }

    func testElispUnescapesQuotesAndBackslashes() throws {
        let decoded = try ICalElispDecoder.decode(
            "((SUMMARY nil \"Say \\\"hi\\\" \\\\ bye\"))")
        XCTAssertEqual(decoded, [ICalProperty("SUMMARY", value: "Say \"hi\" \\ bye")])
    }

    func testElispToleratesExtraWhitespaceAndNewlines() throws {
        let decoded = try ICalElispDecoder.decode(
            "(  (SUMMARY   nil   \"Party\" )\n  (LOCATION nil \"Robot House\")  )")
        XCTAssertEqual(
            decoded,
            [
                ICalProperty("SUMMARY", value: "Party"),
                ICalProperty("LOCATION", value: "Robot House"),
            ])
    }

    func testElispDecodesEmptyList() throws {
        XCTAssertEqual(try ICalElispDecoder.decode("()"), [])
    }

    func testElispThrowsOnTruncatedInput() {
        XCTAssertThrowsError(try ICalElispDecoder.decode("((SUMMARY nil \"Party\")"))
    }

    func testElispThrowsOnUnterminatedString() {
        XCTAssertThrowsError(try ICalElispDecoder.decode("((SUMMARY nil \"Party))"))
    }

    // MARK: - JSON decoder

    func testJSONDecoderRoundTripsThroughEncoder() throws {
        let encoded = try ICalJSONEncoder.encode(Self.sampleProperties)
        let decoded = try ICalJSONDecoder.decode(encoded)
        XCTAssertEqual(decoded, Self.sampleProperties)
    }

    // MARK: - Dispatcher

    func testICalDecoderDispatchesToJSONAndElisp() throws {
        for format: ICalFormat in [.json, .elisp] {
            let encoded = try ICalEncoder.encode(Self.sampleProperties, as: format)
            let decoded = try ICalDecoder.decode(encoded, as: format)
            XCTAssertEqual(decoded, Self.sampleProperties, "format: \(format)")
        }
    }

    func testICalDecoderThrowsForIcalendarFormat() {
        XCTAssertThrowsError(try ICalDecoder.decode("BEGIN:VCALENDAR", as: .icalendar))
    }
}
