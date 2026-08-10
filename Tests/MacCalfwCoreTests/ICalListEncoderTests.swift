import XCTest

@testable import MacCalfwCore

final class ICalListEncoderTests: XCTestCase {
    static let items: [(componentType: String, properties: [ICalProperty])] = [
        ("VEVENT", [ICalProperty("SUMMARY", value: "Party")]),
        ("VEVENT", [ICalProperty("SUMMARY", value: "Meeting")]),
    ]

    func testIcalendarWrapsMultipleComponentsInOneVCalendar() throws {
        let text = try ICalListEncoder.encode(Self.items, as: .icalendar)
        XCTAssertEqual(text.components(separatedBy: "BEGIN:VEVENT").count - 1, 2)
        XCTAssertEqual(text.components(separatedBy: "BEGIN:VCALENDAR").count - 1, 1)
        XCTAssertTrue(text.contains("SUMMARY:Party"))
        XCTAssertTrue(text.contains("SUMMARY:Meeting"))
    }

    func testJSONEncodesListOfPropertyLists() throws {
        let json = try ICalListEncoder.encode(Self.items, as: .json)
        let decoded = try JSONDecoder().decode([[ICalProperty]].self, from: Data(json.utf8))
        XCTAssertEqual(decoded, Self.items.map { $0.properties })
    }

    func testElispEncodesListOfTripleLists() throws {
        let elisp = try ICalListEncoder.encode(Self.items, as: .elisp)
        XCTAssertEqual(
            elisp,
            "(((SUMMARY nil \"Party\"))\n ((SUMMARY nil \"Meeting\")))")
    }

    func testEmptyListEncodesToEmptyContainer() throws {
        XCTAssertEqual(try ICalListEncoder.encode([], as: .elisp), "()")
        XCTAssertEqual(try ICalListEncoder.encode([], as: .json), "[]")
    }
}
