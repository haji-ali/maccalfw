// swift-tools-version:5.9
import Foundation
import PackageDescription

// The MacCalfwCoreTests target is only meaningful in a full checkout
// (it needs Tests/MacCalfwCoreTests to exist); straight.el's build
// directory only vendors src/, not Tests/, so it's included
// conditionally rather than shipped to every install. Package.swift
// is itself evaluated as Swift, so a plain file-existence check here
// is enough to leave the target out of the graph entirely when the
// tests aren't present, instead of failing to resolve it.
let testsPath = "Tests/MacCalfwCoreTests"
let hasTests = FileManager.default.fileExists(atPath: testsPath)

var targets: [Target] = [
    // Portable: encodes/decodes iCalendar text, JSON and the elisp
    // sexp format, and dispatches CLI commands/arguments. Has no
    // dependency on EventKit/Cocoa, so it builds and tests on any
    // platform Swift runs on (this is what lets it be developed and
    // tested on Linux, even though the calendar access itself is
    // macOS-only).
    .target(
        name: "MacCalfwCore",
        path: "src/MacCalfwCore"
    ),
    // Thin executable: adds the actual EventKit calendar/reminder
    // access on top of MacCalfwCore. Only functional on macOS; the
    // EventKit-specific code is guarded with #if canImport(EventKit)
    // so the target still builds (to a stub) on other platforms.
    .executableTarget(
        name: "maccalq",
        dependencies: ["MacCalfwCore"],
        path: "src/maccalq"
    ),
]

if hasTests {
    targets.append(
        .testTarget(
            name: "MacCalfwCoreTests",
            dependencies: ["MacCalfwCore"],
            path: testsPath
        )
    )
}

let package = Package(
    name: "maccalfw",
    platforms: [.macOS(.v14)],
    products: [
        .executable(name: "maccalq", targets: ["maccalq"]),
        .library(name: "MacCalfwCore", targets: ["MacCalfwCore"]),
    ],
    targets: targets
)
