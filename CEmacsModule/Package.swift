// swift-tools-version: 5.7
// The swift-tools-version declares the minimum version of Swift required to build this package.

import PackageDescription

let package = Package(
  name: "Csqlite3",
  products: [
  .library(name: "CEmacsModule", targets: ["CEmacsModule"]),
  ],
  targets: [
    .systemLibrary(name: "CEmacsModule"),
  ]
)
