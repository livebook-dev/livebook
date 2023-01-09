// swift-tools-version: 5.5

import PackageDescription

let package = Package(
    name: "Demo",
    platforms: [
        .macOS(.v11)
    ],
    dependencies: [
        .package(name: "ElixirKit", path: "../../../elixirkit_swift")
    ],
    targets: [
        .executableTarget(
            name: "Demo",
            dependencies: ["ElixirKit"]
        )
    ]
)
