// swift-tools-version: 5.5

import PackageDescription

let package = Package(
    name: "ElixirKit",
    platforms: [
        .macOS(.v11)
    ],
    products: [
        .library(
            name: "ElixirKit",
            targets: ["ElixirKit"]
        ),
    ],
    dependencies: [],
    targets: [
        .target(
            name: "ElixirKit",
            dependencies: []
        ),
        .testTarget(
            name: "ElixirKitTests",
            dependencies: ["ElixirKit"]
        )
    ]
)
