import Foundation
import ElixirKit

@main
struct Demo {
    public static func main() {
        ElixirKit.API.start(name: "demo")

        // Capture ctrl+c
        signal(SIGINT) { signal in
            ElixirKit.API.stop()
            exit(signal)
        }

        ElixirKit.API.publish("log", "Hello from Swift!")
        ElixirKit.API.waitUntilExit()
    }
}
