import AppKit
import ElixirKit

@main
public struct Demo {
    public static func main() {
        let app = NSApplication.shared
        let delegate = AppDelegate()
        app.delegate = delegate
        app.run()
    }
}

class AppDelegate: NSObject, NSApplicationDelegate {
    private var window : NSWindow!

    func applicationDidFinishLaunching(_ aNotification: Notification) {
        ElixirKit.API.start(
            name: "demo",
            terminationHandler: { _ in
                NSApp.terminate(nil)
            }
        )

        ElixirKit.API.publish("log", "Hello from AppKit!")

        let menuItemOne = NSMenuItem()
        menuItemOne.submenu = NSMenu(title: "Demo")
        menuItemOne.submenu?.items = [
            NSMenuItem(title: "Quit Demo", action: #selector(NSApplication.terminate(_:)), keyEquivalent: "q")
        ]
        let menu = NSMenu()
        menu.items = [menuItemOne]
        NSApp.mainMenu = menu

        window = NSWindow(contentRect: NSMakeRect(0, -1000, 200, 200),
                          styleMask: [.titled, .closable],
                          backing: .buffered,
                          defer: true)
        window.orderFrontRegardless()
        window.title = "Demo"

        let button = NSButton(title: "Press me!", target: self, action: #selector(pressMe))
        window.contentView!.subviews.append(button)

        NSApp.setActivationPolicy(.regular)
        NSApp.activate(ignoringOtherApps: true)
    }

    func applicationShouldTerminateAfterLastWindowClosed(_ app: NSApplication) -> Bool {
        return true
    }

    func applicationWillTerminate(_ aNotification: Notification) {
        ElixirKit.API.stop()
    }

    @objc
    func pressMe() {
        ElixirKit.API.publish("log", "button pressed!")
    }
}
