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
    private var window: NSWindow!
    private var button: NSButton!

    func applicationDidFinishLaunching(_ aNotification: Notification) {
        button = NSButton(title: "Press me!", target: self, action: #selector(pressMe))
        button.isEnabled = false

        ElixirKit.API.start(
            name: "demo",
            readyHandler: {
                // GUI updates need to happen on the main thread.
                DispatchQueue.main.sync {
                    self.button.isEnabled = true
                }

                ElixirKit.API.publish("log", "Hello from AppKit!")

                ElixirKit.API.addObserver(queue: .main) { (name, data) in
                    switch name {
                    case "log":
                        print("[client] " + data)
                    default:
                        fatalError("unknown event \(name)")
                    }
                }
            },
            terminationHandler: { _ in
                NSApp.terminate(nil)
            }
        )

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
