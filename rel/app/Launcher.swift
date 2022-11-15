import AppKit

class AppDelegate: NSObject, NSApplicationDelegate {
    private var statusItem: NSStatusItem!

    func applicationDidFinishLaunching(_ aNotification: Notification) {
        let image = NSImage(named: NSImage.Name("Icon"))!
        let resized = NSImage(size: NSSize(width: 18, height: 18), flipped: false) { (dstRect) -> Bool in
            image.draw(in: dstRect)
            return true
        }
        statusItem = NSStatusBar.system.statusItem(withLength: NSStatusItem.variableLength)
        statusItem.button!.image = resized

        let statusMenu = NSMenu()
        statusMenu.addItem(NSMenuItem(title: "Open Browser", action: #selector(AppDelegate.openBrowser), keyEquivalent: "o"))
        statusMenu.addItem(NSMenuItem(title: "Quit", action: #selector(NSApplication.terminate(_:)), keyEquivalent: "q"))
        statusItem.menu = statusMenu

        ElixirKit.subscribe("abort", queue: .main) { message in
            let alert = NSAlert()
            alert.alertStyle = .critical
            alert.messageText = "Livebook crashed"
            alert.informativeText = message
            alert.runModal()
            NSApp.terminate(nil)
        }
    }

    func application(_ app: NSApplication, open urls: [URL]) {
        for url in urls {
            ElixirKit.publish("open_url", url.absoluteString)
        }
    }

    @objc
    func openBrowser() {
        ElixirKit.publish("open_browser", "")
    }
}

@main
class Main {
    static func main() {
        ElixirKit.startAsync()

        let app = NSApplication.shared
        let delegate = AppDelegate()
        app.delegate = delegate
        app.run()
    }
}
