import AppKit
import ElixirKit

@main
public struct Livebook {
    public static func main() {
        let app = NSApplication.shared
        let delegate = AppDelegate()
        app.delegate = delegate
        app.run()
    }
}

class AppDelegate: NSObject, NSApplicationDelegate {
    private var statusItem: NSStatusItem!
    private var logPath: String!
    private var launchedByOpenURL = false
    private var initialURLs: [URL] = []

    func applicationDidFinishLaunching(_ aNotification: Notification) {
        logPath = "\(NSHomeDirectory())/Library/Logs/Livebook.log"

        ElixirKit.API.start(name: "app", logPath: logPath) { process in
            if process.terminationStatus != 0 {
                DispatchQueue.main.sync {
                    let alert = NSAlert()
                    alert.alertStyle = .critical
                    alert.messageText = "Livebook exited with error status \(process.terminationStatus)"
                    alert.addButton(withTitle: "Dismiss")
                    alert.addButton(withTitle: "View Logs")

                    switch alert.runModal() {
                    case .alertSecondButtonReturn:
                        self.viewLogs()
                    default:
                        ()
                    }
                }
            }

            NSApp.terminate(nil)
        }

        if (self.initialURLs == []) {
            ElixirKit.API.publish("open", "")
        }
        else {
            for url in self.initialURLs {
                ElixirKit.API.publish("open", url.absoluteString)
            }
        }

        statusItem = NSStatusBar.system.statusItem(withLength: NSStatusItem.variableLength)
        let button = statusItem.button!
        button.image = NSImage(named: "MenuBarIcon")
        let menu = NSMenu()
        menu.items = [
            NSMenuItem(title: "Open", action: #selector(open), keyEquivalent: "o"),
            NSMenuItem(title: "Quit", action: #selector(NSApplication.terminate(_:)), keyEquivalent: "q")
        ]
        statusItem.menu = menu
    }

    func applicationWillTerminate(_ aNotification: Notification) {
        if ElixirKit.API.isRunning {
            NSStatusBar.system.removeStatusItem(statusItem)

            ElixirKit.API.publish("shutdown", "")
            ElixirKit.API.waitUntilExit()
        }
    }

    func application(_ app: NSApplication, open urls: [URL]) {
        if !ElixirKit.API.isRunning {
            initialURLs = urls
            return
        }

        for url in urls {
            ElixirKit.API.publish("open", url.absoluteString)
        }
    }

    @objc
    func open() {
        ElixirKit.API.publish("open", "")
    }

    @objc
    func viewLogs() {
        NSWorkspace.shared.open(NSURL.fileURL(withPath: logPath))
    }
}
