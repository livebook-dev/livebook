import Foundation
import Network

public class API {
    static var process: Process?
    private static var release: Release?

    public static var isRunning: Bool {
        get {
            release != nil && release!.isRunning;
        }
    }

    public static func start(name: String, logPath: String? = nil, terminationHandler: ((Process) -> Void)? = nil) {
        release = Release(name: name, logPath: logPath, terminationHandler: terminationHandler)
    }

    public static func publish(_ name: String, _ data: String) {
        release!.publish(name, data)
    }

    public static func stop() {
        release!.stop();
    }

    public static func waitUntilExit() {
        release!.waitUntilExit();
    }
}

private class Release {
    let listener: NWListener
    let startProcess: Process
    let semaphore = DispatchSemaphore(value: 0)
    var logHandle: FileHandle?
    var connection: NWConnection?

    var isRunning: Bool {
        get {
            startProcess.isRunning
        }
    }

    init(name: String, logPath: String? = nil, terminationHandler: ((Process) -> Void)? = nil) {
        listener = try! NWListener(using: .tcp, on: .any)

        let bundle = Bundle.main
        var rootDir = "";

        if bundle.bundlePath.hasSuffix(".app") {
            rootDir = "\(bundle.bundlePath)/Contents/Resources"
        }
        else {
            rootDir = bundle.bundlePath
        }

        startProcess = Process()

        if logPath != nil {
            let logPath = logPath!
            let fm = FileManager.default
            if !fm.fileExists(atPath: logPath) { fm.createFile(atPath: logPath, contents: Data()) }
            logHandle = FileHandle(forUpdatingAtPath: logPath)!
            logHandle!.seekToEndOfFile()

            let stdout = Pipe()
            let stderr = Pipe()
            startProcess.standardOutput = stdout
            startProcess.standardError = stderr
            let stdouth = stdout.fileHandleForReading
            let stderrh = stderr.fileHandleForReading
            stdouth.waitForDataInBackgroundAndNotify()
            stderrh.waitForDataInBackgroundAndNotify()

            NotificationCenter.default.addObserver(
                self,
                selector: #selector(receiveStdout(n:)),
                name: NSNotification.Name.NSFileHandleDataAvailable,
                object: stdouth
            )

            NotificationCenter.default.addObserver(
                self,
                selector: #selector(receiveStderr(n:)),
                name: NSNotification.Name.NSFileHandleDataAvailable,
                object: stderrh
            )
        }

        startProcess.launchPath = "\(rootDir)/rel/bin/\(name)"
        startProcess.arguments = ["start"]
        startProcess.terminationHandler = terminationHandler

        listener.stateUpdateHandler = stateDidChange(to:)
        listener.newConnectionHandler = didAccept(connection:)
        listener.start(queue: .global())

        let timeout = DispatchTime.now() + DispatchTimeInterval.seconds(5)

        if semaphore.wait(timeout: timeout) == .timedOut {
            fatalError("waited for connection for more than 5s")
        }
    }

    func stateDidChange(to state: NWListener.State) {
         switch state {
         case .ready:
            start(port: listener.port!.rawValue.description)

         case .failed(let error):
             print("Server error: \(error.localizedDescription)")
             exit(EXIT_FAILURE)
         default:
             break
         }
    }

    func start(port: String) {
        var env = ProcessInfo.processInfo.environment
        env["ELIXIRKIT_PORT"] = port
        startProcess.environment = env
        try! startProcess.run()
    }

    func didAccept(connection: NWConnection) {
        self.connection = connection
        self.connection!.start(queue: .main)
        semaphore.signal()
    }

    func send(_ string: String) {
        connection!.send(
            content: (string + "\n").data(using: .utf8),
            completion: .contentProcessed { error in
                if error != nil {
                    print(error!)
                }
            }
        )
    }

    @objc
    func receiveStdout(n: NSNotification) {
        let h = n.object as! FileHandle
        let data = h.availableData
        if !data.isEmpty {
            FileHandle.standardOutput.write(data)
            logHandle!.write(data)
            h.waitForDataInBackgroundAndNotify()
        }
    }

    @objc
    func receiveStderr(n: NSNotification) {
        let h = n.object as! FileHandle
        let data = h.availableData
        if !data.isEmpty {
            logHandle!.write(data)
            FileHandle.standardError.write(data)
            h.waitForDataInBackgroundAndNotify()
        }
    }

    public func publish(_ name: String, _ data: String) {
        let encoded = data.data(using: .utf8)!.base64EncodedString()
        let message = "event:\(name):\(encoded)"
        send(message)
    }

    public func stop() {
        connection!.cancel()
        listener.cancel()
        waitUntilExit()
    }

    public func waitUntilExit() {
        startProcess.waitUntilExit()
    }
}
