import Foundation
import Network

public class API {
    static var process: Process?
    private static var release: Release?

    static let didReceiveEvent = NSNotification.Name("elixirkit.event")

    public static var isRunning: Bool {
        get {
            release != nil && release!.isRunning;
        }
    }

    public static func start(
        name: String,
        logPath: String? = nil,
        readyHandler: @escaping () -> Void,
        terminationHandler: ((Process) -> Void)? = nil) {

        release = Release(name: name, logPath: logPath, readyHandler: readyHandler, terminationHandler: terminationHandler)
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

    public static func addObserver(queue: OperationQueue?, using: @escaping (((String, String)) -> Void)) {
        NotificationCenter.default.addObserver(forName: didReceiveEvent, object: nil, queue: queue) { n in
            let (name, data) = n.object! as! (String, String)
            using((name, data))
        }
    }
}

private class Release {
    let process: Process
    let logger: Logger
    let listener: NWListener
    var connection: Connection?
    let readyHandler: () -> Void
    let semaphore = DispatchSemaphore(value: 0)

    var isRunning: Bool {
        get {
            process.isRunning
        }
    }

    init(
        name: String,
        logPath: String? = nil,
        readyHandler: @escaping () -> Void,
        terminationHandler: ((Process) -> Void)? = nil) {

        self.readyHandler = readyHandler
        logger = Logger(logPath: logPath)
        listener = try! NWListener(using: .tcp, on: .any)

        let bundle = Bundle.main
        var rootDir = "";

        if bundle.bundlePath.hasSuffix(".app") {
            rootDir = "\(bundle.bundlePath)/Contents/Resources"
        }
        else {
            rootDir = bundle.bundlePath
        }

        process = Process()
        process.launchPath = "\(rootDir)/rel/bin/\(name)"
        process.arguments = ["start"]
        process.terminationHandler = terminationHandler

        if logPath != nil {
            logger.capture(process: process)
        }

        listener.stateUpdateHandler = stateDidChange(to:)
        listener.newConnectionHandler = didAccept(conn:)
        listener.start(queue: .global())

        let seconds = 5
        let timeout = DispatchTime.now() + DispatchTimeInterval.seconds(seconds)

        if semaphore.wait(timeout: timeout) == .timedOut {
            fatalError("waited for connection for more than \(seconds)s")
        }
    }

    public func stop() {
        connection?.cancel()
        listener.cancel()
        waitUntilExit()
    }

    public func waitUntilExit() {
        process.waitUntilExit()
    }

    public func publish(_ name: String, _ data: String) {
        connection!.send("\(name):\(data)")
    }

    private func stateDidChange(to state: NWListener.State) {
         switch state {
         case .ready:
            start(port: listener.port!.rawValue.description)

         case .failed(let error):
             print("Listener error: \(error.localizedDescription)")
             exit(EXIT_FAILURE)
         default:
             break
         }
    }

    private func start(port: String) {
        var env = ProcessInfo.processInfo.environment
        env["ELIXIRKIT_PORT"] = port
        process.environment = env
        try! process.run()
        semaphore.signal()
    }

    private func didAccept(conn: NWConnection) {
        self.connection = Connection(conn: conn, logger: logger)
        readyHandler()
    }
}

// Logs to stdout and a log file (if given)
private class Logger {
    var logHandle: FileHandle?

    init(logPath: String?) {
        if let logPath = logPath {
            let fm = FileManager.default
            if !fm.fileExists(atPath: logPath) { fm.createFile(atPath: logPath, contents: Data()) }
            logHandle = FileHandle(forUpdatingAtPath: logPath)!
            logHandle!.seekToEndOfFile()
        }
    }

    public func capture(process: Process) {
        let stdout = Pipe()
        let stderr = Pipe()
        process.standardOutput = stdout
        process.standardError = stderr
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

    public func puts(_ string: String) {
        let data = (string + "\n").data(using: .utf8)!
        logHandle?.write(data)
        FileHandle.standardOutput.write(data)
    }

    @objc
    private func receiveStdout(n: NSNotification) {
        let h = n.object as! FileHandle
        let data = h.availableData
        if !data.isEmpty {
            FileHandle.standardOutput.write(data)
            logHandle!.write(data)
            h.waitForDataInBackgroundAndNotify()
        }
    }

    @objc
    private func receiveStderr(n: NSNotification) {
        let h = n.object as! FileHandle
        let data = h.availableData
        if !data.isEmpty {
            FileHandle.standardError.write(data)
            logHandle!.write(data)
            h.waitForDataInBackgroundAndNotify()
        }
    }
}

private struct Connection {
    let conn: NWConnection
    let logger: Logger

    init(conn: NWConnection, logger: Logger) {
        self.logger = logger
        self.conn = conn
        self.conn.stateUpdateHandler = stateDidChange(to:)
        self.conn.start(queue: .main)
    }

    func stateDidChange(to state: NWConnection.State) {
        switch state {
        case .ready:
            receiveEventMessage()

        case .failed(let error):
            logger.puts("\(error)")
            exit(EXIT_FAILURE)

        default:
            break
        }
    }

    // Receives event message from the socket and posts it as a notification.
    // A message contains a header which is a big endian uint32 that is the length of the payload that follows.
    func receiveEventMessage() {
        conn.receive(minimumIncompleteLength: 4, maximumLength: 4) { (data, _context, isComplete, error) in
            if (isComplete) {
                return
            }

            if let error = error {
                switch error {
                case .posix(POSIXError.ECANCELED):
                    // socket is closed, ignore.
                    ()
                default:
                    logger.puts("\(error)")
                }

                return
            }

            let size = Int(data!.withUnsafeBytes { pointer in CFSwapInt32BigToHost(pointer.load(as: UInt32.self)) })
            self.conn.receive(minimumIncompleteLength: size, maximumLength: size) { (payload, _context, isComplete, error) in
                if (isComplete) {
                    return
                }

                if let error = error {
                    switch error {
                    case .posix(POSIXError.ECANCELED):
                        // socket is closed, ignore.
                        ()
                    default:
                        logger.puts("\(error)")
                    }

                    return
                }

                let parts = payload!.split(separator: UInt8(ascii:":"), maxSplits: 1)
                let eventName = String(data: parts[0], encoding: .utf8)!
                let eventData = String(data: parts[1], encoding: .utf8)!
                NotificationCenter.default.post(name: API.didReceiveEvent, object: (eventName, eventData))

                self.receiveEventMessage()
            }
        }
    }

    func send(_ string: String) {
        var message = Data()
        let data = string.data(using: .utf8)!
        withUnsafeBytes(of: UInt32(data.count).bigEndian) { message.append(contentsOf: $0) }
        message.append(data)

        conn.send(
            content: message,
            completion: .contentProcessed { error in
                if error != nil {
                    print(error!)
                }
            }
        )
    }

    func cancel() {
        conn.cancel()
    }
}
