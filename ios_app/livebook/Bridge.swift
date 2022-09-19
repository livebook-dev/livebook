//
//  Bridge.swift
//
//  Created by Dominic Letz on 19.09.22
//

import Foundation
import Network
import ZIPFoundation
import SwiftUI

class Bridge {
    var webview: WebViewController?
    let listener: NWListener
    let home: URL
    var lastURL: URL?
    
    func setWebView(view :WebViewController) {
        self.webview = view
        loadURL()
    }
    
    func setURL(url :String) {
        lastURL = URL(string: url)
        loadURL()
    }
    
    func loadURL() {
        if let view = self.webview {
            if let url = self.lastURL {
                print ("opening \(url)")
                view.loadURL(url: url)
            }
        }
    }

    private var connectionsByID: [Int: ServerConnection] = [:]

    init() throws {
        home = FileManager.default.urls(for: .libraryDirectory, in: .userDomainMask)[0].appendingPathComponent(Bundle.main.bundleIdentifier!)
        listener = try! NWListener(using: .tcp, on: NWEndpoint.Port.any)
        listener.stateUpdateHandler = self.stateDidChange(to:)
        listener.newConnectionHandler = self.didAccept(nwConnection:)
        
        // Extracting the app
        let infoAttr = try FileManager.default.attributesOfItem(atPath: zipFile().path)
        let infoDate = infoAttr[FileAttributeKey.creationDate] as! Date
        let build = UserDefaults.standard.string(forKey: "app_build_date")
        
        print("Preparing app files \(infoDate.description) installed: \(String(describing: build))")

        let appdir = home.appendingPathComponent("app")
        let info = appdir.appendingPathComponent("releases").appendingPathComponent("start_erl.data")
        
        if (!FileManager.default.fileExists(atPath: info.path)) {
            try unzipApp(dest: appdir)
        } else if (infoDate.description != build){
            try FileManager.default.removeItem(atPath: appdir.path)
            try unzipApp(dest: appdir)
            UserDefaults.standard.set(infoDate.description, forKey: "app_build_date")
        }
        
        let versions = try String(contentsOfFile: info.path)
        let rel = versions.components(separatedBy: .whitespaces)[1]
        setEnv(name: "RELEASE_SYS_CONFIG", value: appdir.appendingPathComponent("releases/\(rel)/sys").path)
        setEnv(name: "RELEASE_ROOT", value: appdir.path)

        let inet_rc = appdir.appendingPathComponent("inetrc")
        setEnv(name: "ERL_INETRC", value: inet_rc.path)

        let rc = #"""
        %% enable EDNS, 0 means enable YES!
        {edns, 0}.
        {alt_nameserver, {8,8,8,8}}.
        {lookup, [dns]}.
        """#
        print("'\(rc)'")
        try! rc.write(to: inet_rc, atomically: true, encoding: .utf8)

        print("Server starting...")
        listener.start(queue: .global())
    }
    
    func setEnv(name: String, value: String) {
        print("setenv \(name) \(value)")
        setenv(name, value, 0)
    }
    
    func zipFile() -> URL {
        return Bundle.main.url(forResource: "app", withExtension: "zip")!
    }
    
    func unzipApp(dest: URL) throws {
        try FileManager.default.createDirectory(at: dest, withIntermediateDirectories: true, attributes: nil)
        try FileManager.default.unzipItem(at: zipFile(), to: dest)
    }

    func stateDidChange(to newState: NWListener.State) {
        switch newState {
        case .ready:
            print("Bridge Server ready. Starting Elixir")
            setEnv(name: "ELIXIR_DESKTOP_OS", value: "ios");
            setEnv(name: "BRIDGE_PORT", value: (listener.port?.rawValue.description)!);
            // not really the home directory, but persistent between app upgrades (yes?)
            setEnv(name: "HOME", value: home.path)
            // BINDIR not used on iOS but needs to be defined
            let bindir = home.appendingPathComponent("bin")
            setEnv(name: "BINDIR", value: bindir.path)
            
            let urls = FileManager.default.urls(for: .documentDirectory, in: .userDomainMask)
            let logdir = urls[0].path
            let appdir = home.appendingPathComponent("app")
            let ret = start_erlang(appdir.path, logdir)
            print("Ret: " + String(cString: ret!))

        case .failed(let error):
            print("Server failure, error: \(error.localizedDescription)")
            exit(EXIT_FAILURE)
        default:
            break
        }
    }
    
    private func didAccept(nwConnection: NWConnection) {
        let connection = ServerConnection(nwConnection: nwConnection, bridge: self)
        self.connectionsByID[connection.id] = connection
        connection.didStopCallback = { _ in
            self.connectionDidStop(connection)
        }
        connection.start()
        // connection.send(data: "Welcome you are connection: \(connection.id)".data(using: .utf8)!)
        print("server did open connection \(connection.id)")
    }
    
    private func connectionDidStop(_ connection: ServerConnection) {
        self.connectionsByID.removeValue(forKey: connection.id)
        print("server did close connection \(connection.id)")
    }

    private func stop() {
        self.listener.stateUpdateHandler = nil
        self.listener.newConnectionHandler = nil
        self.listener.cancel()
        for connection in self.connectionsByID.values {
            connection.didStopCallback = nil
            connection.stop()
        }
        self.connectionsByID.removeAll()
    }
}

class ServerConnection {
    //The TCP maximum package size is 64K 65536
    let MTU = 65536

    private static var nextID: Int = 0
    let connection: NWConnection
    let id: Int
    var bridge: Bridge

    init(nwConnection: NWConnection, bridge: Bridge) {
        self.bridge = bridge
        connection = nwConnection
        id = ServerConnection.nextID
        ServerConnection.nextID += 1
    }

    var didStopCallback: ((Error?) -> Void)? = nil

    func start() {
        connection.stateUpdateHandler = self.stateDidChange(to:)
        setupReceive()
        connection.start(queue: .main)
    }

    private func stateDidChange(to state: NWConnection.State) {
        switch state {
        case .waiting(let error):
            connectionDidFail(error: error)
        case .failed(let error):
            connectionDidFail(error: error)
        case .ready:
            break
        default:
            break
        }
    }

    private func setupReceive() {
        connection.receive(minimumIncompleteLength: 4, maximumLength: 4) { (data, _, isComplete, error) in
            if isComplete {
                self.connectionDidEnd()
                return
            }
            
            if let error = error {
                self.connectionDidFail(error: error)
                return
            }
            
            let length: Int = Int(CFSwapInt32(data!.uint32))
            self.connection.receive(minimumIncompleteLength: length, maximumLength: length) { (datain, _, isComplete, error) in
                if isComplete {
                    self.connectionDidEnd()
                    return
                }
                
                if let error = error {
                    self.connectionDidFail(error: error)
                    return
                }
                
                let ref = datain!.prefix(8)
                let data = datain!.dropFirst(8)
                let json = try! JSONSerialization.jsonObject(with: data, options: [])
                
                let array = json as! [Any]
                //let module = array[0] as? String
                let method = array[1] as? String
                let args = array[2] as? [Any]
                
                //print ("received \(method)")
                if (method == ":loadURL") {
                    self.bridge.setURL(url: args![0] as! String)
                }

                var response = ref
                if (method == ":getOsDescription") {
                    response.append(self.dataToList(string: "iOS \(UIDevice().model)"))
                } else if (method == ":getCanonicalName") {
                    response.append(self.dataToList(string: "en_en"))
                } else {
                    response.append(":ok".data(using: .utf8)!)
                }
                        
                
                let size: UInt32 = CFSwapInt32(UInt32(response.count))
                var message = withUnsafeBytes(of: size) { Data($0) }
                message.append(response)
                self.send(data: message)
                self.setupReceive()
            }
        }
    }
    
    func dataToList(string: String) -> Data {
        return dataToList(data: string.data(using: .utf8)!)
    }
    func dataToList(data: Data) -> Data {
        let numbers = data.map { "\($0)" }
        return "[\(numbers.joined(separator: ","))]".data(using: .utf8)!
    }
    
    func send(data: Data) {
        self.connection.send(content: data, completion: .contentProcessed( { error in
            if let error = error {
                self.connectionDidFail(error: error)
                return
            }
        }))
    }

    func stop() {
        print("connection \(id) will stop")
    }

    private func connectionDidFail(error: Error) {
        print("connection \(id) did fail, error: \(error)")
        stop(error: error)
    }

    private func connectionDidEnd() {
        print("connection \(id) did end")
        stop(error: nil)
    }

    private func stop(error: Error?) {
        connection.stateUpdateHandler = nil
        connection.cancel()
        if let didStopCallback = didStopCallback {
            self.didStopCallback = nil
            didStopCallback(error)
        }
    }
}

extension Data {
    var uint32: UInt32 {
        get {
            let i32array = self.withUnsafeBytes { $0.load(as: UInt32.self) }
            return i32array
        }
    }
}
