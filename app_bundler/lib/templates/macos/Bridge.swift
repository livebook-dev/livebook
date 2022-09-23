import Foundation
import Network
import SwiftUI

class Bridge {
    let listener: NWListener
    private var connectionsByID: [Int: ServerConnection] = [:]

    init() throws {
        listener = try! NWListener(using: .tcp, on: NWEndpoint.Port.any)
        listener.stateUpdateHandler = self.stateDidChange(to:)
        listener.newConnectionHandler = self.didAccept(nwConnection:)
        listener.start(queue: .global())
    }
    
    func stateDidChange(to newState: NWListener.State) {
        switch newState {
        case .ready:
            print("Bridge Server ready. Starting Elixir")
            setEnv(name: "BRIDGE_PORT", value: (listener.port?.rawValue.description)!);
            startRelease("open_app")
        case .failed(let error):
            print("Server failure, error: \(error.localizedDescription)")
            exit(EXIT_FAILURE)
        default:
            break
        }
    }

    func sendToProcess(name: String, payload: String) {
        let json = "{\":payload\": \(payload), \":pid\": \"\(name)\"}"
        print("sending \(json)")
        let ref: UInt64 = CFSwapInt64(2)
        var response = withUnsafeBytes(of: ref) { Data($0) }
        response.append(json.data(using: .utf8)!)

        let size: UInt32 = CFSwapInt32(UInt32(response.count))
        var message = withUnsafeBytes(of: size) { Data($0) }
        message.append(response)
        self.send(data: message)
    }

    private func send(data: Data) {
        print("send")
        for connection in self.connectionsByID.values {
            print("send1")
            connection.send(data: data)
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
                let json = try! JSONSerialization.jsonObject(with: data, options: 0)
                
                let array = json as! [Any]
                let module = array[0] as? String
                let method = array[1] as? String
                let args = array[2] as? [Any]
            
                var response = ref
                response.append(self.executeMethod(module!, method!, args!).data(using: .utf8)!)

                let size: UInt32 = CFSwapInt32(UInt32(response.count))
                var message = withUnsafeBytes(of: size) { Data($0) }
                message.append(response)
                self.send(data: message)
                self.setupReceive()
            }
        }
    }

    func executeMethod(_: String, _: String, _: [Any]) -> String {
        // todo
        return ":ok"
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