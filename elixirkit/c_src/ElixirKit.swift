import SwiftUI
import Combine

struct ElixirKit {
    static func start() {
        ElixirKitStart()
    }

    static func startAsync() {
        DispatchQueue.global().async {
            start()
        }
    }

    static func publish(_ name: String, _ data: String) {
        ElixirKitPublish(name, data)
    }

    static func publisher(_ name: String) -> Publishers.Map<NotificationCenter.Publisher, String> {
        NotificationCenter.default
            .publisher(for: NSNotification.Name("ElixirKit:\(name)"))
            .map({ String(describing: $0.object!) })
    }

    static func subscribe(_ name: String, queue: OperationQueue?, _ block: @escaping (String) -> Void) {
        NotificationCenter.default.addObserver(forName: NSNotification.Name("ElixirKit:\(name)"), object: nil, queue: queue) { n in
            block(String(describing: n.object!))
        }
    }
}
