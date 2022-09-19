import SwiftUI
import UIKit
import WebKit

struct ContentView: View {
    @State var isActive : Bool = false
    @State var webview : WebViewController?
    
    var body: some View {
        VStack {
            if self.isActive {
                webview!.ignoresSafeArea()
            } else {
                ZStack {
                    Color(red: 47/255, green: 36/255, blue: 58/255)
                        .ignoresSafeArea()
                    Image("Logo")
                        .resizable()
                        .aspectRatio(contentMode: .fit)
                }
            }
        }
        //.ignoresSafeArea(edges: .top)
        .onAppear {
            DispatchQueue.main.async {
                let bridge = try! Bridge()
                self.webview = WebViewController()
                self.webview?.webview.onFinish {
                    self.isActive = true
                }
                bridge.setWebView(view: self.webview!)
            }
        }
    }
}

struct ContentView_Previews: PreviewProvider {
    static var previews: some View {
        ContentView()
    }
}
