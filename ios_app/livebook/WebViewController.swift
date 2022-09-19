//
//  WebView.swift
//
//  Created by Dominic Letz on 19.09.22
//

import Foundation
import Combine

import SwiftUI
import UIKit
import WebKit

struct WebViewController: UIViewRepresentable {
    var webview: WebView
    var lastURL: URL?
    
    init() {
        webview = WebView()
    }

    func makeUIView(context: Context) -> WKWebView {
        print("makeUIView")
        return webview.webview
    }
    
    func updateUIView(_ webView: WKWebView, context: Context) {
        print("updateUIView")
        if lastURL != nil {
            webview.webview.load(URLRequest(url: lastURL!))
        }
    }
    
    func loadURL(url: URL) {
        webview.webview.load(URLRequest(url: url))
    }
}
