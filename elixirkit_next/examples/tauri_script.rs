#!/usr/bin/env cargo +nightly -Zscript
---cargo
[package]
edition = "2024"

[dependencies]
tauri = "2"
tauri_script = { path = "/Users/wojtek/src/tauri_script" }
elixirkit = { path = "../elixirkit_rs" }
---

fn run() {
    let tauri_conf_json: &str = r#"{
        "$schema": "https://schema.tauri.app/config/2",
        "productName": "tauri-script",
        "version": "0.1.0",
        "identifier": "com.tauri-script",
        "app": {
          "windows": [{"title": "tauri-script", "width": 800, "height": 600, "url": "about:blank", "visible": false}]
        }
    }"#;

    let script_exs = r#"
    Mix.install([
      {:phoenix_playground, "~> 0.1.8"},
      {:elixirkit, path: "."}
    ])

    defmodule DemoLive do
      use Phoenix.LiveView

      @impl true
      def mount(_params, _session, socket) do
        {:ok, assign(socket, count: 0)}
      end

      @impl true
      def render(assigns) do
        ~H"""
        <span>Count: {@count}</span>
        <button phx-click="inc">+</button>
        <div style="margin-top: 1em"><button phx-click="toggle_devtools">Dev Tools</button></div>
        """
      end

      @impl true
      def handle_event("inc", _params, socket) do
        count = socket.assigns.count + 1
        ElixirKit.PubSub.broadcast("messages", "count:" <> Integer.to_string(count))
        {:noreply, assign(socket, count: count)}
      end

      def handle_event("toggle_devtools", _params, socket) do
        ElixirKit.PubSub.broadcast("messages", "toggle_devtools")
        {:noreply, socket}
      end
    end

    defmodule App do
      use GenServer
      require Logger

      def start_link(_) do
        GenServer.start_link(__MODULE__, nil)
      end

      @impl true
      def init(_) do
        ElixirKit.PubSub.subscribe("messages")
        {:ok, %{}}
      end

      @impl true
      def handle_info(msg, state) when is_binary(msg) do
        Logger.info("[elixir] got: #{msg}")
        ElixirKit.PubSub.broadcast("messages", "pong")
        {:noreply, state}
      end
    end

    {:ok, _} = PhoenixPlayground.start(
      live: DemoLive,
      port: 8000,
      live_reload: false,
      open_browser: false,
      child_specs: [
        {ElixirKit.PubSub, connect: System.fetch_env!("ELIXIRKIT_PUBSUB"), on_exit: fn -> System.stop() end},
        App
      ]
    )

    ElixirKit.PubSub.broadcast("messages", "ready")
    "#;

    let pubsub = elixirkit::PubSub::listen("tcp://127.0.0.1:0").expect("failed to listen");

    tauri::Builder::default()
        .setup(move |app| {
            use tauri::Manager;

            let app_handle = app.handle().clone();
            pubsub.subscribe("messages", move |msg| {
                if msg == b"ready" {
                    if let Some(window) = app_handle.get_webview_window("main") {
                        let _ = window.navigate("http://localhost:8000".parse().unwrap());
                        let _ = window.show();
                    }
                } else if msg == b"toggle_devtools" {
                    if let Some(window) = app_handle.get_webview_window("main") {
                        if window.is_devtools_open() {
                            window.close_devtools();
                        } else {
                            window.open_devtools();
                        }
                    }
                } else {
                    println!("[rust] {}", String::from_utf8_lossy(msg));
                }
            });

            let app_handle = app.handle().clone();
            tauri::async_runtime::spawn_blocking(move || {
                let status = elixirkit::elixir(&["-e", script_exs], &pubsub)
                    .status()
                    .expect("failed to start Elixir");

                app_handle.exit(status.code().unwrap_or(1));
            });
            Ok(())
        })
        .run(tauri_script::new(tauri_conf_json).build_context())
        .unwrap();
}

fn main() {
    tauri_script::run(file!(), run);
}
