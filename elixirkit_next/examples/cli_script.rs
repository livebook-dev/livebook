#!/usr/bin/env cargo +nightly -Zscript
---cargo
[package]
edition = "2024"

[dependencies]
elixirkit = { path = "../elixirkit_rs" }
---

fn main() {
    let pubsub = elixirkit::PubSub::listen("tcp://127.0.0.1:0").expect("failed to listen");

    let pubsub_for_echo = pubsub.clone();
    pubsub.subscribe("echo", move |msg| {
        println!("[rust] got: {}", String::from_utf8_lossy(msg));
        pubsub_for_echo.broadcast("echo", b"Hello from Rust!").unwrap();
    });

    let code = r#"
        Mix.install([{:elixirkit, path: "."}])

        {:ok, _} =
          ElixirKit.PubSub.start_link(
            connect: System.fetch_env!("ELIXIRKIT_PUBSUB"),
            on_exit: fn -> System.stop() end
          )

        ElixirKit.PubSub.subscribe("echo")
        ElixirKit.PubSub.broadcast("echo", "Hello from Elixir!")

        receive do
          msg ->
            IO.puts("[elixir] got: #{msg}")
        end
    "#;

    let status = elixirkit::elixir(&["-e", code], &pubsub)
        .status()
        .expect("failed to start Elixir");

    std::process::exit(status.code().unwrap_or(1));
}
