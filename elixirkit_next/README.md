# ElixirKit

Run Elixir from Rust/Tauri apps and exchange messages over [PubSub].

## Example

On the Rust side, use [`elixirkit::elixir`] to start Elixir and
[`elixirkit::PubSub`] to exchange messages. Subscribe before starting Elixir so
no messages are missed:

```rust
// main.rs
let pubsub = elixirkit::PubSub::listen("tcp://127.0.0.1:0")
    .expect("failed to listen");

let pubsub_for_topic = pubsub.clone();
pubsub.subscribe("topic", move |msg| {
    if msg == b"ping" {
        pubsub_for_topic.broadcast("topic", b"pong").unwrap();
    }
});

let status = elixirkit::elixir(&["script.exs"])
    .env("ELIXIRKIT_PUBSUB", pubsub.url())
    .status()
    .expect("failed to start Elixir");

std::process::exit(status.code().unwrap_or(1));
```

On the Elixir side, start [`ElixirKit.PubSub`] under a supervision tree and use
[`ElixirKit.PubSub.subscribe/1`] to listen to messages and
[`ElixirKit.PubSub.broadcast/2`] to send messages to the Rust side:

```elixir
# script.exs
Mix.install([:elixirkit])

children = [
  {ElixirKit.PubSub,
   connect: System.get_env("ELIXIRKIT_PUBSUB") || :ignore,
   on_exit: fn -> System.stop() end}
]

{:ok, _} = Supervisor.start_link(children, strategy: :one_for_one)

ElixirKit.PubSub.subscribe("topic")
ElixirKit.PubSub.broadcast("topic", "ping")

receive do
  message ->
    IO.puts(["[elixir] ", inspect(message)])
end
```

## License

Copyright (C) 2026 Dashbit

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at [http://www.apache.org/licenses/LICENSE-2.0](http://www.apache.org/licenses/LICENSE-2.0)

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

[PubSub]:                         https://hexdocs.pm/elixirkit/ElixirKit.PubSub.html
[`ElixirKit.PubSub`]:             https://hexdocs.pm/elixirkit/ElixirKit.PubSub.html
[`ElixirKit.PubSub.subscribe/1`]: https://hexdocs.pm/elixirkit/ElixirKit.PubSub.html#subscribe/1
[`ElixirKit.PubSub.broadcast/2`]: https://hexdocs.pm/elixirkit/ElixirKit.PubSub.html#broadcast/2

[`elixirkit_rs`]:                 https://hexdocs.pm/elixirkit/rs/elixirkit/index.html
[`elixirkit::elixir`]:            https://hexdocs.pm/elixirkit/rs/elixirkit/fn.elixir.html
[`elixirkit::PubSub`]:            https://hexdocs.pm/elixirkit/rs/elixirkit/struct.PubSub.html
