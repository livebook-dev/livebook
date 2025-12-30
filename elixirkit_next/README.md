# ElixirKit

ElixirKit helps running Elixir alongside native applications written in Rust.

🚧 ElixirKit is experimental and subject to change without notice.

See examples:
- [`examples/cli_example`](examples/cli_example) - Minimal CLI that communicates with Elixir
- [`examples/tauri_example`](examples/tauri_example) - Desktop app using Tauri

## Rust API

- `elixirkit::Release::start(path, release_name, env, handler) -> exit_status` - start Elixir release
- `elixirkit::MixTask::start(cwd, task, args, env, handler) -> exit_status` - start Mix task
- `elixirkit::Command::send(&self, (name, data))` - send message to Elixir side
- `handler` is `|&command, (name, data)|`

## Elixir API

- `ElixirKit.start()` - start listener, messages from Rust will be delivered as `{:event, name, data}` tuples
- `ElixirKit.publish(name, data)` - send message to Rust side

# TODO
- Receive `{:message, {name, data}}` messages to match Rust side
- Change to ElixirKit.send({name, data}) to match Rust side
