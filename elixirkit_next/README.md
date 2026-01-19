# ElixirKit

ElixirKit helps running Elixir alongside native applications written in Rust.

🚧 ElixirKit is experimental and subject to change without notice.

See examples:
- [`examples/cli_example`](examples/cli_example) - Minimal CLI that communicates with Elixir
- [`examples/tauri_example`](examples/tauri_example) - Desktop app using Tauri

## Rust API

```rust
let command = elixirkit::release(rel_dir, rel_name)

let exit_code = command.start(|(name, data)| {
    match name {
        "echo" => {
            println!("Received: {data}");
        }
        _ => {}
    }
});
```

### API Reference

- `elixirkit::release(rel_dir, rel_name)` - create command for Elixir release
- `elixirkit::Command::new(program, args)` - create command for any executable
- `command.current_dir(path)` - set working directory
- `command.env(&[("KEY", "value")])` - set environment variables
- `command.start(handler) -> exit_code` - start the command and run event loop
- `command.send((name, data))` - send message to Elixir side

## Elixir API

- `ElixirKit.start_link()` - start listener, messages from Rust will be delivered as `{:event, name, data}` tuples
- `ElixirKit.publish(name, data)` - send message to Rust side

# TODO
- Receive `{:message, {name, data}}` messages to match Rust side
- Change to ElixirKit.send({name, data}) to match Rust side
