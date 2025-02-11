# ElixirKit

ElixirKit helps running Elixir alongside native desktop applications on macOS and Windows.

🚧 ElixirKit is experimental and subject to change without notice.

See [demo](demo) project, in particular [demo/lib/demo.ex](demo/lib/demo.ex),
[demo/rel/appkit](demo/rel/appkit), and [demo/rel/winforms](demo/rel/winforms).

## ElixirKit for macOS (Swift)

- `ElixirKit.API.isRunning: Bool`
  - Returns whether the Elixir runtime is currently running

- `ElixirKit.API.start(name: String, logPath: String?, readyHandler: () -> Void, terminationHandler: ((Process) -> Void)?) -> Void`
  - Starts the Elixir runtime

- `ElixirKit.API.publish(_ name: String, _ data: String) -> Void`
  - Publishes an event to the Elixir runtime

- `ElixirKit.API.stop() -> Void`
  - Stops the Elixir runtime

- `ElixirKit.API.waitUntilExit() -> Void`
  - Blocks until the Elixir runtime exits

- `ElixirKit.API.addObserver(queue: OperationQueue?, using: ((String, String)) -> Void) -> Void`
  - Adds an observer for events from the Elixir runtime

## ElixirKit for Windows (C#)

- `ElixirKit.API.HasExited: bool` - Indicates whether the Elixir runtime has exited

- `ElixirKit.API.IsMainInstance(string id) -> bool`
  - Checks if the current app is the main instance

- `ElixirKit.API.Start(string name, ReadyHandler ready, ExitHandler? exited = null, string? logPath = null) -> void`
  - Starts the Elixir runtime

- `ElixirKit.API.Publish(string name, string data) -> void`
  - Publishes an event to the Elixir runtime

- `ElixirKit.API.Subscribe(EventHandler handler) -> void`
  - Subscribes to events from the Elixir runtime

- `ElixirKit.API.Stop() -> int`
  - Stops the Elixir runtime
  - Returns exit code

- `ElixirKit.API.WaitForExit() -> int`
  - Blocks until the Elixir runtime exits
  - Returns exit code

- `ElixirKit.ReadyHandler:() -> void`
  - Callback for when the runtime is ready

- `ElixirKit.ExitHandler:(int ExitCode) -> void`
  - Callback for when the runtime exits

- `ElixirKit.EventHandler:(string Name, string Data) -> void`
  - Callback for handling events from the runtime
