defmodule ElixirKit.PubSub.Test do
  use ExUnit.Case, async: true

  setup_all do
    # Pre-cache Mix.install so that Elixir spawned from Rust tests
    # doesn't print install output, which we test against.
    {_output, 0} =
      System.cmd("elixir", [
        "-e",
        """
        Mix.install([{:elixirkit, path: "#{__DIR__}/../.."}])
        """
      ])

    :ok
  end

  test "bidirectional messaging" do
    start_supervised!({ElixirKit.PubSub, name: :pubsub1, listen: "tcp://127.0.0.1:0"})

    url = ElixirKit.PubSub.url(:pubsub1)
    start_supervised!({ElixirKit.PubSub, name: :pubsub2, connect: url})

    ElixirKit.PubSub.subscribe(:pubsub1, "topic")
    ElixirKit.PubSub.subscribe(:pubsub2, "topic")

    ElixirKit.PubSub.broadcast(:pubsub1, "topic", "message1")
    ElixirKit.PubSub.broadcast(:pubsub2, "topic", "message2")

    assert_receive "message1"
    assert_receive "message2"
    refute_receive _
  end

  test "bidirectional messaging (rust)" do
    port =
      rust(~s"""
      fn main() {
          let pubsub = elixirkit::PubSub::listen("tcp://127.0.0.1:0")
              .expect("failed to listen");

          let pubsub_for_topic1 = pubsub.clone();
          pubsub.subscribe("topic1", move |msg| {
              if msg == b"ping1" {
                  pubsub_for_topic1.broadcast("topic1", b"pong1").unwrap();
              }
          });

          let pubsub_for_topic2 = pubsub.clone();
          pubsub.subscribe("topic2", move |msg| {
              if msg == b"ping2" {
                  pubsub_for_topic2.broadcast("topic2", b"pong2").unwrap();
              }
          });

          let code = r#"
              Mix.install([{:elixirkit, path: "#{__DIR__}/../.."}])

              {:ok, _} =
                ElixirKit.PubSub.start_link(
                  connect: System.fetch_env!("ELIXIRKIT_PUBSUB"),
                  on_exit: fn -> System.stop() end
                )

              ElixirKit.PubSub.subscribe("topic1")
              ElixirKit.PubSub.broadcast("topic1", "ping1")

              ElixirKit.PubSub.subscribe("topic2")
              ElixirKit.PubSub.broadcast("topic2", "ping2")

              receive do
                "pong1" ->
                  IO.puts("got: pong1")
              end

              receive do
                "pong2" ->
                  IO.puts("got: pong2")
              end
          "#;

          let status = elixirkit::elixir(&["-e", code])
              .env("ELIXIRKIT_PUBSUB", pubsub.url())
              .status()
              .expect("failed to start Elixir");

          std::process::exit(status.code().unwrap_or(1));
      }
      """)

    assert_receive {^port, {:data, {:eol, "got: pong1"}}}, 10_000
    assert_receive {^port, {:data, {:eol, "got: pong2"}}}, 10_000
    assert_receive {^port, {:exit_status, 0}}, 10_000
  end

  test "exit status propagates" do
    port =
      rust(~s"""
      fn main() {
          let code = r#"
              System.halt(2)
          "#;

          let status = std::process::Command::new("elixir")
              .args(&["-e", code])
              .status()
              .expect("failed to start Elixir");

          std::process::exit(status.code().unwrap());
      }
      """)

    assert_receive {^port, {:exit_status, 2}}, 10_000
  end

  @tag :tmp_dir
  test "elixir exception", %{tmp_dir: tmp_dir} do
    path = Path.join(tmp_dir, "test.rs")

    File.write!(path, ~s"""
    #!/usr/bin/env cargo +nightly -Zscript
    ---cargo
    [package]
    edition = "2024"

    [dependencies]
    elixirkit = { path = "#{__DIR__}/../../elixirkit_rs" }
    ---

    fn main() {
        let code = r#"
            raise "foo"
        "#;

        let status = std::process::Command::new("elixir")
            .args(&["-e", code])
            .status()
            .expect("failed to start Elixir");

        std::process::exit(status.code().unwrap());
    }
    """)

    {output, 1} = System.cmd("cargo", ["+nightly", "-Zscript", path], stderr_to_stdout: true)
    assert output =~ "** (RuntimeError) foo"
  end

  @tag :tmp_dir
  test "beam exits when rust dies", %{tmp_dir: tmp_dir} do
    path = Path.join(tmp_dir, "test.rs")
    elixir_exited_path = Path.join(tmp_dir, "elixir_exited")

    File.write!(path, ~s"""
    #!/usr/bin/env cargo +nightly -Zscript
    ---cargo
    [package]
    edition = "2024"

    [dependencies]
    elixirkit = { path = "#{__DIR__}/../../elixirkit_rs" }
    ---

    fn main() {
        let pubsub = elixirkit::PubSub::listen("tcp://127.0.0.1:0")
            .expect("failed to listen");

        pubsub.subscribe("messages", move |msg| {
            if msg == b"ready" {
                println!("ready");
            }
        });

        let code = r#"
            Mix.install([{:elixirkit, path: "#{__DIR__}/../.."}])

            {:ok, _} = ElixirKit.PubSub.start_link(connect: System.fetch_env!("ELIXIRKIT_PUBSUB"), on_exit: fn ->
              File.touch!("#{elixir_exited_path}")
            end)

            ElixirKit.PubSub.subscribe("messages")
            ElixirKit.PubSub.broadcast("messages", "ready")

            Process.sleep(:infinity)
        "#;

        let mut child = std::process::Command::new("elixir")
            .args(&["-e", code])
            .env("ELIXIRKIT_PUBSUB", pubsub.url())
            .env("MIX_ENV", "test")
            .spawn()
            .expect("failed to start Elixir");

        child.wait().expect("failed to wait for child");
    }
    """)

    port =
      Port.open(
        {:spawn_executable, System.find_executable("cargo")},
        [:binary, :use_stdio, :exit_status, args: ["+nightly", "-Zscript", path]]
      )

    assert_receive {^port, {:data, "ready\n"}}, 10_000

    {:os_pid, rust_pid} = Port.info(port, :os_pid)
    System.cmd("kill", [to_string(rust_pid)])
    assert_until(fn -> File.exists?(elixir_exited_path) end, 5000, "BEAM did not exit cleanly")
    assert_receive {^port, {:exit_status, _}}
  end

  defp assert_until(check, timeout, message) do
    if check.() do
      :ok
    else
      if timeout > 0 do
        Process.sleep(100)
        assert_until(check, timeout - 100, message)
      else
        flunk(message)
      end
    end
  end

  defp rust(code) do
    hash = :crypto.hash(:md5, code) |> Base.encode16(case: :lower)
    path = Path.join("/tmp", hash)

    File.write!(path, """
    #!/usr/bin/env cargo +nightly -Zscript
    ---cargo
    [package]
    edition = "2024"

    [dependencies]
    elixirkit = { path = "#{__DIR__}/../../elixirkit_rs" }
    ---

    #{code}
    """)

    Port.open(
      {:spawn_executable, System.find_executable("cargo")},
      [:binary, :use_stdio, :exit_status, {:line, 4096}, args: ["+nightly", "-Zscript", path]]
    )
  end
end
