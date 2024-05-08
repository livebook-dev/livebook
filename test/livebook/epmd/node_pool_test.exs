defmodule Livebook.EPMD.NodePoolTest do
  use ExUnit.Case, async: true

  alias Livebook.EPMD.NodePool

  # Note we do not spawn actual nodes as it can be time
  # intensive (on low spec machines) and is generally
  # complicated.

  describe "start_link" do
    test "correctly starts a registered GenServer", config do
      start_supervised!({NodePool, name: config.test})

      # Verify Process is running
      assert Process.whereis(config.test)
    end
  end

  describe "get_name/2" do
    test "creates a new node name if pool is empty", config do
      start_supervised!({NodePool, name: config.test})

      result = NodePool.get_name(config.test)
      assert is_binary(result)
      [name, host] = node() |> Atom.to_string() |> :binary.split("@")
      assert String.starts_with?(result, name)
      assert String.ends_with?(result, "@" <> host)
      assert result != Atom.to_string(node())
    end

    test "returns an existing name if pool is not empty", config do
      start_supervised!({NodePool, name: config.test, buffer_time: 0})

      name = NodePool.get_name(config.test)
      nodedown(config.test, String.to_atom(name))

      assert NodePool.get_name(config.test) == name
    end

    test "removes an existing name when used", config do
      start_supervised!({NodePool, name: config.test, buffer_time: 0})

      name = NodePool.get_name(config.test)
      nodedown(config.test, String.to_atom(name))

      name = NodePool.get_name(config.test)
      assert NodePool.get_name(config.test) != name
    end
  end

  describe "update_name/get_port" do
    test "updates name info and gets port for name", config do
      start_supervised!({NodePool, name: config.test})

      result = NodePool.get_name(config.test)
      assert NodePool.get_port(config.test, result) == 0
      :ok = NodePool.update_name(config.test, result, 12345)
      assert NodePool.get_port(config.test, result) == 12345
    end

    test "erases port info on node down", config do
      start_supervised!({NodePool, name: config.test, buffer_time: 0})

      result = NodePool.get_name(config.test)
      :ok = NodePool.update_name(config.test, result, 12345)
      assert NodePool.get_port(config.test, result) == 12345
      nodedown(config.test, String.to_atom(result))
      assert NodePool.get_port(config.test, result) == 0
    end

    test "returns no port for unknown names", config do
      start_supervised!({NodePool, name: config.test})
      assert NodePool.get_port(config.test, "never-a-name") == 0
    end
  end

  describe "on nodedown" do
    test "does not add node name to pool if not in generated_names", config do
      start_supervised!({NodePool, name: config.test, buffer_time: 0})
      nodedown(config.test, :some_foo)
      assert NodePool.get_name(config.test) != :some_foo
    end
  end

  # Emulate node down and make sure it is processed
  defp nodedown(process, node) when is_atom(node) do
    send(process, {:nodedown, node, {}})

    # Make sure the send was processed
    _ = :sys.get_status(process)
    # Make sure the send after message processed
    _ = :sys.get_status(process)

    :ok
  end
end
