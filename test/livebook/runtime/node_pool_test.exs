defmodule Livebook.Runtime.NodePoolTest do
  use ExUnit.Case, async: true

  alias Livebook.Runtime.NodePool

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

      result = NodePool.get_name(config.test, node())
      assert is_atom(result)
      assert result |> Atom.to_string() |> String.ends_with?(Atom.to_string(node()))
    end

    test "returns an existing name if pool is not empty", config do
      start_supervised!({NodePool, name: config.test, buffer_time: 0})

      name = NodePool.get_name(config.test, node())
      nodedown(config.test, name)

      assert NodePool.get_name(config.test, node()) == name
    end

    test "removes an existing name when used", config do
      start_supervised!({NodePool, name: config.test, buffer_time: 0})

      name = NodePool.get_name(config.test, node())
      nodedown(config.test, name)

      name = NodePool.get_name(config.test, node())
      assert NodePool.get_name(config.test, node()) != name
    end
  end

  describe "on nodedown" do
    test "does not add node name to pool if not in generated_names", config do
      start_supervised!({NodePool, name: config.test, buffer_time: 0})
      nodedown(config.test, :some_foo)
      assert NodePool.get_name(config.test, node()) != :some_foo
    end
  end

  # Emulate node down and make sure it is processed
  defp nodedown(process, node) do
    send(process, {:nodedown, node, {}})

    # Make sure the send was processed
    _ = :sys.get_status(process)
    # Make sure the send after message processed
    _ = :sys.get_status(process)

    :ok
  end
end
