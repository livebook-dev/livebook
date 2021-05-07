defmodule Livebook.Runtime.NodePoolTest do
  use ExUnit.Case, async: true

  alias Livebook.Runtime.NodePool

  # Tests for Livebook.Runtime.NodePool
  #
  # Note:
  # 1. We do not spawn actual nodes as it can be time
  #    intensive (on low spec machines)
  #
  # 2. We wait a certain period of time after we mock
  #    a node down, so that the buffer period is spent,
  #    and the node is added to pool.

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

      # Assert that we get a result and that it is an atom
      result = NodePool.get_name(config.test, node())
      assert result
      assert is_atom(result)
    end

    test "returns an existing name if pool is not empty", config do
      start_supervised!({NodePool, name: config.test, buffer_time: 1})

      name = NodePool.get_name(config.test, node())
      send(config.test, {:nodedown, name, {}})
      Process.sleep(2)

      assert NodePool.get_name(config.test, node()) == name
    end

    test "removes an existing name when used", config do
      start_supervised!({NodePool, name: config.test, buffer_time: 1})

      name = NodePool.get_name(config.test, node())
      send(config.test, {:nodedown, name, {}})
      Process.sleep(2)

      name = NodePool.get_name(config.test, node())
      assert NodePool.get_name(config.test, node()) != name
    end
  end

  describe "on nodedown" do
    test "does not add node name to pool if not in generated_names", config do
      start_supervised!({NodePool, name: config.test})

      # Mock a nodedown
      send(config.test, {:nodedown, :some_foo, {}})
      Process.sleep(2)

      # Verify that name is not in pool, by calling get_name/2
      assert NodePool.get_name(config.test, node()) != :some_foo
    end
  end
end
