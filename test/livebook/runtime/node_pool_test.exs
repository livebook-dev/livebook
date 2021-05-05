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
    test "correctly starts a registered GenServer" do
      start_supervised!({NodePool, name: Foo})

      # Verify Process is running
      assert Process.whereis(Foo)
    end
  end

  describe "get_name/2" do
    test "creates a new node name if pool is empty" do
      start_supervised!({NodePool, name: Bar})

      # Assert that we get a result and that it is an atom
      result = NodePool.get_name(Bar, node())
      assert result
      assert is_atom(result)
    end

    test "adds a new node name to generated names when generated" do
      start_supervised!({NodePool, name: Baz, buffer_time: 1})

      result = NodePool.get_name(Baz, node())

      # Mock node down
      send(Baz, {:nodedown, result, {}})
      Process.sleep(2)

      # Check if node name is added calling get_name/2 again
      assert NodePool.get_name(Baz, node()) == result
    end

    test "returns an existing name if pool is not empty" do
      start_supervised!({NodePool, name: Fu, buffer_time: 1})

      name = NodePool.get_name(Fu, node())
      send(Fu, {:nodedown, name, {}})
      Process.sleep(2)

      assert NodePool.get_name(Fu, node()) == name
    end

    test "removes an existing name when used" do
      start_supervised!({NodePool, name: Qux, buffer_time: 1})

      name = NodePool.get_name(Qux, node())
      send(Qux, {:nodedown, name, {}})
      Process.sleep(2)

      name = NodePool.get_name(Qux, node())
      assert NodePool.get_name(Qux, node()) != name
    end
  end

  describe "On nodedown" do
    test "adds node name to pool" do
      start_supervised!({NodePool, name: Quux, buffer_time: 1})

      name = NodePool.get_name(Quux, node())

      # Mock a nodedown
      send(Quux, {:nodedown, name, {}})
      Process.sleep(2)

      # Verify that name is in pool, by calling get_name/2
      assert NodePool.get_name(Quux, node()) == name
    end

    test "does not add node name to pool if not in generated_names" do
      start_supervised!({NodePool, name: Waldo})

      # Mock a nodedown
      send(Waldo, {:nodedown, :some_foo, {}})
      Process.sleep(2)

      # Verify that name is not in pool, by calling get_name/2
      assert NodePool.get_name(Waldo, node()) != :some_foo
    end
  end
end
