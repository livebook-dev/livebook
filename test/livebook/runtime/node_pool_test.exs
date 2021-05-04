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
      result = NodePool.get_name(node(), Bar)
      assert result
      assert is_atom(result)
    end

    test "adds a new node name to given pool when generated" do
      start_supervised!({NodePool, name: Baz})

      result = NodePool.get_name(node(), Baz)

      assert given_contains?(Baz, result)
    end

    test "returns an existing name if pool is not empty" do
      start_supervised!({NodePool, name: Quux, given_pool: [:foobar], own_pool: [:foobar]})
      assert NodePool.get_name(node(), Quux) == :foobar
    end

    test "removes an existing name when used" do
      start_supervised!({NodePool, name: Qux, given_pool: [:foobar], own_pool: [:foobar]})

      NodePool.get_name(node(), Qux)
      assert own_empty?(Qux)
    end
  end

  describe "On nodedown" do
    test "adds node name to pool if in given" do
      start_supervised!({NodePool, name: Quux, given_pool: [:janfu], buffer_time: 1})

      # Mock a nodedown
      send(Quux, {:add_node, :janfu})
      Process.sleep(5)

      # Verify that atom `:janfu` is in pool
      assert own_contains?(Quux, :janfu)
    end

    test "does not add node name to pool if not in given" do
      start_supervised!({NodePool, name: Waldo, buffer_time: 1})

      # Mock a nodedown
      send(Waldo, {:add_node, :some_foo})
      Process.sleep(5)

      assert not own_contains?(Waldo, :some_foo)
    end
  end

  defp own_empty?(name) do
    state = :sys.get_state(Process.whereis(name))
    Enum.empty?(state.own_pool)
  end

  defp own_contains?(name, atom) do
    state = :sys.get_state(Process.whereis(name))
    Enum.member?(state.own_pool, atom)
  end

  defp given_contains?(name, atom) do
    state = :sys.get_state(Process.whereis(name))
    Enum.member?(state.given_pool, atom)
  end
end
