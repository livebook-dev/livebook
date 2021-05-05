defmodule Livebook.Runtime.NodePoolTest do
  use ExUnit.Case, async: true

  alias Livebook.Runtime.NodePool

  # Tests for Livebook.Runtime.NodePool
  #
  # Note:
  #   We do not spawn actual nodes as it can be time
  #   intensive (on low spec machines)

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
      start_supervised!({NodePool, name: Baz})

      result = NodePool.get_name(Baz, node())

      assert generated_contains?(Baz, result)
    end

    test "returns an existing name if pool is not empty" do
      start_supervised!({NodePool, name: Fu})

      name = NodePool.get_name(Fu, node())
      send(Fu, {:add_node, name})

      assert NodePool.get_name(Fu, node()) == name
    end

    test "removes an existing name when used" do
      start_supervised!({NodePool, name: Qux})

      name = NodePool.get_name(Qux, node())
      send(Qux, {:add_node, name})

      NodePool.get_name(Qux, node())
      assert not free_contains?(Qux, name)
    end
  end

  describe "On nodedown" do
    test "adds node name to pool" do
      start_supervised!({NodePool, name: Quux})

      name = NodePool.get_name(Quux, node())

      # Mock a nodedown
      send(Quux, {:add_node, name})

      # Verify that name is in pool
      assert free_contains?(Quux, name)
    end

    test "does not add node name to pool if not in generated_names" do
      start_supervised!({NodePool, name: Waldo})

      # Mock a nodedown
      send(Waldo, {:add_node, :some_foo})

      assert not free_contains?(Waldo, :some_foo)
    end
  end

  defp free_contains?(name, atom) do
    state = :sys.get_state(Process.whereis(name))
    Enum.member?(state.free_names, atom)
  end

  defp generated_contains?(name, atom) do
    state = :sys.get_state(Process.whereis(name))
    Enum.member?(state.generated_names, atom)
  end
end
