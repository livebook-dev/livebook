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

  describe "start" do
    test "/0 correctly starts a registered GenServer" do
      NodePool.start()

      # Verify Process is running
      assert Process.whereis(NodePool)

      GenServer.stop(NodePool)
    end

    test "/1 correctly starts a registered GenServer with correct buffer time" do
      NodePool.start(1)

      # Verify Process is running
      assert Process.whereis(NodePool)

      # Verify buffer time
      state = :sys.get_state(NodePool)
      assert state.buffer_time == 1

      GenServer.stop(NodePool)
    end
  end

  describe "get_name/1" do
    test "creates a new node name if pool is empty" do
      NodePool.start()

      # Verify pool is empty
      assert empty?()

      # Assert that we get a result and that it is an atom
      result = NodePool.get_name(node())
      assert result
      assert is_atom(result)
      GenServer.stop(NodePool)
    end

    test "returns an existing name if pool is not empty" do
      NodePool.start(1)

      # Mock a node down
      send(NodePool, {:nodedown, :foobar})
      Process.sleep(5)

      assert contains?(:foobar)

      assert NodePool.get_name(node()) == :foobar
      GenServer.stop(NodePool)
    end
  end

  test "On nodeup removes pooled name accordingly" do
    NodePool.start(1)

    # Mock a node down and up
    send(NodePool, {:nodedown, :foobar})
    Process.sleep(5)
    send(NodePool, {:nodeup, :foobar})

    # Assert that atom `:foobar` is not in pool
    assert not contains?(:foobar)

    GenServer.stop(NodePool)
  end

  describe "On nodedown" do
    test "adds node name to pool" do
      NodePool.start(1)

      # Mock a nodedown
      send(NodePool, {:nodedown, :janfu})
      Process.sleep(5)

      # Verify that atom `:janfu` is in pool
      assert contains?(:janfu)

      GenServer.stop(NodePool)
    end

    test "adds node name to pool only after given buffer time" do
      # Start pool with buffer as 0.1 seconds
      NodePool.start(100)

      # Spawn node and assert still empty
      send(NodePool, {:nodedown, :snafu})
      assert empty?()

      # Wait and check again
      Process.sleep(100)
      assert contains?(:snafu)

      GenServer.stop(NodePool)
    end
  end

  defp empty? do
    state = :sys.get_state(NodePool)
    Enum.empty?(state.pool)
  end

  defp contains?(atom) do
    state = :sys.get_state(NodePool)
    Enum.member?(state.pool, atom)
  end
end
