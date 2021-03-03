defmodule Livebook.Runtime.ElixirStandaloneTest do
  use ExUnit.Case, async: true

  alias Livebook.Runtime

  describe "init/1" do
    test "starts a new Elixir runtime in distribution mode and ties its lifetime to the Manager process" do
      assert {:ok, %{node: node} = runtime} = Runtime.ElixirStandalone.init()
      Runtime.connect(runtime)

      # Make sure the node is running.
      Node.monitor(node, true)
      assert :pong = Node.ping(node)

      # Tell the owner process to stop.
      Livebook.Runtime.ErlDist.Manager.stop(node)

      # Once Manager terminates, the node should terminate as well.
      assert_receive {:nodedown, ^node}
    end

    test "loads necessary modules and starts manager process" do
      assert {:ok, %{node: node} = runtime} = Runtime.ElixirStandalone.init()
      Runtime.connect(runtime)

      assert evaluator_module_loaded?(node)
      assert manager_started?(node)
    end
  end

  test "Runtime.disconnect/1 makes the node terminate" do
    assert {:ok, %{node: node} = runtime} = Runtime.ElixirStandalone.init()
    Runtime.connect(runtime)

    # Make sure the node is running.
    Node.monitor(node, true)
    assert :pong = Node.ping(node)

    Runtime.disconnect(runtime)

    assert_receive {:nodedown, ^node}
  end

  defp evaluator_module_loaded?(node) do
    :rpc.call(node, :code, :is_loaded, [Livebook.Evaluator]) != false
  end

  defp manager_started?(node) do
    :rpc.call(node, Process, :whereis, [Livebook.Runtime.ErlDist.Manager]) != nil
  end
end
