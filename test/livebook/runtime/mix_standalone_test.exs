defmodule Livebook.Runtime.MixStandaloneTest do
  use ExUnit.Case, async: true

  alias Livebook.Runtime

  test "integration" do
    # Start node initialization
    project_path = Path.expand("../../support/project", __DIR__)
    emitter = Livebook.Utils.Emitter.new(self())
    runtime = Runtime.MixStandalone.new(project_path)
    Runtime.MixStandalone.connect_async(runtime, emitter)

    ref = emitter.ref
    # Wait for the Mix setup to finish and for node initialization
    assert_receive {:emitter, ^ref, {:output, "Running mix deps.get...\n"}}, 15_000
    assert_receive {:emitter, ^ref, {:ok, runtime}}, 15_000

    Runtime.take_ownership(runtime)
    %{node: node} = runtime

    # Make sure the node is running.
    Node.monitor(node, true)
    assert :pong = Node.ping(node)

    # Ensure the initialization works
    assert evaluator_module_loaded?(node)
    assert manager_started?(node)

    # Ensure modules from the Mix project are available
    assert :rpc.call(node, Project, :hello, []) == "hello"

    # Stopping the runtime should also terminate the node
    Runtime.disconnect(runtime)
    assert_receive {:nodedown, ^node}
  end

  defp evaluator_module_loaded?(node) do
    :rpc.call(node, :code, :is_loaded, [Livebook.Runtime.Evaluator]) != false
  end

  defp manager_started?(node) do
    :rpc.call(node, Process, :whereis, [Livebook.Runtime.ErlDist.NodeManager]) != nil
  end
end
