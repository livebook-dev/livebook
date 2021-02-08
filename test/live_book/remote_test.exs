defmodule LiveBook.RemoteTest do
  use ExUnit.Case, async: true

  alias LiveBook.{Remote, Runtime}

  # All the checks are in a single test,
  # so that we re-use the started runtime node.
  test "initialize/1 and deinitialize/1 dynamically load/unload modules and start/stop a supervision tree" do
    node = start_node()

    # Initially the node has no idea about LiveBook.
    refute evaluator_module_loaded?(node)
    refute supervisor_started?(node)

    Remote.initialize(node)

    # Initialization should load the necessary modules
    # and start the supervision tree.
    assert evaluator_module_loaded?(node)
    assert supervisor_started?(node)

    Remote.initialize(node)

    # Subsequent initialization should have no effect on the environment.
    # It just increases the initialization counter.
    assert evaluator_module_loaded?(node)
    assert supervisor_started?(node)

    Remote.deinitialize(node)

    # As we called initialization twice (e.g. from two distinct sessions),
    # the first deinitialization should only decrease a counter.
    assert evaluator_module_loaded?(node)
    assert supervisor_started?(node)

    Remote.deinitialize(node)

    # Finally the second deinitialization should unload the modules
    # and stop the supervision tree.
    refute evaluator_module_loaded?(node)
    refute supervisor_started?(node)
  end

  defp start_node() do
    {:ok, runtime} = Runtime.Standalone.init(self())
    Runtime.get_node(runtime)
  end

  defp evaluator_module_loaded?(node) do
    :rpc.call(node, :code, :is_loaded, [LiveBook.Evaluator]) != false
  end

  defp supervisor_started?(node) do
    :rpc.call(node, Process, :whereis, [LiveBook.Remote.Supervisor]) != nil
  end
end
