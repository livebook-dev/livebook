defmodule LiveBook.Runtime.Remote.ManagerTest do
  use ExUnit.Case, async: false

  alias LiveBook.Runtime.Remote.{Manager, EvaluatorSupervisor}

  describe "set_owner/2" do
    test "starts watching the given process and terminates as soon as it terminates" do
      Manager.start()

      owner =
        spawn(fn ->
          receive do
            :stop -> :ok
          end
        end)

      Manager.set_owner(node(), owner)

      # Make sure the node is running.
      assert Process.whereis(LiveBook.Runtime.Remote.Manager) != nil
      ref = Process.monitor(LiveBook.Runtime.Remote.Manager)

      # Tell the owner process to stop.
      send(owner, :stop)

      # Once the owner process terminates, the node should terminate as well.
      assert_receive {:DOWN, ^ref, :process, _, _}
    end
  end

  describe "evaluate_code/2" do
    test "spawns a new evaluator when necessary" do
      Manager.start()
      Manager.set_owner(node(), self())
      Manager.evaluate_code(node(), "1 + 1", :container1, :evaluation1)

      assert_receive {:evaluation_response, :evaluation1, {:ok, 2}}

      Manager.stop(node())
    end
  end
end
