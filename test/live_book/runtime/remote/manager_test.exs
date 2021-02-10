defmodule LiveBook.Runtime.Remote.ManagerTest do
  use ExUnit.Case, async: true

  alias LiveBook.Runtime.Remote.Manager

  setup do
    {:ok, pid} = Manager.start()
    :ok
  end

  describe "set_owner/2" do
    test "starts watching the given process and terminates as soon as it terminates" do
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
end
