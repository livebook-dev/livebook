defmodule Livebook.Runtime.ErlDist.ManagerTest do
  use ExUnit.Case, async: false

  alias Livebook.Runtime.ErlDist.Manager

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
      assert Process.whereis(Livebook.Runtime.ErlDist.Manager) != nil
      ref = Process.monitor(Livebook.Runtime.ErlDist.Manager)

      # Tell the owner process to stop.
      send(owner, :stop)

      # Once the owner process terminates, the node should terminate as well.
      assert_receive {:DOWN, ^ref, :process, _, _}
    end
  end

  describe "evaluate_code/6" do
    test "spawns a new evaluator when necessary" do
      Manager.start()
      Manager.set_owner(node(), self())
      Manager.evaluate_code(node(), "1 + 1", :container1, :evaluation1)

      assert_receive {:evaluation_response, :evaluation1, _}

      Manager.stop(node())
    end

    test "prevents from module redefinition warning being printed to standard error" do
      Manager.start()
      Manager.set_owner(node(), self())

      stderr =
        ExUnit.CaptureIO.capture_io(:stderr, fn ->
          Manager.evaluate_code(node(), "defmodule Foo do end", :container1, :evaluation1)
          Manager.evaluate_code(node(), "defmodule Foo do end", :container1, :evaluation2)

          assert_receive {:evaluation_response, :evaluation1, _}
          assert_receive {:evaluation_response, :evaluation2, _}
        end)

      assert stderr == ""

      Manager.stop(node())
    end

    test "proxies evaluation stderr to stdout" do
      Manager.start()
      Manager.set_owner(node(), self())

      Manager.evaluate_code(node(), ~s{IO.puts(:stderr, "error")}, :container1, :evaluation1)

      assert_receive {:evaluation_stdout, :evaluation1, "error\n"}

      Manager.stop(node())
    end
  end

  @tag capture_log: true
  test "notifies the owner when an evaluator goes down" do
    Manager.start()
    Manager.set_owner(node(), self())

    code = """
    spawn_link(fn -> raise "sad cat" end)
    """

    Manager.evaluate_code(node(), code, :container1, :evaluation1)

    assert_receive {:container_down, :container1, message}
    assert message =~ "sad cat"

    Manager.stop(node())
  end
end
