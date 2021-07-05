defmodule Livebook.Runtime.ErlDist.RuntimeServerTest do
  use ExUnit.Case, async: false

  alias Livebook.Runtime.ErlDist.{NodeManager, RuntimeServer}

  setup do
    {:ok, manager_pid} =
      NodeManager.start_link(unload_modules_on_termination: false, anonymous: true)

    runtime_server_pid = NodeManager.start_runtime_server(manager_pid)
    RuntimeServer.set_owner(runtime_server_pid, self())
    {:ok, %{pid: runtime_server_pid}}
  end

  describe "set_owner/2" do
    test "starts watching the given process and terminates as soon as it terminates", %{pid: pid} do
      owner =
        spawn(fn ->
          receive do
            :stop -> :ok
          end
        end)

      RuntimeServer.set_owner(pid, owner)

      # Make sure the node is running.
      assert Process.alive?(pid)
      ref = Process.monitor(pid)

      # Tell the owner process to stop.
      send(owner, :stop)

      # Once the owner process terminates, the node should terminate as well.
      assert_receive {:DOWN, ^ref, :process, _, _}
    end
  end

  describe "evaluate_code/6" do
    test "spawns a new evaluator when necessary", %{pid: pid} do
      RuntimeServer.evaluate_code(pid, "1 + 1", :container1, :evaluation1, nil)

      assert_receive {:evaluation_response, :evaluation1, _, %{evaluation_time_ms: _time_ms}}
    end

    test "prevents from module redefinition warning being printed to standard error", %{pid: pid} do
      stderr =
        ExUnit.CaptureIO.capture_io(:stderr, fn ->
          RuntimeServer.evaluate_code(pid, "defmodule Foo do end", :container1, :evaluation1, nil)
          RuntimeServer.evaluate_code(pid, "defmodule Foo do end", :container1, :evaluation2, nil)

          assert_receive {:evaluation_response, :evaluation1, _, %{evaluation_time_ms: _time_ms}}
          assert_receive {:evaluation_response, :evaluation2, _, %{evaluation_time_ms: _time_ms}}
        end)

      assert stderr == ""
    end

    test "proxies evaluation stderr to evaluation stdout", %{pid: pid} do
      RuntimeServer.evaluate_code(
        pid,
        ~s{IO.puts(:stderr, "error")},
        :container1,
        :evaluation1,
        nil
      )

      assert_receive {:evaluation_output, :evaluation1, "error\n"}
    end

    @tag capture_log: true
    test "proxies logger messages to evaluation stdout", %{pid: pid} do
      code = """
      require Logger
      Logger.error("hey")
      """

      RuntimeServer.evaluate_code(pid, code, :container1, :evaluation1, nil)

      assert_receive {:evaluation_output, :evaluation1, log_message}
      assert log_message =~ "[error] hey"
    end
  end

  describe "request_completion_items/6" do
    test "provides basic completion when no evaluation reference is given", %{pid: pid} do
      RuntimeServer.request_completion_items(pid, self(), :comp_ref, "System.ver", nil, nil)
      assert_receive {:completion_response, :comp_ref, [%{label: "version/0"}]}
    end

    test "provides extended completion when previous evaluation reference is given", %{pid: pid} do
      RuntimeServer.evaluate_code(pid, "number = 10", :c1, :e1, nil)
      assert_receive {:evaluation_response, :e1, _, %{evaluation_time_ms: _time_ms}}

      RuntimeServer.request_completion_items(pid, self(), :comp_ref, "num", :c1, :e1)

      assert_receive {:completion_response, :comp_ref, [%{label: "number"}]}
    end
  end

  test "notifies the owner when an evaluator goes down", %{pid: pid} do
    code = """
    spawn_link(fn -> Process.exit(self(), :kill) end)
    """

    RuntimeServer.evaluate_code(pid, code, :container1, :evaluation1, nil)

    assert_receive {:container_down, :container1, message}
    assert message =~ "killed"
  end
end
