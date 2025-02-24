defmodule Livebook.Runtime.ErlDist.RuntimeServerTest do
  use ExUnit.Case, async: true

  alias Livebook.Runtime.ErlDist.NodeManager
  alias Livebook.Runtime.ErlDist.RuntimeServer

  setup ctx do
    {:ok, runtime_server_pid} = NodeManager.start_runtime_server(node(), ctx[:opts] || [])
    RuntimeServer.attach(runtime_server_pid, self())
    {:ok, %{pid: runtime_server_pid}}
  end

  defmacrop terminal_text(text, chunk \\ false) do
    quote do
      %{type: :terminal_text, text: unquote(text), chunk: unquote(chunk)}
    end
  end

  describe "attach/2" do
    test "starts watching the given process and terminates as soon as it terminates" do
      owner =
        spawn(fn ->
          receive do
            :stop -> :ok
          end
        end)

      {:ok, pid} = NodeManager.start_runtime_server(node())
      RuntimeServer.attach(pid, owner)

      # Make sure the node is running.
      assert Process.alive?(pid)
      ref = Process.monitor(pid)

      # Tell the owner process to stop.
      send(owner, :stop)

      # Once the owner process terminates, the node should terminate as well.
      assert_receive {:DOWN, ^ref, :process, _, _}
    end
  end

  describe "evaluate_code/5" do
    test "spawns a new evaluator when necessary", %{pid: pid} do
      RuntimeServer.evaluate_code(pid, :elixir, "1 + 1", {:c1, :e1}, [])

      assert_receive {:runtime_evaluation_response, :e1, _, %{evaluation_time_ms: _time_ms}}
    end

    test "prevents from module redefinition warning being printed to standard error", %{pid: pid} do
      stderr =
        ExUnit.CaptureIO.capture_io(:stderr, fn ->
          code = "defmodule Foo do end"
          RuntimeServer.evaluate_code(pid, :elixir, code, {:c1, :e1}, [])
          RuntimeServer.evaluate_code(pid, :elixir, code, {:c1, :e2}, [])

          assert_receive {:runtime_evaluation_response, :e1, _, %{evaluation_time_ms: _time_ms}}
          assert_receive {:runtime_evaluation_response, :e2, _, %{evaluation_time_ms: _time_ms}}
        end)

      refute stderr =~ "redefining module Foo"
    end

    test "proxies evaluation stderr to evaluation stdout", %{pid: pid} do
      RuntimeServer.evaluate_code(
        pid,
        :elixir,
        ~s{IO.puts(:stderr, "error to stdout")},
        {:c1, :e1},
        []
      )

      assert_receive {:runtime_evaluation_output, :e1, terminal_text(output, true)}

      assert output =~ "error to stdout\n"
    end

    @tag capture_log: true
    test "proxies logger messages to evaluation stdout", %{pid: pid} do
      code = """
      require Logger
      Logger.error("hey")
      """

      RuntimeServer.evaluate_code(pid, :elixir, code, {:c1, :e1}, [])

      assert_receive {:runtime_evaluation_output, :e1, terminal_text(log_message, true)}

      assert log_message =~ "[error] hey"

      # Stop the server explicitly to make sure all logger messages are
      # sent by the time the test ends and stops capturing logs
      RuntimeServer.stop(pid)
    end

    test "supports cross-container evaluation context references", %{pid: pid} do
      RuntimeServer.evaluate_code(pid, :elixir, "x = 1", {:c1, :e1}, [])
      assert_receive {:runtime_evaluation_response, :e1, _, %{evaluation_time_ms: _time_ms}}

      RuntimeServer.evaluate_code(pid, :elixir, "x", {:c2, :e2}, [{:c1, :e1}])

      assert_receive {:runtime_evaluation_response, :e2, terminal_text("\e[34m1\e[0m"),
                      %{evaluation_time_ms: _time_ms}}
    end

    test "evaluates code in different containers in parallel", %{pid: pid} do
      # Start a process that waits for two joins and only then
      # sends a response back to the callers and terminates
      code = """
      loop = fn loop, state ->
        receive do
          {:join, caller} ->
            state = update_in(state.count, &(&1 + 1))
            state = update_in(state.callers, &[caller | &1])

            if state.count < 2 do
              loop.(loop, state)
            else
              for caller <- state.callers do
                send(caller, :join_ack)
              end
            end
        end
      end

      pid = spawn(fn -> loop.(loop, %{callers: [], count: 0}) end)
      """

      RuntimeServer.evaluate_code(pid, :elixir, code, {:c1, :e1}, [])
      assert_receive {:runtime_evaluation_response, :e1, _, %{evaluation_time_ms: _time_ms}}

      await_code = """
      send(pid, {:join, self()})

      receive do
        :join_ack -> :ok
      end
      """

      # Note: it's important to first start evaluation in :c2,
      # because it needs to copy evaluation context from :c1

      RuntimeServer.evaluate_code(pid, :elixir, await_code, {:c2, :e2}, [{:c1, :e1}])
      RuntimeServer.evaluate_code(pid, :elixir, await_code, {:c1, :e3}, [{:c1, :e1}])

      assert_receive {:runtime_evaluation_response, :e2, _, %{evaluation_time_ms: _time_ms}}
      assert_receive {:runtime_evaluation_response, :e3, _, %{evaluation_time_ms: _time_ms}}
    end
  end

  describe "handle_intellisense/6 given completion request" do
    test "provides basic completion when no evaluation reference is given", %{pid: pid} do
      request = {:completion, "System.ver"}
      ref = RuntimeServer.handle_intellisense(pid, self(), request, [], nil)

      assert_receive {:runtime_intellisense_response, ^ref, ^request,
                      %{items: [%{label: "version/0"}]}}
    end

    test "provides extended completion when previous evaluation reference is given", %{pid: pid} do
      code = """
      alias IO.ANSI
      number = 10
      """

      RuntimeServer.evaluate_code(pid, :elixir, code, {:c1, :e1}, [])
      assert_receive {:runtime_evaluation_response, :e1, _, %{evaluation_time_ms: _time_ms}}

      request = {:completion, "num"}

      ref =
        RuntimeServer.handle_intellisense(pid, self(), request, [{:c1, :e1}], nil)

      assert_receive {:runtime_intellisense_response, ^ref, ^request,
                      %{items: [%{label: "number"}]}}

      request = {:completion, "ANSI.brigh"}

      ref =
        RuntimeServer.handle_intellisense(pid, self(), request, [{:c1, :e1}], nil)

      assert_receive {:runtime_intellisense_response, ^ref, ^request,
                      %{items: [%{label: "bright/0"}]}}
    end
  end

  describe "handle_intellisense/6 given details request" do
    test "responds with identifier details", %{pid: pid} do
      request = {:details, "System.version", 10}
      ref = RuntimeServer.handle_intellisense(pid, self(), request, [], nil)

      assert_receive {:runtime_intellisense_response, ^ref, ^request,
                      %{range: %{from: 1, to: 15}, contents: [_]}}
    end
  end

  describe "handle_intellisense/6 given format request" do
    test "responds with a formatted code", %{pid: pid} do
      request = {:format, "System.version"}
      ref = RuntimeServer.handle_intellisense(pid, self(), request, [], nil)

      assert_receive {:runtime_intellisense_response, ^ref, ^request, %{code: "System.version()"}}
    end
  end

  describe "read_file/2" do
    test "returns file contents when the file exists", %{pid: pid} do
      assert {:ok, _} = RuntimeServer.read_file(pid, __ENV__.file)
    end

    test "returns an error when the file does not exist", %{pid: pid} do
      assert {:error, "no such file or directory"} =
               RuntimeServer.read_file(pid, "/definitly_non_existent/file/path")
    end
  end

  @tag capture_log: true
  test "notifies the owner when an evaluator goes down", %{pid: pid} do
    code = """
    Task.async(fn -> raise "error" end)
    """

    RuntimeServer.evaluate_code(pid, :elixir, code, {:c1, :e1}, [])

    assert_receive {:runtime_container_down, :c1, message}
    assert message =~ "(RuntimeError) error"

    # Stop the server explicitly to make sure all logger messages are
    # sent by the time the test ends and stops capturing logs
    RuntimeServer.stop(pid)
  end

  describe "smart cells" do
    defmodule Kino.DumbCell do
      use GenServer

      # Every smart cell needs a child_spec, we use the GenServer default

      def start_link(info) do
        {:ok, pid} = GenServer.start_link(__MODULE__, info)

        {:ok, pid,
         %{
           source: "source",
           js_view: %{ref: info.ref, pid: pid, assets: %{}},
           editor: nil,
           scan_binding: fn pid, _binding, _env -> send(pid, :scan_binding_ping) end,
           scan_eval_result: fn pid, _result -> send(pid, :scan_eval_result_ping) end
         }}
      end

      @impl true
      def init(info) do
        if info.attrs["crash"] do
          raise "crash"
        end

        {:ok, info}
      end

      @impl true
      def handle_info(message, info) do
        send(info.target_pid, {:smart_cell_debug, info.ref, :handle_info, message})
        {:noreply, info}
      end
    end

    defmodule Kino.SmartCell do
      def definitions() do
        [%{kind: "dumb", module: Kino.DumbCell, name: "Test smart cell"}]
      end
    end

    @opts [smart_cell_definitions_module: Kino.SmartCell]

    @tag opts: @opts
    test "notifies runtime owner when a smart cell is started", %{pid: pid} do
      RuntimeServer.start_smart_cell(pid, "dumb", "ref", %{}, [])
      assert_receive {:runtime_smart_cell_started, "ref", %{js_view: %{}, source: "source"}}
    end

    @tag opts: @opts
    test "notifies runtime owner when a smart cell goes down", %{pid: pid} do
      RuntimeServer.start_smart_cell(pid, "dumb", "ref", %{}, [])
      assert_receive {:runtime_smart_cell_started, "ref", %{js_view: %{pid: pid}}}
      Process.exit(pid, :crashed)
      assert_receive {:runtime_smart_cell_down, "ref"}
    end

    @tag opts: @opts
    @tag capture_log: true
    test "notifies runtime owner when a smart cell fails to start", %{pid: pid} do
      RuntimeServer.start_smart_cell(pid, "dumb", "ref", %{"crash" => true}, [])
      assert_receive {:runtime_smart_cell_down, "ref"}

      # Stop the server explicitly to make sure all logger messages are
      # sent by the time the test ends and stops capturing logs
      RuntimeServer.stop(pid)
    end

    @tag opts: @opts
    test "once started scans binding and sends the result to the cell server", %{pid: pid} do
      RuntimeServer.start_smart_cell(pid, "dumb", "ref", %{}, [])
      assert_receive {:smart_cell_debug, "ref", :handle_info, :scan_binding_ping}
    end

    @tag opts: @opts
    test "scans binding when a new parent locators are set", %{pid: pid} do
      RuntimeServer.evaluate_code(pid, :elixir, "1 + 1", {:c1, :e1}, [])
      RuntimeServer.start_smart_cell(pid, "dumb", "ref", %{}, [])
      assert_receive {:smart_cell_debug, "ref", :handle_info, :scan_binding_ping}
      RuntimeServer.set_smart_cell_parent_locators(pid, "ref", [{:c1, :e1}])
      assert_receive {:smart_cell_debug, "ref", :handle_info, :scan_binding_ping}
    end

    @tag opts: @opts
    test "scans binding when one of the parent locators is evaluated", %{pid: pid} do
      RuntimeServer.evaluate_code(pid, :elixir, "1 + 1", {:c1, :e1}, [])
      RuntimeServer.start_smart_cell(pid, "dumb", "ref", %{}, [{:c1, :e1}])
      assert_receive {:smart_cell_debug, "ref", :handle_info, :scan_binding_ping}
      RuntimeServer.evaluate_code(pid, :elixir, "1 + 1", {:c1, :e1}, [])
      assert_receive {:smart_cell_debug, "ref", :handle_info, :scan_binding_ping}
    end

    @tag opts: @opts
    test "scans evaluation result when the smart cell is evaluated", %{pid: pid} do
      RuntimeServer.start_smart_cell(pid, "dumb", "ref", %{}, [])
      RuntimeServer.evaluate_code(pid, :elixir, "1 + 1", {:c1, :e1}, [], smart_cell_ref: "ref")
      assert_receive {:smart_cell_debug, "ref", :handle_info, :scan_eval_result_ping}
    end

    @tag opts: @opts
    test "ignores stop request for stopped smart cells", %{pid: pid} do
      RuntimeServer.start_smart_cell(pid, "dumb", "ref", %{}, [])
      assert_receive {:runtime_smart_cell_started, "ref", %{js_view: %{pid: smart_cell_pid}}}
      Process.exit(smart_cell_pid, :crashed)
      assert_receive {:runtime_smart_cell_down, "ref"}
      RuntimeServer.stop_smart_cell(pid, "ref")

      # The server should still be operational
      RuntimeServer.evaluate_code(pid, :elixir, "1 + 1", {:c1, :e1}, [])
    end
  end

  test "clients monitoring", %{pid: pid, test: test} do
    # Pretend we are the subscriber
    Process.register(self(), test)

    code =
      """
      pid = Process.whereis(#{inspect(test)})

      ref = make_ref()
      send(Process.group_leader(), {:io_request, self(), ref, {:livebook_monitor_clients, pid}})

      receive do
        {:io_reply, ^ref, {:ok, ["c1"]}} -> :ok
      end
      """

    RuntimeServer.register_clients(pid, ["c1"])

    RuntimeServer.evaluate_code(pid, :elixir, code, {:c1, :e1}, [])
    assert_receive {:runtime_evaluation_response, :e1, _, %{evaluation_time_ms: _time_ms}}

    RuntimeServer.register_clients(pid, ["c2"])
    assert_receive {:client_join, "c2"}

    RuntimeServer.unregister_clients(pid, ["c1"])
    assert_receive {:client_leave, "c1"}
  end
end
