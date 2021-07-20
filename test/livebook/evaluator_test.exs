defmodule Livebook.EvaluatorTest do
  use ExUnit.Case, async: true

  alias Livebook.Evaluator

  setup do
    evaluator = start_supervised!(Evaluator)
    %{evaluator: evaluator}
  end

  describe "evaluate_code/6" do
    test "given a valid code returns evaluation result", %{evaluator: evaluator} do
      code = """
      x = 1
      y = 2
      x + y
      """

      Evaluator.evaluate_code(evaluator, self(), code, :code_1)

      assert_receive {:evaluation_response, :code_1, {:ok, 3}, %{evaluation_time_ms: _time_ms}}
    end

    test "given no prev_ref does not see previous evaluation context", %{evaluator: evaluator} do
      Evaluator.evaluate_code(evaluator, self(), "x = 1", :code_1)
      assert_receive {:evaluation_response, :code_1, _, %{evaluation_time_ms: _time_ms}}

      ignore_warnings(fn ->
        Evaluator.evaluate_code(evaluator, self(), "x", :code_2)

        assert_receive {:evaluation_response, :code_2,
                        {:error, _kind, %CompileError{description: "undefined function x/0"},
                         _stacktrace}, %{evaluation_time_ms: _time_ms}}
      end)
    end

    test "given prev_ref sees previous evaluation context", %{evaluator: evaluator} do
      Evaluator.evaluate_code(evaluator, self(), "x = 1", :code_1)
      assert_receive {:evaluation_response, :code_1, _, %{evaluation_time_ms: _time_ms}}

      Evaluator.evaluate_code(evaluator, self(), "x", :code_2, :code_1)

      assert_receive {:evaluation_response, :code_2, {:ok, 1}, %{evaluation_time_ms: _time_ms}}
    end

    test "given invalid prev_ref just uses default context", %{evaluator: evaluator} do
      Evaluator.evaluate_code(evaluator, self(), ":hey", :code_1, :code_nonexistent)

      assert_receive {:evaluation_response, :code_1, {:ok, :hey}, %{evaluation_time_ms: _time_ms}}
    end

    test "captures standard output and sends it to the caller", %{evaluator: evaluator} do
      Evaluator.evaluate_code(evaluator, self(), ~s{IO.puts("hey")}, :code_1)

      assert_receive {:evaluation_output, :code_1, "hey\n"}
    end

    test "using standard input sends input request to the caller", %{evaluator: evaluator} do
      Evaluator.evaluate_code(evaluator, self(), ~s{IO.gets("name: ")}, :code_1)

      assert_receive {:evaluation_input, :code_1, reply_to, "name: "}
      send(reply_to, {:evaluation_input_reply, {:ok, "Jake Peralta\n"}})

      assert_receive {:evaluation_response, :code_1, {:ok, "Jake Peralta\n"},
                      %{evaluation_time_ms: _time_ms}}
    end

    test "returns error along with its kind and stacktrace", %{evaluator: evaluator} do
      code = """
      List.first(%{})
      """

      Evaluator.evaluate_code(evaluator, self(), code, :code_1)

      assert_receive {:evaluation_response, :code_1,
                      {:error, :error, %FunctionClauseError{},
                       [{List, :first, _arity, _location}]}, %{evaluation_time_ms: _time_ms}}
    end

    test "in case of an error returns only the relevant part of stacktrace",
         %{evaluator: evaluator} do
      code = """
      defmodule Livebook.EvaluatorTest.Stacktrace.Math do
        def bad_math do
          result = 1 / 0
          {:ok, result}
        end
      end

      defmodule Livebook.EvaluatorTest.Stacktrace.Cat do
        def meow do
          Livebook.EvaluatorTest.Stacktrace.Math.bad_math()
          :ok
        end
      end

      Livebook.EvaluatorTest.Stacktrace.Cat.meow()
      """

      ignore_warnings(fn ->
        Evaluator.evaluate_code(evaluator, self(), code, :code_1)

        expected_stacktrace = [
          {Livebook.EvaluatorTest.Stacktrace.Math, :bad_math, 0, [file: 'nofile', line: 3]},
          {Livebook.EvaluatorTest.Stacktrace.Cat, :meow, 0, [file: 'nofile', line: 10]}
        ]

        # Note: evaluating module definitions is relatively slow, so we use a higher wait timeout.
        assert_receive {:evaluation_response, :code_1,
                        {:error, _kind, _error, ^expected_stacktrace},
                        %{evaluation_time_ms: _time_ms}},
                       1_000
      end)
    end

    test "in case of an error uses own evaluation context as the resulting context",
         %{evaluator: evaluator} do
      code1 = """
      x = 2
      """

      code2 = """
      raise ":<"
      """

      code3 = """
      x * x
      """

      Evaluator.evaluate_code(evaluator, self(), code1, :code_1)
      assert_receive {:evaluation_response, :code_1, {:ok, _}, %{evaluation_time_ms: _time_ms}}

      Evaluator.evaluate_code(evaluator, self(), code2, :code_2, :code_1)

      assert_receive {:evaluation_response, :code_2, {:error, _, _, _},
                      %{evaluation_time_ms: _time_ms}}

      Evaluator.evaluate_code(evaluator, self(), code3, :code_3, :code_2)
      assert_receive {:evaluation_response, :code_3, {:ok, 4}, %{evaluation_time_ms: _time_ms}}
    end

    test "given file option sets it in evaluation environment", %{evaluator: evaluator} do
      code = """
      __DIR__
      """

      opts = [file: "/path/dir/file"]
      Evaluator.evaluate_code(evaluator, self(), code, :code_1, nil, opts)

      assert_receive {:evaluation_response, :code_1, {:ok, "/path/dir"},
                      %{evaluation_time_ms: _time_ms}}
    end

    test "kills widgets that that no evaluation points to", %{evaluator: evaluator} do
      # Evaluate the code twice, which spawns two widget processes
      # First of them should be eventually killed

      Evaluator.evaluate_code(evaluator, self(), spawn_widget_code(), :code_1)

      assert_receive {:evaluation_response, :code_1, {:ok, widget_pid1},
                      %{evaluation_time_ms: _time_ms}}

      Evaluator.evaluate_code(evaluator, self(), spawn_widget_code(), :code_1)

      assert_receive {:evaluation_response, :code_1, {:ok, widget_pid2},
                      %{evaluation_time_ms: _time_ms}}

      ref = Process.monitor(widget_pid1)
      assert_receive {:DOWN, ^ref, :process, ^widget_pid1, :shutdown}

      assert Process.alive?(widget_pid2)
    end

    test "does not kill a widget if another evaluation points to it", %{evaluator: evaluator} do
      Evaluator.evaluate_code(evaluator, self(), spawn_widget_code(), :code_1)

      assert_receive {:evaluation_response, :code_1, {:ok, widget_pid1},
                      %{evaluation_time_ms: _time_ms}}

      Evaluator.evaluate_code(evaluator, self(), spawn_widget_code(), :code_2)

      assert_receive {:evaluation_response, :code_2, {:ok, widget_pid2},
                      %{evaluation_time_ms: _time_ms}}

      ref = Process.monitor(widget_pid1)
      refute_receive {:DOWN, ^ref, :process, ^widget_pid1, :shutdown}

      assert Process.alive?(widget_pid1)
      assert Process.alive?(widget_pid2)
    end
  end

  describe "forget_evaluation/2" do
    test "invalidates the given reference", %{evaluator: evaluator} do
      Evaluator.evaluate_code(evaluator, self(), "x = 1", :code_1)
      assert_receive {:evaluation_response, :code_1, _, %{evaluation_time_ms: _time_ms}}

      Evaluator.forget_evaluation(evaluator, :code_1)

      ignore_warnings(fn ->
        Evaluator.evaluate_code(evaluator, self(), "x", :code_2, :code_1)

        assert_receive {:evaluation_response, :code_2,
                        {:error, _kind, %CompileError{description: "undefined function x/0"},
                         _stacktrace}, %{evaluation_time_ms: _time_ms}}
      end)
    end

    test "kills widgets that no evaluation points to", %{evaluator: evaluator} do
      Evaluator.evaluate_code(evaluator, self(), spawn_widget_code(), :code_1)

      assert_receive {:evaluation_response, :code_1, {:ok, widget_pid1},
                      %{evaluation_time_ms: _time_ms}}

      Evaluator.forget_evaluation(evaluator, :code_1)

      ref = Process.monitor(widget_pid1)
      assert_receive {:DOWN, ^ref, :process, ^widget_pid1, :shutdown}
    end
  end

  describe "handle_intellisense/5 given completion request" do
    test "sends completion response to the given process", %{evaluator: evaluator} do
      request = {:completion, "System.ver"}
      Evaluator.handle_intellisense(evaluator, self(), :ref, request)
      assert_receive {:intellisense_response, :ref, %{items: [%{label: "version/0"}]}}, 1_000
    end

    test "given evaluation reference uses its bindings and env", %{evaluator: evaluator} do
      code = """
      alias IO.ANSI
      number = 10
      """

      Evaluator.evaluate_code(evaluator, self(), code, :code_1)
      assert_receive {:evaluation_response, :code_1, _, %{evaluation_time_ms: _time_ms}}

      request = {:completion, "num"}
      Evaluator.handle_intellisense(evaluator, self(), :ref, request, :code_1)
      assert_receive {:intellisense_response, :ref, %{items: [%{label: "number"}]}}, 1_000

      request = {:completion, "ANSI.brigh"}
      Evaluator.handle_intellisense(evaluator, self(), :ref, request, :code_1)

      assert_receive {:intellisense_response, :ref, %{items: [%{label: "bright/0"}]}}, 1_000
    end
  end

  describe "initialize_from/3" do
    setup do
      parent_evaluator = start_supervised!(Evaluator, id: :parent_evaluator)
      %{parent_evaluator: parent_evaluator}
    end

    test "copies the given context and sets as the initial one",
         %{evaluator: evaluator, parent_evaluator: parent_evaluator} do
      Evaluator.evaluate_code(parent_evaluator, self(), "x = 1", :code_1)
      assert_receive {:evaluation_response, :code_1, _, %{evaluation_time_ms: _time_ms}}

      Evaluator.initialize_from(evaluator, parent_evaluator, :code_1)

      Evaluator.evaluate_code(evaluator, self(), "x", :code_2)
      assert_receive {:evaluation_response, :code_2, {:ok, 1}, %{evaluation_time_ms: _time_ms}}
    end

    test "mirrors process dictionary of the given evaluator",
         %{evaluator: evaluator, parent_evaluator: parent_evaluator} do
      Evaluator.evaluate_code(parent_evaluator, self(), "Process.put(:data, 1)", :code_1)
      assert_receive {:evaluation_response, :code_1, _, %{evaluation_time_ms: _time_ms}}

      Evaluator.initialize_from(evaluator, parent_evaluator, :code_1)

      Evaluator.evaluate_code(evaluator, self(), "Process.get(:data)", :code_2)
      assert_receive {:evaluation_response, :code_2, {:ok, 1}, %{evaluation_time_ms: _time_ms}}
    end
  end

  # Helpers

  # Some of the code passed to Evaluator above is expected
  # to produce compilation warnings, so we ignore them.
  defp ignore_warnings(fun) do
    ExUnit.CaptureIO.capture_io(:stderr, fun)
    :ok
  end

  # Returns a code that spawns and renders a widget process
  # and returns its pid from the evaluation
  defp spawn_widget_code() do
    """
    widget_pid = spawn(fn ->
      Process.sleep(:infinity)
    end)

    ref = make_ref()
    send(Process.group_leader(), {:io_request, self(), ref, {:livebook_put_output, {:vega_lite_dynamic, widget_pid}}})

    receive do
      {:io_reply, ^ref, :ok} -> :ok
    end

    widget_pid
    """
  end
end
