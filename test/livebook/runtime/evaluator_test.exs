defmodule Livebook.Runtime.EvaluatorTest do
  use ExUnit.Case, async: true

  import Livebook.TestHelpers
  alias Livebook.Runtime.Evaluator

  @moduletag :tmp_dir

  setup_all do
    # We setup Pythonx in the current process, so we can test Python
    # code evaluation. Testing pyproject.toml evaluation is tricky
    # because it requires a separate VM, so we only rely on the LV
    # integration tests.

    ExUnit.CaptureIO.capture_io(fn ->
      Pythonx.uv_init("""
      [project]
      name = "project"
      version = "0.0.0"
      requires-python = "==3.13.*"
      dependencies = []
      """)
    end)

    :ok
  end

  setup ctx do
    ebin_path =
      if ctx[:with_ebin_path] do
        ebin_path = Path.join(ctx.tmp_dir, "ebin")
        File.rm_rf!(ebin_path)
        File.mkdir_p!(ebin_path)
        Code.append_path(ebin_path)
        ebin_path
      end

    {:ok, object_tracker} = start_supervised(Evaluator.ObjectTracker)
    {:ok, client_tracker} = start_supervised(Evaluator.ClientTracker)

    opts = [
      send_to: self(),
      object_tracker: object_tracker,
      client_tracker: client_tracker,
      tmp_dir: ctx.tmp_dir || nil,
      ebin_path: ebin_path
    ]

    {:ok, _pid, evaluator} = start_supervised({Evaluator, opts})

    %{
      evaluator: evaluator,
      object_tracker: object_tracker,
      client_tracker: client_tracker,
      ebin_path: ebin_path
    }
  end

  defmacrop metadata do
    quote do
      %{
        evaluation_time_ms: _,
        memory_usage: %{},
        code_markers: _,
        identifiers_used: _,
        identifiers_defined: _
      }
    end
  end

  defmacrop ansi_number(number), do: "\e[34m#{number}\e[0m"
  defmacrop ansi_string(string), do: "\e[32m\"#{string}\"\e[0m"

  defmacrop error(message) do
    quote do
      %{type: :error, message: unquote(message), context: nil}
    end
  end

  describe "evaluate_code/6" do
    test "given a valid code returns evaluation result", %{evaluator: evaluator} do
      code = """
      x = 1
      y = 2
      x + y
      """

      Evaluator.evaluate_code(evaluator, :elixir, code, :code_1, [])

      assert_receive {:runtime_evaluation_response, :code_1, terminal_text(ansi_number(3)),
                      metadata() = metadata}

      assert metadata.evaluation_time_ms >= 0

      assert %{atom: _, binary: _, code: _, ets: _, other: _, processes: _, total: _} =
               metadata.memory_usage
    end

    test "given no parent refs does not see previous evaluation context", %{evaluator: evaluator} do
      Evaluator.evaluate_code(evaluator, :elixir, "x = 1", :code_1, [])
      assert_receive {:runtime_evaluation_response, :code_1, _, metadata()}

      Evaluator.evaluate_code(evaluator, :elixir, "x", :code_2, [])

      assert_receive {:runtime_evaluation_response, :code_2,
                      error(
                        "\e[31m** (CompileError) cannot compile cell (errors have been logged)\e[0m"
                      ), metadata()}
    end

    test "given parent refs sees previous evaluation context", %{evaluator: evaluator} do
      Evaluator.evaluate_code(evaluator, :elixir, "x = 1", :code_1, [])
      assert_receive {:runtime_evaluation_response, :code_1, _, metadata()}

      Evaluator.evaluate_code(evaluator, :elixir, "x", :code_2, [:code_1])

      assert_receive {:runtime_evaluation_response, :code_2, terminal_text(ansi_number(1)),
                      metadata()}
    end

    test "given invalid parent ref uses the default context", %{evaluator: evaluator} do
      Evaluator.evaluate_code(evaluator, :elixir, "1", :code_1, [:code_nonexistent])

      assert_receive {:runtime_evaluation_response, :code_1, terminal_text(ansi_number(1)),
                      metadata()}
    end

    test "given parent refs sees previous process dictionary", %{evaluator: evaluator} do
      Evaluator.evaluate_code(evaluator, :elixir, "Process.put(:x, 1)", :code_1, [])
      assert_receive {:runtime_evaluation_response, :code_1, _, metadata()}
      Evaluator.evaluate_code(evaluator, :elixir, "Process.put(:x, 2)", :code_2, [])
      assert_receive {:runtime_evaluation_response, :code_2, _, metadata()}

      Evaluator.evaluate_code(evaluator, :elixir, "Process.get(:x)", :code_3, [:code_1])

      assert_receive {:runtime_evaluation_response, :code_3, terminal_text(ansi_number(1)),
                      metadata()}

      Evaluator.evaluate_code(evaluator, :elixir, "Process.get(:x)", :code_3, [:code_2])

      assert_receive {:runtime_evaluation_response, :code_3, terminal_text(ansi_number(2)),
                      metadata()}
    end

    test "keeps :rand state intact in process dictionary", %{evaluator: evaluator} do
      Evaluator.evaluate_code(evaluator, :elixir, ":rand.seed(:default, 0)", :code_1, [])
      assert_receive {:runtime_evaluation_response, :code_1, _, metadata()}

      Evaluator.evaluate_code(evaluator, :elixir, ":rand.uniform()", :code_2, [])

      assert_receive {:runtime_evaluation_response, :code_2, terminal_text(result1), metadata()}

      Evaluator.evaluate_code(evaluator, :elixir, ":rand.uniform()", :code_2, [])

      assert_receive {:runtime_evaluation_response, :code_2, terminal_text(result2), metadata()}

      assert result1 != result2

      Evaluator.evaluate_code(evaluator, :elixir, ":rand.seed(:default, 0)", :code_1, [])
      assert_receive {:runtime_evaluation_response, :code_1, _, metadata()}

      Evaluator.evaluate_code(evaluator, :elixir, ":rand.uniform()", :code_2, [])

      assert_receive {:runtime_evaluation_response, :code_2, terminal_text(^result1), metadata()}

      Evaluator.evaluate_code(evaluator, :elixir, ":rand.uniform()", :code_2, [])

      assert_receive {:runtime_evaluation_response, :code_2, terminal_text(^result2), metadata()}
    end

    test "captures standard output and sends it to the caller", %{evaluator: evaluator} do
      Evaluator.evaluate_code(evaluator, :elixir, ~s{IO.puts("hey")}, :code_1, [])

      assert_receive {:runtime_evaluation_output, :code_1, terminal_text("hey\n", true)}
    end

    test "using livebook input sends input request to the caller", %{evaluator: evaluator} do
      code = """
      ref = make_ref()
      send(Process.group_leader(), {:io_request, self(), ref, {:livebook_get_input_value, "input1"}})

      receive do
        {:io_reply, ^ref, {:ok, value}} -> value
      end
      """

      Evaluator.evaluate_code(evaluator, :elixir, code, :code_1, [])

      assert_receive {:runtime_evaluation_input_request, :code_1, reply_to, "input1"}
      send(reply_to, {:runtime_evaluation_input_reply, {:ok, 10}})

      assert_receive {:runtime_evaluation_response, :code_1, terminal_text(ansi_number(10)),
                      metadata()}
    end

    test "returns error along with its kind and stacktrace", %{evaluator: evaluator} do
      code = """
      List.first(%{})
      """

      Evaluator.evaluate_code(evaluator, :elixir, code, :code_1, [], file: "file.ex")

      assert_receive {:runtime_evaluation_response, :code_1, error(message), metadata()}

      assert """
             ** (FunctionClauseError) no function clause matching in List.first/2

                 The following arguments were given to List.first/2:

                     # 1
                     %{}

                     # 2
                     nil

                 Attempted function clauses (showing 2 out of 2):

                     def first([], default)
                     def first([head | _], _default)

             """ <> _ = clean_message(message)
    end

    test "only keeps #cell: markers in stacktrace", %{evaluator: evaluator} do
      code = """
      List.first(%{})
      """

      Evaluator.evaluate_code(evaluator, :elixir, code, :code_1, [], file: "file.ex#cell:abcDEF")

      assert_receive {:runtime_evaluation_response, :code_1, error(message), metadata()}
      assert message =~ "  #cell:abcDEF:1: (file)"
    end

    test "returns additional metadata when there is a syntax error", %{evaluator: evaluator} do
      code = "1+"

      Evaluator.evaluate_code(evaluator, :elixir, code, :code_1, [], file: "file.ex")

      assert_receive {:runtime_evaluation_response, :code_1, error(message),
                      %{
                        code_markers: [
                          %{
                            line: 1,
                            description: "syntax error: expression is incomplete",
                            severity: :error
                          }
                        ]
                      }}

      assert clean_message(message) === """
             ** (TokenMissingError) token missing on file.ex:1:2:
                 error: syntax error: expression is incomplete
                 │
               1 │ 1+
                 │  ^
                 │
                 └─ file.ex:1:2\
             """
    end

    test "returns additional metadata when there is a compilation error", %{evaluator: evaluator} do
      code = "x"

      Evaluator.evaluate_code(evaluator, :elixir, code, :code_1, [], file: "file.ex")

      assert_receive {:runtime_evaluation_response, :code_1,
                      error(
                        "\e[31m** (CompileError) cannot compile cell (errors have been logged)\e[0m"
                      ),
                      %{
                        code_markers: [
                          %{
                            line: 1,
                            description: ~s/undefined variable "x"/,
                            severity: :error
                          }
                        ]
                      }}
    end

    test "returns additional metadata when there is a module compilation error",
         %{evaluator: evaluator} do
      code = """
      defmodule Livebook.Runtime.EvaluatorTest.Invalid do
        x
      end
      """

      Evaluator.evaluate_code(evaluator, :elixir, code, :code_1, [], file: "file.ex")

      assert_receive {:runtime_evaluation_response, :code_1,
                      error(
                        "\e[31m** (CompileError) file.ex: cannot compile module Livebook.Runtime.EvaluatorTest.Invalid " <>
                          "(errors have been logged)\e[0m" <> _
                      ),
                      %{
                        code_markers: [
                          %{
                            line: 2,
                            description: ~s/undefined variable "x"/,
                            severity: :error
                          }
                        ]
                      }}
    end

    test "ignores code errors when they happen in the actual evaluation", %{evaluator: evaluator} do
      code = """
      Code.eval_string("x")
      """

      Evaluator.evaluate_code(evaluator, :elixir, code, :code_1, [], file: "file.ex")

      assert_receive {:runtime_evaluation_response, :code_1,
                      error(
                        "\e[31m** (CompileError) cannot compile cell (errors have been logged)\e[0m"
                      ), %{code_markers: []}}
    end

    test "in case of an error returns only the relevant part of stacktrace",
         %{evaluator: evaluator} do
      code = """
      defmodule Livebook.Runtime.EvaluatorTest.Stacktrace.Math do
        def bad_math do
          result = 1 / 0
          {:ok, result}
        end
      end

      defmodule Livebook.Runtime.EvaluatorTest.Stacktrace.Cat do
        def meow do
          Livebook.Runtime.EvaluatorTest.Stacktrace.Math.bad_math()
          :ok
        end
      end

      Livebook.Runtime.EvaluatorTest.Stacktrace.Cat.meow()
      """

      Evaluator.evaluate_code(evaluator, :elixir, code, :code_1, [])

      # Note: evaluating module definitions is relatively slow, so we use a higher wait timeout.
      assert_receive {:runtime_evaluation_response, :code_1, error(message), metadata()},
                     2_000

      assert clean_message(message) ==
               """
               ** (ArithmeticError) bad argument in arithmetic expression
                   nofile:3: Livebook.Runtime.EvaluatorTest.Stacktrace.Math.bad_math/0
                   nofile:10: Livebook.Runtime.EvaluatorTest.Stacktrace.Cat.meow/0
                   nofile:15: (file)
               """
    end

    test "in case of an error uses empty evaluation context as the resulting context",
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

      Evaluator.evaluate_code(evaluator, :elixir, code1, :code_1, [])

      assert_receive {:runtime_evaluation_response, :code_1, terminal_text(ansi_number(2)),
                      metadata()}

      Evaluator.evaluate_code(evaluator, :elixir, code2, :code_2, [:code_1])

      assert_receive {:runtime_evaluation_response, :code_2, error(_), metadata()}

      Evaluator.evaluate_code(evaluator, :elixir, code3, :code_3, [:code_2, :code_1])

      assert_receive {:runtime_evaluation_response, :code_3, terminal_text(ansi_number(4)),
                      metadata()}
    end

    test "given file option sets it in evaluation environment", %{evaluator: evaluator} do
      code = """
      __DIR__
      """

      opts = [file: "/path/dir/file"]
      Evaluator.evaluate_code(evaluator, :elixir, code, :code_1, [], opts)

      assert_receive {:runtime_evaluation_response, :code_1,
                      terminal_text(ansi_string("/path/dir")), metadata()}
    end

    test "kills widgets that that no evaluation points to", %{evaluator: evaluator} do
      # Evaluate the code twice, each time a new widget is spawned.
      # The evaluation reference is the same, so the second one overrides
      # the first one and the first widget should eventually be killed.

      Evaluator.evaluate_code(evaluator, :elixir, spawn_widget_code(), :code_1, [])

      assert_receive {:runtime_evaluation_response, :code_1, terminal_text(widget_pid1_string),
                      metadata()}

      widget_pid1 = IEx.Helpers.pid(widget_pid1_string)

      ref = Process.monitor(widget_pid1)

      Evaluator.evaluate_code(evaluator, :elixir, spawn_widget_code(), :code_1, [])

      assert_receive {:runtime_evaluation_response, :code_1, terminal_text(widget_pid2_string),
                      metadata()}

      widget_pid2 = IEx.Helpers.pid(widget_pid2_string)

      assert_receive {:DOWN, ^ref, :process, ^widget_pid1, _reason}

      assert Process.alive?(widget_pid2)
    end

    test "kills widgets when the spawning process terminates", %{evaluator: evaluator} do
      # The widget is spawned from a process that terminates,
      # so the widget should terminate immediately as well

      Evaluator.evaluate_code(
        evaluator,
        :elixir,
        spawn_widget_from_terminating_process_code(),
        :code_1,
        []
      )

      assert_receive {:runtime_evaluation_response, :code_1, terminal_text(widget_pid1_string),
                      metadata()}

      widget_pid1 = IEx.Helpers.pid(widget_pid1_string)

      ref = Process.monitor(widget_pid1)
      assert_receive {:DOWN, ^ref, :process, ^widget_pid1, _reason}
    end

    @tag :with_ebin_path
    test "raises when redefining a module in a different evaluation", %{evaluator: evaluator} do
      code = """
      defmodule Livebook.Runtime.EvaluatorTest.Redefinition do
      end
      """

      Evaluator.evaluate_code(evaluator, :elixir, code, :code_1, [])
      assert_receive {:runtime_evaluation_response, :code_1, terminal_text(_), metadata()}

      # Redefining in the same evaluation works
      Evaluator.evaluate_code(evaluator, :elixir, code, :code_1, [])
      assert_receive {:runtime_evaluation_response, :code_1, terminal_text(_), metadata()}

      Evaluator.evaluate_code(evaluator, :elixir, code, :code_2, [], file: "file.ex")

      assert_receive {:runtime_evaluation_response, :code_2,
                      error(
                        "\e[31m** (CompileError) file.ex:1: module Livebook.Runtime.EvaluatorTest.Redefinition is already defined\e[0m"
                      ),
                      %{
                        code_markers: [
                          %{
                            line: 1,
                            description:
                              "module Livebook.Runtime.EvaluatorTest.Redefinition is already defined",
                            severity: :error
                          }
                        ]
                      }}
    end

    @tag :with_ebin_path
    test "writes module bytecode to disk when :ebin_path is specified",
         %{evaluator: evaluator, ebin_path: ebin_path} do
      code = """
      defmodule Livebook.Runtime.EvaluatorTest.Disk do
        @moduledoc "Test."
      end
      """

      Evaluator.evaluate_code(evaluator, :elixir, code, :code_1, [])
      assert_receive {:runtime_evaluation_response, :code_1, terminal_text(_), metadata()}

      assert File.exists?(Path.join(ebin_path, "Elixir.Livebook.Runtime.EvaluatorTest.Disk.beam"))

      assert {:docs_v1, _, _, _, _, _, _} = Code.fetch_docs(Livebook.Runtime.EvaluatorTest.Disk)
    end

    test "deletes defined modules if the evaluation fails", %{evaluator: evaluator} do
      code = """
      defmodule Livebook.Runtime.EvaluatorTest.Raised do
      end

      raise "failed"
      """

      Evaluator.evaluate_code(evaluator, :elixir, code, :code_1, [])
      assert_receive {:runtime_evaluation_response, :code_1, error(_), metadata()}

      refute Code.ensure_loaded?(Livebook.Runtime.EvaluatorTest.Raised)
    end

    @tag :with_ebin_path
    @tag capture_log: true
    test "deletes defined modules on termination", %{evaluator: evaluator} do
      code = """
      defmodule Livebook.Runtime.EvaluatorTest.Exited do
      end

      Process.exit(self(), :kill)
      """

      {:group_leader, gl} = Process.info(evaluator.pid, :group_leader)

      Evaluator.evaluate_code(evaluator, :elixir, code, :code_1, [])

      ref = Process.monitor(gl)
      assert_receive {:DOWN, ^ref, :process, ^gl, _reason}

      refute Code.ensure_loaded?(Livebook.Runtime.EvaluatorTest.Exited)
    end

    @tag :with_ebin_path
    test "returns identifier definitions", %{evaluator: evaluator} do
      Code.put_compiler_option(:debug_info, true)

      code = ~S'''
      defmodule Livebook.Runtime.EvaluatorTest.ModuleDef1 do
        def fun(), do: :ok
      end

      defmodule Livebook.Runtime.EvaluatorTest.ModuleDef2 do
        defmodule Foo do
          defstruct [:name]
        end
      end
      '''

      file = "file.livemd#cell_id:123456789"

      Evaluator.evaluate_code(evaluator, :elixir, code, :code_1, [], file: file)

      assert_receive {:runtime_evaluation_response, :code_1, terminal_text(_),
                      metadata() = metadata}

      assert metadata.identifier_definitions == [
               %{
                 label: "Livebook.Runtime.EvaluatorTest.ModuleDef1",
                 line: 1,
                 file: file
               },
               %{
                 label: "Livebook.Runtime.EvaluatorTest.ModuleDef2",
                 line: 5,
                 file: file
               },
               %{
                 label: "Livebook.Runtime.EvaluatorTest.ModuleDef2.Foo",
                 line: 6,
                 file: file
               }
             ]
    after
      Code.put_compiler_option(:debug_info, false)
    end
  end

  describe "doctests" do
    @describetag :with_ebin_path

    test "assertions", %{evaluator: evaluator} do
      code = ~S'''
      defmodule Livebook.Runtime.EvaluatorTest.DoctestsAssertions do
        @moduledoc """

            iex> raise "oops"
            ** (ArgumentError) not oops

            iex> require ExUnit.Assertions
            ...> ExUnit.Assertions.assert false
        """

        @doc """
            iex> Livebook.Runtime.EvaluatorTest.DoctestsAssertions.data()
            %{
              name: "Amy Santiago",
              description: "nypd detective",
              precinct: 99
            }

          iex> Livebook.Runtime.EvaluatorTest.DoctestsAssertions.data()
          %{name: "Jake Peralta", description: "NYPD detective"}
        """
        def data() do
          %{
            name: "Amy Santiago",
            description: "nypd detective",
            precinct: 99
          }
        end
      end
      '''

      Evaluator.evaluate_code(evaluator, :elixir, code, :code_1, [])

      assert_receive {:runtime_doctest_report, :code_1, %{line: 4, status: :running}}

      assert_receive {:runtime_doctest_report, :code_1,
                      %{
                        column: 6,
                        details:
                          "\e[31mexpected exception ArgumentError but got RuntimeError with message \"oops\"\e[0m",
                        end_line: 5,
                        line: 4,
                        status: :failed
                      }}

      assert_receive {:runtime_doctest_report, :code_1, %{line: 7, status: :running}}

      assert_receive {:runtime_doctest_report, :code_1,
                      %{
                        column: 6,
                        details: "\e[31mExpected truthy, got false\e[0m",
                        end_line: 8,
                        line: 7,
                        status: :failed
                      }}

      assert_receive {:runtime_doctest_report, :code_1, %{line: 12, status: :running}}

      assert_receive {:runtime_doctest_report, :code_1, %{line: 12, status: :success}}

      assert_receive {:runtime_doctest_report, :code_1, %{line: 19, status: :running}}

      assert_receive {:runtime_doctest_report, :code_1,
                      %{column: 4, details: _, end_line: 20, line: 19, status: :failed}}
    end

    test "multiple assertions at once", %{evaluator: evaluator} do
      code = ~S'''
      defmodule Livebook.Runtime.EvaluatorTest.DoctestsMiddle do
        @moduledoc """

            iex> 1 + 1
            2
            iex> 1 + 2
            :wrong
            iex> 1 + 3
            4

        """
      end
      '''

      Evaluator.evaluate_code(evaluator, :elixir, code, :code_1, [])

      assert_receive {:runtime_doctest_report, :code_1, %{line: 4, status: :running}}

      assert_receive {:runtime_doctest_report, :code_1,
                      %{
                        column: 6,
                        details: _,
                        end_line: 7,
                        line: 4,
                        status: :failed
                      }}
    end

    test "runtime errors", %{evaluator: evaluator} do
      code = ~S'''
      defmodule Livebook.Runtime.EvaluatorTest.DoctestsRuntime do
        @moduledoc """

            iex> 1 = 2

        """

        @doc """
            iex> Livebook.Runtime.EvaluatorTest.DoctestsRuntime.raise_with_stacktrace()
            :what
        """
        def raise_with_stacktrace() do
          Enum.map(1, & &1)
        end

        @doc """
            iex> Livebook.Runtime.EvaluatorTest.DoctestsRuntime.exit()
            :what
        """
        def exit() do
          Process.exit(self(), :shutdown)
        end
      end
      '''

      Evaluator.evaluate_code(evaluator, :elixir, code, :code_1, [])

      assert_receive {:runtime_doctest_report, :code_1, %{line: 4, status: :running}}

      assert_receive {:runtime_doctest_report, :code_1,
                      %{
                        column: 6,
                        details: "\e[31mmatch (=) failed" <> _,
                        end_line: 4,
                        line: 4,
                        status: :failed
                      }}

      assert_receive {:runtime_doctest_report, :code_1, %{line: 9, status: :running}}

      assert_receive {:runtime_doctest_report, :code_1,
                      %{
                        column: 6,
                        details:
                          "\e[31m** (Protocol.UndefinedError) protocol Enumerable not implemented for type Integer. " <>
                            _,
                        end_line: 10,
                        line: 9,
                        status: :failed
                      }}

      assert_receive {:runtime_doctest_report, :code_1, %{line: 17, status: :running}}

      assert_receive {:runtime_doctest_report, :code_1,
                      %{
                        column: 6,
                        details: "\e[31m** (EXIT from #PID<" <> _,
                        end_line: 18,
                        line: 17,
                        status: :failed
                      }}
    end

    test "invalid", %{evaluator: evaluator} do
      code = ~S'''
      defmodule Livebook.Runtime.EvaluatorTest.DoctestsInvalid do
        @doc """

            iex> 1 +
            :who_knows

        """
        def foo, do: :ok
      end
      '''

      Evaluator.evaluate_code(evaluator, :elixir, code, :code_1, [])

      assert_receive {:runtime_doctest_report, :code_1, %{line: 4, status: :running}}

      assert_receive {:runtime_doctest_report, :code_1,
                      %{
                        column: 6,
                        details: "\e[31mDoctest did not compile, got: (TokenMissingError) " <> _,
                        end_line: 5,
                        line: 4,
                        status: :failed
                      }}
    end

    test "does not run generated doctests", %{evaluator: evaluator} do
      code = ~S'''
      defmodule Livebook.Runtime.EvaluatorTest.DoctestsGeneratedBase do
        defmacro __using__(_) do
          quote do
            @doc """

                iex> 1
                2

                iex> 2
                2

            """
            def foo, do: :ok
          end
        end
      end

      defmodule Livebook.Runtime.EvaluatorTest.DoctestsGenerated do
        use Livebook.Runtime.EvaluatorTest.DoctestsGeneratedBase
      end
      '''

      Evaluator.evaluate_code(evaluator, :elixir, code, :code_1, [])

      assert_receive {:runtime_evaluation_response, :code_1, _, metadata()}
      refute_received {:runtime_doctest_report, :code_1, %{}}

      # Here the generated doctest line matches another iex> prompt
      # in the module, but we expect the :erl_anno check to filter
      # it out

      code = ~S'''
      defmodule Livebook.Runtime.EvaluatorTest.DoctestsGeneratedBase do
        defmacro __using__(_) do
          quote do
            @doc """

                iex> 1
                2

            """
            def foo, do: :ok
          end
        end
      end

      defmodule Livebook.Runtime.EvaluatorTest.DoctestsGenerated do
        use Livebook.Runtime.EvaluatorTest.DoctestsGeneratedBase
        @string """
            iex> 1
            2
        """
      end
      '''

      Evaluator.evaluate_code(evaluator, :elixir, code, :code_1, [])

      assert_receive {:runtime_evaluation_response, :code_1, _, metadata()}
      refute_received {:runtime_doctest_report, :code_1, %{}}
    end
  end

  describe "evaluate_code/6 identifier tracking" do
    defp eval(code, evaluator, eval_idx) do
      ref = eval_idx
      parent_refs = Enum.to_list((eval_idx - 1)..0//-1)
      Evaluator.evaluate_code(evaluator, :elixir, code, ref, parent_refs)
      assert_receive {:runtime_evaluation_response, ^ref, _output, metadata}
      %{used: metadata.identifiers_used, defined: metadata.identifiers_defined}
    end

    test "variables", %{evaluator: evaluator} do
      identifiers =
        """
        x = 1
        y = 1
        """
        |> eval(evaluator, 0)

      assert %{
               {:variable, {:x, nil}} => _,
               {:variable, {:y, nil}} => _
             } = identifiers.defined

      identifiers =
        """
        x
        """
        |> eval(evaluator, 1)

      assert {:variable, {:x, nil}} in identifiers.used
      assert {:variable, {:y, nil}} not in identifiers.used
    end

    test "variables with non-default context", %{evaluator: evaluator} do
      identifiers =
        """
        var!(x, :context) = 1
        """
        |> eval(evaluator, 0)

      assert %{{:variable, {:x, :context}} => _} = identifiers.defined

      identifiers =
        """
        var!(x, :context)
        """
        |> eval(evaluator, 1)

      assert {:variable, {:x, :context}} in identifiers.used
    end

    test "variables used inside a module", %{evaluator: evaluator} do
      identifiers =
        """
        x = 1
        y = 1
        z = 1
        """
        |> eval(evaluator, 0)

      assert %{
               {:variable, {:x, nil}} => _,
               {:variable, {:y, nil}} => _,
               {:variable, {:z, nil}} => _
             } = identifiers.defined

      identifiers =
        """
        defmodule Livebook.Runtime.EvaluatorTest.Identifiers.UsedVars do
          def fun(), do: unquote(x)
        end

        y
        """
        |> eval(evaluator, 1)

      assert {:variable, {:x, nil}} in identifiers.used
      assert {:variable, {:y, nil}} in identifiers.used
      assert {:variable, {:z, nil}} not in identifiers.used
    end

    test "module definition", %{evaluator: evaluator} do
      identifiers =
        """
        defmodule Livebook.Runtime.EvaluatorTest.Identifiers.ModuleDefinition do
          def fun(), do: 1
        end
        """
        |> eval(evaluator, 0)

      assert {:alias, :"Elixir.Livebook.Runtime.EvaluatorTest.Identifiers.ModuleDefinition"} in identifiers.used

      assert %{
               {:module, :"Elixir.Livebook.Runtime.EvaluatorTest.Identifiers.ModuleDefinition"} =>
                 version1
             } = identifiers.defined

      identifiers =
        """
        defmodule Livebook.Runtime.EvaluatorTest.Identifiers.ModuleDefinition do
          def fun(), do: 1
        end
        """
        |> eval(evaluator, 0)

      assert %{
               {:module, :"Elixir.Livebook.Runtime.EvaluatorTest.Identifiers.ModuleDefinition"} =>
                 ^version1
             } = identifiers.defined

      identifiers =
        """
        defmodule Livebook.Runtime.EvaluatorTest.Identifiers.ModuleDefinition do
          def fun(), do: 2
        end
        """
        |> eval(evaluator, 0)

      assert %{
               {:module, :"Elixir.Livebook.Runtime.EvaluatorTest.Identifiers.ModuleDefinition"} =>
                 version2
             } = identifiers.defined

      assert version2 != version1
    end

    test "module function call", %{evaluator: evaluator} do
      identifiers =
        """
        Enum.uniq([1, 2])
        """
        |> eval(evaluator, 0)

      assert {:module, :"Elixir.Enum"} in identifiers.used
    end

    test "alias", %{evaluator: evaluator} do
      identifiers =
        """
        alias Map, as: M
        """
        |> eval(evaluator, 0)

      assert {:alias, :"Elixir.Map"} in identifiers.used

      assert %{{:alias, :"Elixir.M"} => :"Elixir.Map"} = identifiers.defined

      identifiers =
        """
        M.new()
        """
        |> eval(evaluator, 1)

      assert {:alias, :"Elixir.M"} in identifiers.used
      assert {:module, :"Elixir.Map"} in identifiers.used
    end

    test "require", %{evaluator: evaluator} do
      identifiers =
        """
        require Integer
        """
        |> eval(evaluator, 0)

      assert {:alias, :"Elixir.Integer"} in identifiers.used

      assert %{{:require, :"Elixir.Integer"} => :ok} = identifiers.defined

      identifiers =
        """
        Integer.is_even(2)
        """
        |> eval(evaluator, 1)

      assert {:alias, :"Elixir.Integer"} in identifiers.used
      assert {:require, :"Elixir.Integer"} in identifiers.used
      assert {:module, :"Elixir.Integer"} in identifiers.used
    end

    test "import", %{evaluator: evaluator} do
      identifiers =
        """
        import Enum
        """
        |> eval(evaluator, 0)

      assert %{:imports => version1} = identifiers.defined

      identifiers =
        """
        import Enum
        """
        |> eval(evaluator, 0)

      assert %{:imports => ^version1} = identifiers.defined

      identifiers =
        """
        import Integer
        """
        |> eval(evaluator, 0)

      assert :imports in identifiers.used
      assert {:alias, :"Elixir.Integer"} in identifiers.used
      assert {:module, :"Elixir.Integer"} in identifiers.used

      assert %{
               :imports => version2,
               {:require, :"Elixir.Integer"} => :ok
             } = identifiers.defined

      assert version2 != version1

      identifiers =
        """
        is_even(2)
        """
        |> eval(evaluator, 1)

      assert :imports in identifiers.used
      assert {:module, :"Elixir.Integer"} in identifiers.used

      identifiers =
        """
        Integer.is_even(2)
        """
        |> eval(evaluator, 1)

      assert {:require, :"Elixir.Integer"} in identifiers.used
      assert {:module, :"Elixir.Integer"} in identifiers.used
    end

    test "struct", %{evaluator: evaluator} do
      identifiers =
        """
        %URI{}
        """
        |> eval(evaluator, 0)

      assert {:alias, :"Elixir.URI"} in identifiers.used
      assert {:module, :"Elixir.URI"} in identifiers.used
    end

    test "process dictionary", %{evaluator: evaluator} do
      identifiers =
        """
        :ok
        """
        |> eval(evaluator, 0)

      # Every evaluation should depend on process dictionary
      assert :pdict in identifiers.used

      identifiers =
        """
        Process.put(:x, 1)
        """
        |> eval(evaluator, 0)

      assert %{pdict: version1} = identifiers.defined

      identifiers =
        """
        Process.put(:x, 1)
        """
        |> eval(evaluator, 0)

      assert %{pdict: ^version1} = identifiers.defined

      identifiers =
        """
        Process.put(:x, 2)
        """
        |> eval(evaluator, 0)

      assert %{pdict: version2} = identifiers.defined

      assert version2 != version1
    end

    test "context merging", %{evaluator: evaluator} do
      """
      x = 1
      y = 1

      alias Enum, as: E
      alias Map, as: M

      require Enum

      import Integer, only: [is_odd: 1, is_even: 1, to_string: 2, to_charlist: 2]

      Process.put(:x, 1)
      Process.put(:y, 1)
      Process.put(:z, 1)

      defmodule Livebook.Runtime.EvaluatorTest.Identifiers.ContextMergingOne do
      end
      """
      |> eval(evaluator, 0)

      """
      y = 2

      alias MapSet, as: M

      require Map

      import Integer, except: [is_even: 1, to_string: 2]

      Process.put(:y, 2)
      Process.delete(:z)

      defmodule Livebook.Runtime.EvaluatorTest.Identifiers.ContextMergingTwo do
      end
      """
      |> eval(evaluator, 1)

      # Evaluation 0 context

      context = Evaluator.get_evaluation_context(evaluator, [0])

      assert Enum.sort(context.binding) == [x: 1, y: 1]

      assert Enum.sort(context.env.aliases) == [{E, Enum}, {M, Map}]

      assert Enum in context.env.requires

      assert [_, _ | _] = context.env.functions[Integer]
      assert [_, _ | _] = context.env.macros[Integer]

      assert context.env.versioned_vars == %{{:x, nil} => 0, {:y, nil} => 1}

      assert context.env.context_modules == [
               Livebook.Runtime.EvaluatorTest.Identifiers.ContextMergingOne
             ]

      assert context.pdict == %{x: 1, y: 1, z: 1}

      # Evaluation 1 context

      context = Evaluator.get_evaluation_context(evaluator, [1])

      assert Enum.sort(context.binding) == [y: 2]

      assert Enum.sort(context.env.aliases) == [{M, MapSet}]

      assert Map in context.env.requires
      assert Enum not in context.env.requires

      # Imports are not diffed
      assert {Integer, [to_charlist: 2]} in context.env.functions
      assert {Integer, [is_odd: 1]} in context.env.macros

      assert context.env.versioned_vars == %{{:y, nil} => 0}

      assert context.env.context_modules == [
               Livebook.Runtime.EvaluatorTest.Identifiers.ContextMergingTwo
             ]

      # Process dictionary is not diffed
      assert context.pdict == %{x: 1, y: 2}

      # Merged context

      context = Evaluator.get_evaluation_context(evaluator, [1, 0])

      assert Enum.sort(context.binding) == [x: 1, y: 2]

      assert Enum.sort(context.env.aliases) == [{E, Enum}, {M, MapSet}]

      assert Enum in context.env.requires
      assert Map in context.env.requires

      assert {Integer, [to_charlist: 2]} in context.env.functions
      assert {Integer, [is_odd: 1]} in context.env.macros

      assert context.env.versioned_vars == %{{:x, nil} => 0, {:y, nil} => 1}

      assert context.env.context_modules == [
               Livebook.Runtime.EvaluatorTest.Identifiers.ContextMergingTwo,
               Livebook.Runtime.EvaluatorTest.Identifiers.ContextMergingOne
             ]

      assert context.pdict == %{x: 1, y: 2}
    end

    test "context merging with errored evaluation", %{evaluator: evaluator} do
      # This test is similar to the above, but the second evaluation
      # fails with an error

      """
      x = 1
      y = 1

      alias Enum, as: E
      alias Map, as: M

      require Enum

      import Integer, only: [is_odd: 1, is_even: 1, to_string: 2, to_charlist: 2]

      Process.put(:x, 1)
      Process.put(:y, 1)
      Process.put(:z, 1)

      defmodule Livebook.Runtime.EvaluatorTest.Identifiers.ErrorContextMergingOne do
      end
      """
      |> eval(evaluator, 0)

      """
      y = 2

      alias MapSet, as: M

      require Map

      import Integer, except: [is_even: 1, to_string: 2]

      Process.put(:y, 2)
      Process.delete(:z)

      defmodule Livebook.Runtime.EvaluatorTest.Identifiers.ErrorContextMergingTwo do
      end

      raise "oops"
      """
      |> eval(evaluator, 1)

      # Evaluation 1 context
      #
      # Should be empty, except for imports and process dictionary,
      # which are not diffed and should be kept from evaluation 0.

      context = Evaluator.get_evaluation_context(evaluator, [1])

      assert Enum.sort(context.binding) == []

      assert Enum.sort(context.env.aliases) == []

      assert Map not in context.env.requires
      assert Enum not in context.env.requires

      assert [_, _ | _] = context.env.functions[Integer]
      assert [_, _ | _] = context.env.macros[Integer]

      assert context.env.versioned_vars == %{}

      assert context.env.context_modules == []

      assert context.pdict == %{x: 1, y: 1, z: 1}

      # Merged context

      context = Evaluator.get_evaluation_context(evaluator, [1, 0])

      assert Enum.sort(context.binding) == [x: 1, y: 1]

      assert Enum.sort(context.env.aliases) == [{E, Enum}, {M, Map}]

      assert Enum in context.env.requires

      assert [_, _ | _] = context.env.functions[Integer]
      assert [_, _ | _] = context.env.macros[Integer]

      assert context.env.versioned_vars == %{{:x, nil} => 0, {:y, nil} => 1}

      assert context.env.context_modules == [
               Livebook.Runtime.EvaluatorTest.Identifiers.ErrorContextMergingOne
             ]

      assert context.pdict == %{x: 1, y: 1, z: 1}
    end
  end

  describe "forget_evaluation/2" do
    test "invalidates the given reference", %{evaluator: evaluator} do
      Evaluator.evaluate_code(evaluator, :elixir, "x = 1", :code_1, [])
      assert_receive {:runtime_evaluation_response, :code_1, _, metadata()}

      Evaluator.forget_evaluation(evaluator, :code_1)

      Evaluator.evaluate_code(evaluator, :elixir, "x", :code_2, [:code_1])

      assert_receive {:runtime_evaluation_response, :code_2,
                      error(
                        "\e[31m** (CompileError) cannot compile cell (errors have been logged)\e[0m"
                      ), metadata()}
    end

    test "kills widgets that no evaluation points to", %{evaluator: evaluator} do
      Evaluator.evaluate_code(evaluator, :elixir, spawn_widget_code(), :code_1, [])

      assert_receive {:runtime_evaluation_response, :code_1, terminal_text(widget_pid1_string),
                      metadata()}

      widget_pid1 = IEx.Helpers.pid(widget_pid1_string)

      ref = Process.monitor(widget_pid1)
      Evaluator.forget_evaluation(evaluator, :code_1)

      assert_receive {:DOWN, ^ref, :process, ^widget_pid1, _reason}
    end

    @tag :with_ebin_path
    test "deletes modules defined by the given evaluation", %{evaluator: evaluator} do
      code = """
      defmodule Livebook.Runtime.EvaluatorTest.ForgetEvaluation.Redefinition do
      end
      """

      Evaluator.evaluate_code(evaluator, :elixir, code, :code_1, [])
      assert_receive {:runtime_evaluation_response, :code_1, terminal_text(_), metadata()}

      Evaluator.forget_evaluation(evaluator, :code_1)

      # Define the module in a different evaluation
      Evaluator.evaluate_code(evaluator, :elixir, code, :code_2, [])
      assert_receive {:runtime_evaluation_response, :code_2, terminal_text(_), metadata()}
    end
  end

  describe "initialize_from/3" do
    setup %{object_tracker: object_tracker, client_tracker: client_tracker} do
      opts = [send_to: self(), object_tracker: object_tracker, client_tracker: client_tracker]
      {:ok, _pid, parent_evaluator} = start_supervised({Evaluator, opts}, id: :parent_evaluator)

      %{parent_evaluator: parent_evaluator}
    end

    test "copies the given context and sets as the initial one",
         %{evaluator: evaluator, parent_evaluator: parent_evaluator} do
      Evaluator.evaluate_code(parent_evaluator, :elixir, "x = 1", :code_1, [])
      assert_receive {:runtime_evaluation_response, :code_1, _, metadata()}

      Evaluator.initialize_from(evaluator, parent_evaluator, [:code_1])

      Evaluator.evaluate_code(evaluator, :elixir, "x", :code_2, [])

      assert_receive {:runtime_evaluation_response, :code_2, terminal_text(ansi_number(1)),
                      metadata()}
    end
  end

  describe "binding order" do
    test "keeps binding in evaluation order, starting from most recent", %{evaluator: evaluator} do
      Evaluator.evaluate_code(evaluator, :elixir, "b = 1", :code_1, [])
      Evaluator.evaluate_code(evaluator, :elixir, "a = 1", :code_2, [:code_1])
      Evaluator.evaluate_code(evaluator, :elixir, "c = 1", :code_3, [:code_2, :code_1])
      Evaluator.evaluate_code(evaluator, :elixir, "x = 1", :code_4, [:code_3, :code_2, :code_1])

      %{binding: binding} =
        Evaluator.get_evaluation_context(evaluator, [:code_4, :code_3, :code_2, :code_1])

      assert [:x, :c, :a, :b] == Enum.map(binding, &elem(&1, 0))
    end

    test "treats rebound names as new", %{evaluator: evaluator} do
      Evaluator.evaluate_code(evaluator, :elixir, "b = 1", :code_1, [])
      Evaluator.evaluate_code(evaluator, :elixir, "a = 1", :code_2, [:code_1])
      Evaluator.evaluate_code(evaluator, :elixir, "b = 2", :code_3, [:code_2, :code_1])

      %{binding: binding} =
        Evaluator.get_evaluation_context(evaluator, [:code_3, :code_2, :code_1])

      assert [:b, :a] == Enum.map(binding, &elem(&1, 0))
    end
  end

  describe "erlang evaluation" do
    test "evaluates erlang code", %{evaluator: evaluator} do
      Evaluator.evaluate_code(
        evaluator,
        :erlang,
        "X = lists:seq(1, 3), lists:sum(X).",
        :code_1,
        []
      )

      assert_receive {:runtime_evaluation_response, :code_1, terminal_text("6"), metadata()}
    end

    @tag :with_ebin_path
    test "evaluates erlang-module code", %{evaluator: evaluator} do
      code = """
      -module(tryme).

      -export([go/0, macros/0]).

      go() -> {ok,went}.
      macros() -> {?MODULE, ?FILE}.
      """

      Evaluator.evaluate_code(evaluator, :erlang, code, :code_1, [])

      assert_receive {:runtime_evaluation_response, :code_1, terminal_text(_), metadata()}

      Evaluator.evaluate_code(evaluator, :erlang, "tryme:macros().", :code_2, [:code_1])
      assert_receive {:runtime_evaluation_response, :code_2, terminal_text(output), metadata()}
      assert output == "{tryme,\"nofile\"}"
    end

    @tag tmp_dir: false
    test "evaluates erlang-module code without filesystem", %{evaluator: evaluator} do
      code = """
      -module(tryme).

      -export([go/0]).

      go() -> {ok,went}.
      """

      Evaluator.evaluate_code(evaluator, :erlang, code, :code_1, [])
      assert_receive {:runtime_evaluation_response, :code_1, error(message), metadata()}
      assert message =~ "writing Erlang modules requires a writeable file system"
    end

    @tag :with_ebin_path
    test "evaluates erlang-module error", %{
      evaluator: evaluator
    } do
      code = """
      -module(tryme).

      -export([go/0]).

      go() ->{ok,went}.
      go() ->{ok,went}.
      """

      Evaluator.evaluate_code(evaluator, :erlang, code, :code_1, [])

      assert_receive {:runtime_evaluation_response, :code_1, error(message), metadata()}

      assert message =~ "compile forms error"
    end

    test "mixed erlang/elixir bindings", %{evaluator: evaluator} do
      Evaluator.evaluate_code(evaluator, :elixir, "x = 1", :code_1, [])
      Evaluator.evaluate_code(evaluator, :erlang, "Y = X.", :code_2, [:code_1])
      Evaluator.evaluate_code(evaluator, :elixir, "z = y", :code_3, [:code_2])

      %{binding: binding} =
        Evaluator.get_evaluation_context(evaluator, [:code_3, :code_2, :code_1])

      assert [{:z, 1}, {:y, 1}, {:x, 1}] == binding
    end

    test "uses unambiguous camelization for erlang/elixir bindings", %{evaluator: evaluator} do
      Evaluator.evaluate_code(evaluator, :erlang, "{JSON, JsOn, JsON} = {1, 2, 3}.", :code_1, [])

      assert_receive {:runtime_evaluation_response, :code_1, terminal_text(_), metadata()}

      Evaluator.evaluate_code(
        evaluator,
        :elixir,
        """
        assertion1 = {j_s_o_n, js_on, js_o_n} == {1, 2, 3}
        {j_s_o_n, js_on, js_o_n} = {11, 12, 13}
        """,
        :code_2,
        [:code_1]
      )

      assert_receive {:runtime_evaluation_response, :code_2, terminal_text(_), metadata()}

      Evaluator.evaluate_code(
        evaluator,
        :erlang,
        """
        Assertion2 = {JSON, JsOn, JsON} =:= {11, 12, 13}.
        """,
        :code_3,
        [:code_2]
      )

      assert_receive {:runtime_evaluation_response, :code_3, terminal_text(_), metadata()}

      %{binding: binding} =
        Evaluator.get_evaluation_context(evaluator, [:code_3, :code_2, :code_1])

      assert [
               {:assertion2, true},
               {:js_on, 12},
               {:js_o_n, 13},
               {:j_s_o_n, 11},
               {:assertion1, true}
             ] == binding
    end

    test "inspects erlang results using erlang format", %{evaluator: evaluator} do
      code = ~S"#{x=>1}."
      Evaluator.evaluate_code(evaluator, :erlang, code, :code_1, [], file: "file.ex")

      assert_receive {:runtime_evaluation_response, :code_1, terminal_text(~S"#{x => 1}"),
                      metadata()}
    end

    test "does not return error marker on empty source", %{evaluator: evaluator} do
      Evaluator.evaluate_code(evaluator, :erlang, "", :code_1, [])

      assert_receive {:runtime_evaluation_response, :code_1, error(_), metadata() = metadata}

      assert metadata.code_markers == []
    end

    test "syntax and tokenizer errors are converted", %{evaluator: evaluator} do
      # Incomplete input
      Evaluator.evaluate_code(evaluator, :erlang, "X =", :code_1, [])
      assert_receive {:runtime_evaluation_response, :code_1, error(message), metadata()}

      assert clean_message(message) === """
             ** (TokenMissingError) token missing on nofile:1:4:
                 error: syntax error before:
                 │
               1 │ X =
                 │    ^
                 │
                 └─ nofile:1:4\
             """

      # Parser error
      Evaluator.evaluate_code(evaluator, :erlang, "X ==/== a.", :code_2, [])
      assert_receive {:runtime_evaluation_response, :code_2, error(message), metadata()}

      assert clean_message(message) === """
             ** (SyntaxError) invalid syntax found on nofile:1:5:
                 error: syntax error before: /=
                 │
               1 │ X ==/== a.
                 │     ^
                 │
                 └─ nofile:1:5\
             """

      # Tokenizer error
      Evaluator.evaluate_code(evaluator, :erlang, "$a$", :code_3, [])
      assert_receive {:runtime_evaluation_response, :code_3, error(message), metadata()}

      assert clean_message(message) === """
             ** (SyntaxError) invalid syntax found on nofile:1:3:
                 error: unterminated character
                 │
               1 │ $a$
                 │   ^
                 │
                 └─ nofile:1:3\
             """

      # Erlang exception
      Evaluator.evaluate_code(evaluator, :erlang, "list_to_binary(1).", :code_4, [])
      assert_receive {:runtime_evaluation_response, :code_4, error(message), metadata()}

      assert clean_message(message) =~ """
             exception error: bad argument
               in function  list_to_binary/1
                  called as list_to_binary(1)
                  *** argument 1: not an iolist term
               in call from erl_eval:do_apply/7 (erl_eval.erl, line\
             """
    end
  end

  describe "python evaluation" do
    test "evaluates python code", %{evaluator: evaluator} do
      code = """
      x = [1, 2, 3]
      sum(x)
      """

      Evaluator.evaluate_code(evaluator, :python, code, :code_1, [])

      assert_receive {:runtime_evaluation_response, :code_1, terminal_text("6"), metadata()}
    end

    test "uses and defines binding", %{evaluator: evaluator} do
      Evaluator.evaluate_code(evaluator, :elixir, "x = 1", :code_1, [])
      assert_receive {:runtime_evaluation_response, :code_1, _, metadata()}

      Evaluator.evaluate_code(evaluator, :python, "y = x", :code_2, [:code_1])
      assert_receive {:runtime_evaluation_response, :code_2, _, metadata()}

      Evaluator.evaluate_code(evaluator, :elixir, "z = y", :code_3, [:code_2, :code_1])
      assert_receive {:runtime_evaluation_response, :code_3, _, metadata()}

      %{binding: binding} =
        Evaluator.get_evaluation_context(evaluator, [:code_3, :code_2, :code_1])

      assert [{:z, %Pythonx.Object{}}, {:y, %Pythonx.Object{}}, {:x, 1}] = binding
    end

    test "syntax error", %{evaluator: evaluator} do
      Evaluator.evaluate_code(evaluator, :python, "1 +", :code_1, [])

      assert_receive {:runtime_evaluation_response, :code_1, error(message),
                      %{
                        code_markers: [
                          %{
                            line: 1,
                            description: "SyntaxError: invalid syntax",
                            severity: :error
                          }
                        ]
                      }}

      assert clean_message(message) == """
               File "<unknown>", line 1
                 1 +
                    ^
             SyntaxError: invalid syntax
             """
    end

    test "runtime error", %{evaluator: evaluator} do
      Evaluator.evaluate_code(evaluator, :python, "import unknown", :code_1, [])

      assert_receive {:runtime_evaluation_response, :code_1, error(message),
                      %{
                        code_markers: [
                          %{
                            line: 1,
                            description: "ModuleNotFoundError: No module named 'unknown'",
                            severity: :error
                          }
                        ]
                      }}

      assert clean_message(message) == """
             Traceback (most recent call last):
               File "<string>", line 1, in <module>
             ModuleNotFoundError: No module named 'unknown'
             """
    end

    test "captures standard output and sends it to the caller", %{evaluator: evaluator} do
      code = """
      print("hello from Python")
      """

      Evaluator.evaluate_code(evaluator, :python, code, :code_1, [])

      assert_receive {:runtime_evaluation_output, :code_1,
                      terminal_text("hello from Python\n", true)}
    end

    test "captures standard error and sends it to the caller", %{evaluator: evaluator} do
      code = """
      import sys
      print("error from Python", file=sys.stderr)
      """

      Evaluator.evaluate_code(evaluator, :python, code, :code_1, [])

      assert_receive {:runtime_evaluation_output, :code_1,
                      terminal_text("error from Python\n", true)}
    end
  end

  describe "formatting" do
    test "gracefully handles errors in the inspect protocol", %{evaluator: evaluator} do
      code = "%Livebook.TestModules.BadInspect{}"
      Evaluator.evaluate_code(evaluator, :elixir, code, :code_1, [], file: "file.ex")

      assert_receive {:runtime_evaluation_response, :code_1, error(message), metadata()}

      assert message =~ ":bad_return"
    end
  end

  # Helpers

  # Returns a code that spawns a widget process, registers
  # a pointer for it and adds monitoring, then returns widget
  # pid from the evaluation
  defp spawn_widget_code() do
    """
    widget_pid = spawn(fn ->
      receive do
        :stop -> :ok
      end
    end)

    ref = make_ref()
    send(Process.group_leader(), {:io_request, self(), ref, {:livebook_reference_object, widget_pid, self()}})

    receive do
      {:io_reply, ^ref, :ok} -> :ok
    end

    send(Process.group_leader(), {:io_request, self(), ref, {:livebook_monitor_object, widget_pid, widget_pid, :stop}})

    receive do
      {:io_reply, ^ref, :ok} -> :ok
    end

    widget_pid
    """
  end

  defp spawn_widget_from_terminating_process_code() do
    """
    parent = self()

    # Arbitrary process that spawns the widget and terminates afterwards
    spawn(fn ->
      widget_pid = spawn(fn ->
        receive do
          :stop -> :ok
        end
      end)

      ref = make_ref()
      send(Process.group_leader(), {:io_request, self(), ref, {:livebook_reference_object, widget_pid, self()}})

      receive do
        {:io_reply, ^ref, :ok} -> :ok
      end

      send(Process.group_leader(), {:io_request, self(), ref, {:livebook_monitor_object, widget_pid, widget_pid, :stop}})

      receive do
        {:io_reply, ^ref, :ok} -> :ok
      end

      send(parent, {:widget_pid, widget_pid})
    end)

    receive do
      {:widget_pid, widget_pid} -> widget_pid
    end
    """
  end
end
