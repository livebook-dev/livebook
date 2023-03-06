defmodule Livebook.Runtime.EvaluatorTest do
  use ExUnit.Case, async: true

  alias Livebook.Runtime.Evaluator

  setup ctx do
    ebin_path =
      if ctx[:with_ebin_path] do
        hash = ctx.test |> to_string() |> :erlang.md5() |> Base.encode32(padding: false)
        path = ["tmp", inspect(ctx.module), hash, "ebin"] |> Path.join() |> Path.expand()
        File.rm_rf!(path)
        File.mkdir_p!(path)
        Code.append_path(path)
        path
      end

    {:ok, object_tracker} = start_supervised(Evaluator.ObjectTracker)

    {:ok, _pid, evaluator} =
      start_supervised(
        {Evaluator, [send_to: self(), object_tracker: object_tracker, ebin_path: ebin_path]}
      )

    %{evaluator: evaluator, object_tracker: object_tracker, ebin_path: ebin_path}
  end

  defmacrop metadata do
    quote do
      %{
        evaluation_time_ms: _,
        memory_usage: %{},
        code_error: _,
        identifiers_used: _,
        identifiers_defined: _
      }
    end
  end

  describe "evaluate_code/6" do
    test "given a valid code returns evaluation result", %{evaluator: evaluator} do
      code = """
      x = 1
      y = 2
      x + y
      """

      Evaluator.evaluate_code(evaluator, code, :code_1, [])

      assert_receive {:runtime_evaluation_response, :code_1, {:ok, 3}, metadata() = metadata}
      assert metadata.evaluation_time_ms >= 0

      assert %{atom: _, binary: _, code: _, ets: _, other: _, processes: _, total: _} =
               metadata.memory_usage
    end

    test "given no parent refs does not see previous evaluation context", %{evaluator: evaluator} do
      Evaluator.evaluate_code(evaluator, "x = 1", :code_1, [])
      assert_receive {:runtime_evaluation_response, :code_1, _, metadata()}

      ignore_warnings(fn ->
        Evaluator.evaluate_code(evaluator, "x", :code_2, [])

        assert_receive {:runtime_evaluation_response, :code_2,
                        {:error, _kind,
                         %CompileError{
                           description: "undefined function x/0 (there is no such import)"
                         }, _stacktrace}, metadata()}
      end)
    end

    test "given parent refs sees previous evaluation context", %{evaluator: evaluator} do
      Evaluator.evaluate_code(evaluator, "x = 1", :code_1, [])
      assert_receive {:runtime_evaluation_response, :code_1, _, metadata()}

      Evaluator.evaluate_code(evaluator, "x", :code_2, [:code_1])

      assert_receive {:runtime_evaluation_response, :code_2, {:ok, 1}, metadata()}
    end

    test "given invalid parent ref uses the default context", %{evaluator: evaluator} do
      Evaluator.evaluate_code(evaluator, ":hey", :code_1, [:code_nonexistent])

      assert_receive {:runtime_evaluation_response, :code_1, {:ok, :hey}, metadata()}
    end

    test "given parent refs sees previous process dictionary", %{evaluator: evaluator} do
      Evaluator.evaluate_code(evaluator, "Process.put(:x, 1)", :code_1, [])
      assert_receive {:runtime_evaluation_response, :code_1, _, metadata()}
      Evaluator.evaluate_code(evaluator, "Process.put(:x, 2)", :code_2, [])
      assert_receive {:runtime_evaluation_response, :code_2, _, metadata()}

      Evaluator.evaluate_code(evaluator, "Process.get(:x)", :code_3, [:code_1])
      assert_receive {:runtime_evaluation_response, :code_3, {:ok, 1}, metadata()}

      Evaluator.evaluate_code(evaluator, "Process.get(:x)", :code_3, [:code_2])
      assert_receive {:runtime_evaluation_response, :code_3, {:ok, 2}, metadata()}
    end

    test "keeps :rand state intact in process dictionary", %{evaluator: evaluator} do
      Evaluator.evaluate_code(evaluator, ":rand.seed(:default, 0)", :code_1, [])
      assert_receive {:runtime_evaluation_response, :code_1, _, metadata()}

      Evaluator.evaluate_code(evaluator, ":rand.uniform()", :code_2, [])
      assert_receive {:runtime_evaluation_response, :code_2, {:ok, number1}, metadata()}

      Evaluator.evaluate_code(evaluator, ":rand.uniform()", :code_2, [])
      assert_receive {:runtime_evaluation_response, :code_2, {:ok, number2}, metadata()}

      assert number1 != number2

      Evaluator.evaluate_code(evaluator, ":rand.seed(:default, 0)", :code_1, [])
      assert_receive {:runtime_evaluation_response, :code_1, _, metadata()}

      Evaluator.evaluate_code(evaluator, ":rand.uniform()", :code_2, [])
      assert_receive {:runtime_evaluation_response, :code_2, {:ok, ^number1}, metadata()}

      Evaluator.evaluate_code(evaluator, ":rand.uniform()", :code_2, [])
      assert_receive {:runtime_evaluation_response, :code_2, {:ok, ^number2}, metadata()}
    end

    test "captures standard output and sends it to the caller", %{evaluator: evaluator} do
      Evaluator.evaluate_code(evaluator, ~s{IO.puts("hey")}, :code_1, [])

      assert_receive {:runtime_evaluation_output, :code_1, {:stdout, "hey\n"}}
    end

    test "using livebook input sends input request to the caller", %{evaluator: evaluator} do
      code = """
      ref = make_ref()
      send(Process.group_leader(), {:io_request, self(), ref, {:livebook_get_input_value, "input1"}})

      receive do
        {:io_reply, ^ref, {:ok, value}} -> value
      end
      """

      Evaluator.evaluate_code(evaluator, code, :code_1, [])

      assert_receive {:runtime_evaluation_input, :code_1, reply_to, "input1"}
      send(reply_to, {:runtime_evaluation_input_reply, {:ok, :value}})

      assert_receive {:runtime_evaluation_response, :code_1, {:ok, :value}, metadata()}
    end

    test "returns error along with its kind and stacktrace", %{evaluator: evaluator} do
      code = """
      List.first(%{})
      """

      Evaluator.evaluate_code(evaluator, code, :code_1, [], file: "file.ex")

      assert_receive {:runtime_evaluation_response, :code_1,
                      {:error, :error, :function_clause,
                       [
                         {List, :first, _arity, _location1},
                         {:elixir_eval, :__FILE__, 1, _location2}
                       ]}, metadata()}
    end

    test "returns additional metadata when there is a syntax error", %{evaluator: evaluator} do
      code = "1+"

      Evaluator.evaluate_code(evaluator, code, :code_1, [], file: "file.ex")

      assert_receive {:runtime_evaluation_response, :code_1,
                      {:error, :error, %TokenMissingError{}, _stacktrace},
                      %{
                        code_error: %{
                          line: 1,
                          description: "syntax error: expression is incomplete"
                        }
                      }}
    end

    test "returns additional metadata when there is a compilation error", %{evaluator: evaluator} do
      code = "x"

      Evaluator.evaluate_code(evaluator, code, :code_1, [], file: "file.ex")

      assert_receive {:runtime_evaluation_response, :code_1,
                      {:error, :error, %CompileError{}, _stacktrace},
                      %{
                        code_error: %{
                          line: 1,
                          description: "undefined function x/0 (there is no such import)"
                        }
                      }}
    end

    test "ignores code errors when they happen in the actual evaluation", %{evaluator: evaluator} do
      code = """
      Code.eval_string("x")
      """

      Evaluator.evaluate_code(evaluator, code, :code_1, [], file: "file.ex")

      expected_stacktrace = [
        {:elixir_eval, :__FILE__, 1, [file: ~c"file.ex", line: 1]}
      ]

      assert_receive {:runtime_evaluation_response, :code_1,
                      {:error, :error, %CompileError{}, ^expected_stacktrace}, %{code_error: nil}}
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

      ignore_warnings(fn ->
        Evaluator.evaluate_code(evaluator, code, :code_1, [])

        expected_stacktrace = [
          {Livebook.Runtime.EvaluatorTest.Stacktrace.Math, :bad_math, 0,
           [file: ~c"nofile", line: 3]},
          {Livebook.Runtime.EvaluatorTest.Stacktrace.Cat, :meow, 0, [file: ~c"nofile", line: 10]},
          {:elixir_eval, :__FILE__, 1, [file: ~c"nofile", line: 15]}
        ]

        # Note: evaluating module definitions is relatively slow, so we use a higher wait timeout.
        assert_receive {:runtime_evaluation_response, :code_1,
                        {:error, _kind, _error, ^expected_stacktrace}, metadata()},
                       2_000
      end)
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

      Evaluator.evaluate_code(evaluator, code1, :code_1, [])
      assert_receive {:runtime_evaluation_response, :code_1, {:ok, _}, metadata()}

      Evaluator.evaluate_code(evaluator, code2, :code_2, [:code_1])

      assert_receive {:runtime_evaluation_response, :code_2, {:error, _, _, _}, metadata()}

      Evaluator.evaluate_code(evaluator, code3, :code_3, [:code_2, :code_1])
      assert_receive {:runtime_evaluation_response, :code_3, {:ok, 4}, metadata()}
    end

    test "given file option sets it in evaluation environment", %{evaluator: evaluator} do
      code = """
      __DIR__
      """

      opts = [file: "/path/dir/file"]
      Evaluator.evaluate_code(evaluator, code, :code_1, [], opts)

      assert_receive {:runtime_evaluation_response, :code_1, {:ok, "/path/dir"}, metadata()}
    end

    test "kills widgets that that no evaluation points to", %{evaluator: evaluator} do
      # Evaluate the code twice, each time a new widget is spawned.
      # The evaluation reference is the same, so the second one overrides
      # the first one and the first widget should eventually be killed.

      Evaluator.evaluate_code(evaluator, spawn_widget_code(), :code_1, [])

      assert_receive {:runtime_evaluation_response, :code_1, {:ok, widget_pid1}, metadata()}

      ref = Process.monitor(widget_pid1)

      Evaluator.evaluate_code(evaluator, spawn_widget_code(), :code_1, [])

      assert_receive {:runtime_evaluation_response, :code_1, {:ok, widget_pid2}, metadata()}

      assert_receive {:DOWN, ^ref, :process, ^widget_pid1, _reason}

      assert Process.alive?(widget_pid2)
    end

    test "kills widgets when the spawning process terminates", %{evaluator: evaluator} do
      # The widget is spawned from a process that terminates,
      # so the widget should terminate immediately as well

      Evaluator.evaluate_code(
        evaluator,
        spawn_widget_from_terminating_process_code(),
        :code_1,
        []
      )

      assert_receive {:runtime_evaluation_response, :code_1, {:ok, widget_pid1}, metadata()}

      ref = Process.monitor(widget_pid1)
      assert_receive {:DOWN, ^ref, :process, ^widget_pid1, _reason}
    end

    @tag :with_ebin_path
    test "raises when redefining a module in a different evaluation", %{evaluator: evaluator} do
      code = """
      defmodule Livebook.Runtime.EvaluatorTest.Redefinition do
      end
      """

      Evaluator.evaluate_code(evaluator, code, :code_1, [])
      assert_receive {:runtime_evaluation_response, :code_1, {:ok, _}, metadata()}

      # Redefining in the same evaluation works
      Evaluator.evaluate_code(evaluator, code, :code_1, [])
      assert_receive {:runtime_evaluation_response, :code_1, {:ok, _}, metadata()}

      Evaluator.evaluate_code(evaluator, code, :code_2, [], file: "file.ex")

      assert_receive {:runtime_evaluation_response, :code_2,
                      {:error, :error, %CompileError{}, []},
                      %{
                        code_error: %{
                          line: 1,
                          description:
                            "module Livebook.Runtime.EvaluatorTest.Redefinition is already defined"
                        }
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

      Evaluator.evaluate_code(evaluator, code, :code_1, [])
      assert_receive {:runtime_evaluation_response, :code_1, {:ok, _}, metadata()}

      assert File.exists?(Path.join(ebin_path, "Elixir.Livebook.Runtime.EvaluatorTest.Disk.beam"))

      assert {:docs_v1, _, _, _, _, _, _} = Code.fetch_docs(Livebook.Runtime.EvaluatorTest.Disk)
    end

    test "deletes defined modules if the evaluation fails", %{evaluator: evaluator} do
      code = """
      defmodule Livebook.Runtime.EvaluatorTest.Raised do
      end

      raise "failed"
      """

      Evaluator.evaluate_code(evaluator, code, :code_1, [])
      assert_receive {:runtime_evaluation_response, :code_1, {:error, _, _, _}, metadata()}

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

      Evaluator.evaluate_code(evaluator, code, :code_1, [])

      ref = Process.monitor(gl)
      assert_receive {:DOWN, ^ref, :process, ^gl, _reason}

      refute Code.ensure_loaded?(Livebook.Runtime.EvaluatorTest.Exited)
    end

    @tag :with_ebin_path
    test "runs doctests when a module is defined", %{evaluator: evaluator} do
      code = ~S'''
      defmodule Livebook.Runtime.EvaluatorTest.Doctests do
        @moduledoc """

            iex> raise "oops"
            ** (ArgumentError) not oops

            iex> 1 +
            :who_knows

            iex> 1 = 2

            iex> require ExUnit.Assertions
            ...> ExUnit.Assertions.assert false
        """

        @doc """
            iex> Livebook.Runtime.EvaluatorTest.Doctests.data()
            %{
              name: "Amy Santiago",
              description: "nypd detective",
              precinct: 99
            }

            iex> Livebook.Runtime.EvaluatorTest.Doctests.data()
            %{name: "Jake Peralta", description: "NYPD detective"}
        """
        def data() do
          %{
            name: "Amy Santiago",
            description: "nypd detective",
            precinct: 99
          }
        end

        @doc """
            iex> Livebook.Runtime.EvaluatorTest.Doctests.raise_with_stacktrace()
            :what
        """
        def raise_with_stacktrace() do
          Enum.map(1, & &1)
        end

        @doc """
            iex> Livebook.Runtime.EvaluatorTest.Doctests.exit()
            :what
        """
        def exit() do
          Process.exit(self(), :shutdown)
        end
      end
      '''

      Evaluator.evaluate_code(evaluator, code, :code_1, [])

      assert_receive {:runtime_evaluation_output, :code_1, {:text, doctest_result}}

      assert doctest_result =~ "8 doctests, 7 failures"
      assert doctest_result =~ "Doctest did not compile, got: (TokenMissingError)"

      assert doctest_result =~
               "expected exception ArgumentError but got RuntimeError with message \"oops\""

      assert_receive {:runtime_evaluation_response, :code_1, {:ok, _}, metadata()}
    end
  end

  describe "evaluate_code/6 identifier tracking" do
    defp eval(code, evaluator, eval_idx) do
      ref = eval_idx
      parent_refs = Enum.to_list((eval_idx - 1)..0//-1)
      Evaluator.evaluate_code(evaluator, code, ref, parent_refs)
      assert_receive {:runtime_evaluation_response, ^ref, {:ok, _}, metadata}
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

    test "reports parentheses-less arity-0 import as a used variable", %{evaluator: evaluator} do
      identifiers =
        """
        self
        """
        |> eval(evaluator, 0)

      assert {:variable, {:self, nil}} in identifiers.used
      assert :imports in identifiers.used
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
  end

  describe "forget_evaluation/2" do
    test "invalidates the given reference", %{evaluator: evaluator} do
      Evaluator.evaluate_code(evaluator, "x = 1", :code_1, [])
      assert_receive {:runtime_evaluation_response, :code_1, _, metadata()}

      Evaluator.forget_evaluation(evaluator, :code_1)

      ignore_warnings(fn ->
        Evaluator.evaluate_code(evaluator, "x", :code_2, [:code_1])

        assert_receive {:runtime_evaluation_response, :code_2,
                        {:error, _kind,
                         %CompileError{
                           description: "undefined function x/0 (there is no such import)"
                         }, _stacktrace}, metadata()}
      end)
    end

    test "kills widgets that no evaluation points to", %{evaluator: evaluator} do
      Evaluator.evaluate_code(evaluator, spawn_widget_code(), :code_1, [])

      assert_receive {:runtime_evaluation_response, :code_1, {:ok, widget_pid1}, metadata()}

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

      Evaluator.evaluate_code(evaluator, code, :code_1, [])
      assert_receive {:runtime_evaluation_response, :code_1, {:ok, _}, metadata()}

      Evaluator.forget_evaluation(evaluator, :code_1)

      # Define the module in a different evaluation
      Evaluator.evaluate_code(evaluator, code, :code_2, [])
      assert_receive {:runtime_evaluation_response, :code_2, {:ok, _}, metadata()}
    end
  end

  describe "initialize_from/3" do
    setup %{object_tracker: object_tracker} do
      {:ok, _pid, parent_evaluator} =
        start_supervised({Evaluator, [send_to: self(), object_tracker: object_tracker]},
          id: :parent_evaluator
        )

      %{parent_evaluator: parent_evaluator}
    end

    test "copies the given context and sets as the initial one",
         %{evaluator: evaluator, parent_evaluator: parent_evaluator} do
      Evaluator.evaluate_code(parent_evaluator, "x = 1", :code_1, [])
      assert_receive {:runtime_evaluation_response, :code_1, _, metadata()}

      Evaluator.initialize_from(evaluator, parent_evaluator, [:code_1])

      Evaluator.evaluate_code(evaluator, "x", :code_2, [])
      assert_receive {:runtime_evaluation_response, :code_2, {:ok, 1}, metadata()}
    end
  end

  describe "binding order" do
    test "keeps binding in evaluation order, starting from most recent", %{evaluator: evaluator} do
      Evaluator.evaluate_code(evaluator, "b = 1", :code_1, [])
      Evaluator.evaluate_code(evaluator, "a = 1", :code_2, [:code_1])
      Evaluator.evaluate_code(evaluator, "c = 1", :code_3, [:code_2, :code_1])
      Evaluator.evaluate_code(evaluator, "x = 1", :code_4, [:code_3, :code_2, :code_1])

      %{binding: binding} =
        Evaluator.get_evaluation_context(evaluator, [:code_4, :code_3, :code_2, :code_1])

      assert [:x, :c, :a, :b] == Enum.map(binding, &elem(&1, 0))
    end

    test "treats rebound names as new", %{evaluator: evaluator} do
      Evaluator.evaluate_code(evaluator, "b = 1", :code_1, [])
      Evaluator.evaluate_code(evaluator, "a = 1", :code_2, [:code_1])
      Evaluator.evaluate_code(evaluator, "b = 2", :code_3, [:code_2, :code_1])

      %{binding: binding} =
        Evaluator.get_evaluation_context(evaluator, [:code_3, :code_2, :code_1])

      assert [:b, :a] == Enum.map(binding, &elem(&1, 0))
    end
  end

  # Helpers

  # Some of the code passed to Evaluator above is expected
  # to produce compilation warnings, so we ignore them.
  defp ignore_warnings(fun) do
    ExUnit.CaptureIO.capture_io(:stderr, fun)
    :ok
  end

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
