defmodule LiveBook.EvaluatorTest do
  use ExUnit.Case, async: true

  alias LiveBook.Evaluator

  setup do
    {:ok, evaluator} = Evaluator.start_link()
    %{evaluator: evaluator}
  end

  describe "evaluate_code/4" do
    test "given a valid code returns evaluation result", %{evaluator: evaluator} do
      code = """
      x = 1
      y = 2
      x + y
      """

      result = Evaluator.evaluate_code(evaluator, code, 1)

      assert result == {:ok, 3}
    end

    test "given no prev_ref does not see previous evaluation context", %{evaluator: evaluator} do
      Evaluator.evaluate_code(evaluator, "x = 1", :code_1)

      result = Evaluator.evaluate_code(evaluator, "x", :code_2)

      assert {:error, _kind, %CompileError{description: "undefined function x/0"}, _stacktrace} =
               result
    end

    test "given prev_ref sees previous evaluation context", %{evaluator: evaluator} do
      Evaluator.evaluate_code(evaluator, "x = 1", :code_1)

      result = Evaluator.evaluate_code(evaluator, "x", :code_2, :code_1)

      assert result == {:ok, 1}
    end

    test "given invalid prev_ref raises an error", %{evaluator: evaluator} do
      assert_raise ArgumentError, fn ->
        Evaluator.evaluate_code(evaluator, ":ok", :code_1, :code_nonexistent)
      end
    end

    test "captures standard output and sends it to the caller", %{evaluator: evaluator} do
      Evaluator.evaluate_code(evaluator, ~s{IO.puts("hey")}, :code_1)

      assert_received {:evaluator_stdout, :code_1, "hey\n"}
    end

    test "using standard input results in an immediate error", %{evaluator: evaluator} do
      result = Evaluator.evaluate_code(evaluator, ~s{IO.gets("> ")}, :code_1)

      assert result == {:ok, {:error, :enotsup}}
    end

    test "returns error along with its kind and stacktrace", %{evaluator: evaluator} do
      code = """
      List.first(%{})
      """

      result = Evaluator.evaluate_code(evaluator, code, :code_1)

      assert {:error, :error, %FunctionClauseError{}, [{List, :first, 1, _location}]} = result
    end

    test "in case of an error returns only the relevant part of stacktrace", %{
      evaluator: evaluator
    } do
      code = """
      defmodule Math do
        def bad_math do
          result = 1 / 0
          {:ok, result}
        end
      end

      defmodule Cat do
        def meow do
          Math.bad_math()
          :ok
        end
      end

      Cat.meow()
      """

      result = Evaluator.evaluate_code(evaluator, code, :code_1)

      expected_stacktrace = [
        {Math, :bad_math, 0, [file: 'nofile', line: 3]},
        {Cat, :meow, 0, [file: 'nofile', line: 10]}
      ]

      assert {:error, _kind, _error, ^expected_stacktrace} = result
    end
  end

  describe "forget_evaluation/2" do
    test "invalidates the given reference", %{evaluator: evaluator} do
      Evaluator.evaluate_code(evaluator, "x = 1", :code_1)
      Evaluator.forget_evaluation(evaluator, :code_1)

      assert_raise ArgumentError, fn ->
        Evaluator.evaluate_code(evaluator, ":ok", :code_2, :code_1)
      end
    end
  end
end
