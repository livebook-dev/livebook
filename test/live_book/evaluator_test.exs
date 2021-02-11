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

      Evaluator.evaluate_code(evaluator, self(), code, :code_1)

      assert_receive {:evaluation_response, :code_1, {:ok, 3}}
    end

    test "given no prev_ref does not see previous evaluation context", %{evaluator: evaluator} do
      Evaluator.evaluate_code(evaluator, self(), "x = 1", :code_1)
      assert_receive {:evaluation_response, :code_1, _}

      Evaluator.evaluate_code(evaluator, self(), "x", :code_2)

      assert_receive {:evaluation_response, :code_2,
                      {:error, _kind, %CompileError{description: "undefined function x/0"},
                       _stacktrace}}
    end

    test "given prev_ref sees previous evaluation context", %{evaluator: evaluator} do
      Evaluator.evaluate_code(evaluator, self(), "x = 1", :code_1)
      assert_receive {:evaluation_response, :code_1, _}

      Evaluator.evaluate_code(evaluator, self(), "x", :code_2, :code_1)

      assert_receive {:evaluation_response, :code_2, {:ok, 1}}
    end

    test "given invalid prev_ref just uses default context", %{evaluator: evaluator} do
      Evaluator.evaluate_code(evaluator, self(), ":hey", :code_1, :code_nonexistent)

      assert_receive {:evaluation_response, :code_1, {:ok, :hey}}
    end

    test "captures standard output and sends it to the caller", %{evaluator: evaluator} do
      Evaluator.evaluate_code(evaluator, self(), ~s{IO.puts("hey")}, :code_1)

      assert_receive {:evaluation_stdout, :code_1, "hey\n"}
    end

    test "using standard input results in an immediate error", %{evaluator: evaluator} do
      Evaluator.evaluate_code(evaluator, self(), ~s{IO.gets("> ")}, :code_1)

      assert_receive {:evaluation_response, :code_1, {:ok, {:error, :enotsup}}}
    end

    test "returns error along with its kind and stacktrace", %{evaluator: evaluator} do
      code = """
      List.first(%{})
      """

      Evaluator.evaluate_code(evaluator, self(), code, :code_1)

      assert_receive {:evaluation_response, :code_1,
                      {:error, :error, %FunctionClauseError{}, [{List, :first, 1, _location}]}}
    end

    test "in case of an error returns only the relevant part of stacktrace", %{
      evaluator: evaluator
    } do
      code = """
      defmodule LiveBook.EvaluatorTest.Stacktrace.Math do
        def bad_math do
          result = 1 / 0
          {:ok, result}
        end
      end

      defmodule LiveBook.EvaluatorTest.Stacktrace.Cat do
        def meow do
          LiveBook.EvaluatorTest.Stacktrace.Math.bad_math()
          :ok
        end
      end

      LiveBook.EvaluatorTest.Stacktrace.Cat.meow()
      """

      Evaluator.evaluate_code(evaluator, self(), code, :code_1)

      expected_stacktrace = [
        {LiveBook.EvaluatorTest.Stacktrace.Math, :bad_math, 0, [file: 'nofile', line: 3]},
        {LiveBook.EvaluatorTest.Stacktrace.Cat, :meow, 0, [file: 'nofile', line: 10]}
      ]

      # Note: evaluating module definitions is relatively slow, so we use a higher wait timeout.
      assert_receive {:evaluation_response, :code_1,
                      {:error, _kind, _error, ^expected_stacktrace}},
                     1000
    end
  end

  describe "forget_evaluation/2" do
    test "invalidates the given reference", %{evaluator: evaluator} do
      Evaluator.evaluate_code(evaluator, self(), "x = 1", :code_1)
      assert_receive {:evaluation_response, :code_1, _}

      Evaluator.forget_evaluation(evaluator, :code_1)
      Evaluator.evaluate_code(evaluator, self(), "x", :code_2, :code_1)

      assert_receive {:evaluation_response, :code_2,
                      {:error, _kind, %CompileError{description: "undefined function x/0"},
                       _stacktrace}}
    end
  end
end
