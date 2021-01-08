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

      assert {:error, %CompileError{description: "undefined function x/0"}} = result
    end

    test "given prev_ref sees previous evaluation context", %{evaluator: evaluator} do
      Evaluator.evaluate_code(evaluator, "x = 1", :code_1)

      result = Evaluator.evaluate_code(evaluator, "x", :code_2, :code_1)

      assert result == {:ok, 1}
    end

    test "captures standard output and sends it to the caller", %{evaluator: evaluator} do
      Evaluator.evaluate_code(evaluator, ~s{IO.puts("hey")}, :code_1)

      assert_received {:evaluator_stdout, :code_1, "hey\n"}
    end

    test "using standard input results in an immediate error", %{evaluator: evaluator} do
      result = Evaluator.evaluate_code(evaluator, ~s{IO.gets("> ")}, :code_1)

      assert result == {:ok, {:error, :enotsup}}
    end
  end
end
