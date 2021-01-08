defmodule LiveBook.EvaluatorTest do
  use ExUnit.Case, async: true

  alias LiveBook.Evaluator

  setup do
    evaluator = start_supervised!(Evaluator)
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

    test "given prev_ref does sees previous evaluation context", %{evaluator: evaluator} do
      Evaluator.evaluate_code(evaluator, "x = 1", :code_1)

      result = Evaluator.evaluate_code(evaluator, "x", :code_2, :code_1)

      assert result == {:ok, 1}
    end
  end
end
