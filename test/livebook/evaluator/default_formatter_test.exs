defmodule Livebook.Evaluator.DefaultFormatterTest do
  use ExUnit.Case, async: true

  alias Livebook.Evaluator.DefaultFormatter

  test "inspects successful results" do
    result = 10
    assert {:text, "\e[34m10\e[0m"} = DefaultFormatter.format_response({:ok, result})
  end

  test "gracefully handles errors in the inspect protocol" do
    result = %Livebook.TestModules.BadInspect{}
    assert {:error, error, :other} = DefaultFormatter.format_response({:ok, result})
    assert error =~ ":bad_return"
  end
end
