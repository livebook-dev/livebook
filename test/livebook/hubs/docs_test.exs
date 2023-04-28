defmodule Livebook.Intellisense.DocsTest do
  use ExUnit.Case, async: true

  alias Livebook.Intellisense.Docs

  test "any_docs?/1" do
    refute Docs.any_docs?(Livebook.TestModules.Docs.Without)
    refute Docs.any_docs?(Livebook.TestModules.Docs.ModuleHidden)
    refute Docs.any_docs?(Livebook.TestModules.Docs.FunctionHidden)
    assert Docs.any_docs?(Livebook.TestModules.Docs.Module)
    assert Docs.any_docs?(Livebook.TestModules.Docs.Function)
  end
end
