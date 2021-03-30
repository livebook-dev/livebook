defmodule Livebook.Notebook.HelloLivebookTest do
  use ExUnit.Case, async: true

  alias Livebook.Notebook
  alias Livebook.Notebook.HelloLivebook

  test "new/0 correctly builds a new notebook" do
    assert %Notebook{} = HelloLivebook.new()
  end
end
