defmodule Livebook.Notebook.WelcomeTest do
  use ExUnit.Case, async: true

  alias Livebook.Notebook
  alias Livebook.Notebook.Welcome

  test "new/0 correctly builds a new notebook" do
    assert %Notebook{} = Welcome.new()
  end
end
