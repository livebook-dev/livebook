defmodule Livebook.LearnTest do
  use ExUnit.Case, async: true

  alias Livebook.Notebook
  alias Livebook.Notebook.Learn

  describe "notebook_by_slug!/1" do
    test "returns notebook structure and images if found" do
      assert {%Notebook{}, _imaegs} = Learn.notebook_by_slug!("intro-to-livebook")
    end

    test "raises an error if no matching notebook if found" do
      assert_raise Learn.NotFoundError, fn ->
        Learn.notebook_by_slug!("invalid-slug")
      end
    end
  end
end
