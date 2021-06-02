defmodule Livebook.ExploreTest do
  use ExUnit.Case, async: true

  alias Livebook.Notebook
  alias Livebook.Notebook.Explore

  describe "notebook_by_slug!/1" do
    test "returns notebook structure if found" do
      assert %Notebook{} = Explore.notebook_by_slug!("intro-to-livebook")
    end

    test "raises an error if no matching notebook if found" do
      assert_raise Explore.NotFoundError, fn ->
        Explore.notebook_by_slug!("invalid-slug")
      end
    end
  end
end
