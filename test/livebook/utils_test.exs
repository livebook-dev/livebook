defmodule Livebook.UtilsTest do
  use ExUnit.Case, async: true
  doctest Livebook.Utils

  describe "expand_desktop_url/1" do
    test "empty url" do
      assert Livebook.Utils.expand_desktop_url("") == "http://localhost:4002/"
    end

    test "/settings" do
      assert Livebook.Utils.expand_desktop_url("/settings") ==
               "http://localhost:4002/settings"
    end

    test "file://" do
      assert Livebook.Utils.expand_desktop_url("file://c/foo.txt") ==
               "http://localhost:4002/open?path=c%2Ffoo.txt"

      assert Livebook.Utils.expand_desktop_url("file://c/../../../foo.txt") ==
               "http://localhost:4002/open?path=c%2F..%2F..%2F..%2Ffoo.txt"

      assert Livebook.Utils.expand_desktop_url("file://c\\foo.txt") ==
               "http://localhost:4002/open?path=c%5Cfoo.txt"
    end

    test "livebook://" do
      assert Livebook.Utils.expand_desktop_url("livebook://github.com/a/b/blob/main/a.livemd") ==
               "http://localhost:4002/import?url=https%3A%2F%2Fgithub.com%2Fa%2Fb%2Fblob%2Fmain%2Fa.livemd"

      assert Livebook.Utils.expand_desktop_url("livebook://github.com/../../../a.livemd") ==
               "http://localhost:4002/import?url=https%3A%2F%2Fgithub.com%2F..%2F..%2F..%2Fa.livemd"
    end

    test "other urls" do
      assert_raise FunctionClauseError, fn ->
        Livebook.Utils.expand_desktop_url("file:a.txt")
      end

      assert_raise FunctionClauseError, fn ->
        Livebook.Utils.expand_desktop_url("livebook:a.txt")
      end

      assert_raise FunctionClauseError, fn ->
        Livebook.Utils.expand_desktop_url("a.txt")
      end
    end
  end

  describe "validate_mutual_inclusion/2" do
    setup do
      {:ok,
       %{
         types: %{
           name: :string,
           conditional_one: :integer,
           conditional_two: :integer,
           conditional_three: :integer
         }
       }}
    end

    test "all fields present", %{types: types} do
      changeset =
        Ecto.Changeset.change({%{}, types}, %{
          name: "name",
          conditional_one: 1,
          conditional_two: 2,
          conditional_three: 3
        })
        |> Livebook.Utils.validate_mutual_inclusion([
          :conditional_one,
          :conditional_two,
          :conditional_three
        ])

      assert changeset.valid?
    end

    test "no fields present", %{types: types} do
      changeset =
        Ecto.Changeset.change({%{}, types}, %{
          name: "name"
        })
        |> Livebook.Utils.validate_mutual_inclusion([
          :conditional_one,
          :conditional_two,
          :conditional_three
        ])

      assert changeset.valid?
    end

    test "some fields present", %{types: types} do
      changeset =
        Ecto.Changeset.change({%{}, types}, %{
          name: "name",
          conditional_one: 1,
          conditional_two: 2
        })
        |> Livebook.Utils.validate_mutual_inclusion([
          :conditional_one,
          :conditional_two,
          :conditional_three
        ])

      expected =
        {:conditional_one,
         {"if any of [:conditional_one, :conditional_two, :conditional_three] is present, all must be present",
          []}}

      refute changeset.valid?
      assert [^expected | []] = changeset.errors
    end

    test "any/either inflection", %{types: types} do
      changeset =
        Ecto.Changeset.change({%{}, types}, %{
          name: "name",
          conditional_one: 1,
          conditional_two: 2
        })
        |> Livebook.Utils.validate_mutual_inclusion([
          :conditional_two,
          :conditional_three
        ])

      expected =
        {:conditional_two,
         {"if either of [:conditional_two, :conditional_three] is present, both must be present",
          []}}

      refute changeset.valid?
      assert [^expected | []] = changeset.errors
    end
  end
end
