defmodule Livebook.Users.UserTest do
  use Livebook.DataCase, async: true

  alias Livebook.Users.User

  describe "change/2" do
    test "given valid attributes returns and updated user" do
      user = build(:user)

      attrs = %{
        "name" => "Jake Peralta",
        "hex_color" => "#000000",
        "payload" => %{"country" => "US"}
      }

      changeset = User.changeset(user, attrs)

      assert changeset.valid?
      assert get_field(changeset, :name) == "Jake Peralta"
      assert get_field(changeset, :hex_color) == "#000000"
      assert get_field(changeset, :payload) == %{"country" => "US"}
    end

    test "given invalid color returns an error" do
      user = build(:user)
      attrs = %{"hex_color" => "#invalid"}
      changeset = User.changeset(user, attrs)

      refute changeset.valid?
      assert "not a valid color" in errors_on(changeset).hex_color
    end
  end
end
