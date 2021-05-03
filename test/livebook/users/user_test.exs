defmodule Livebook.Users.UserTest do
  use ExUnit.Case, async: true

  alias Livebook.Users.User

  describe "change/2" do
    test "given valid attributes returns and updated user" do
      user = User.new()
      attrs = %{"name" => "Jake Peralta", "hex_color" => "#000000"}
      assert {:ok, %User{name: "Jake Peralta", hex_color: "#000000"}} = User.change(user, attrs)
    end

    test "given empty name sets name to nil" do
      user = User.new()
      attrs = %{"name" => ""}
      assert {:ok, %User{name: nil}} = User.change(user, attrs)
    end

    test "given invalid color returns an error" do
      user = User.new()
      attrs = %{"hex_color" => "#invalid"}
      assert {:error, [{:hex_color, "not a valid color"}], _user} = User.change(user, attrs)
    end

    test "given invalid attribute partially updates the user" do
      user = User.new()
      current_hex_color = user.hex_color
      attrs = %{"hex_color" => "#invalid", "name" => "Jake Peralta"}

      assert {:error, _errors, %User{name: "Jake Peralta", hex_color: ^current_hex_color}} =
               User.change(user, attrs)
    end
  end
end
