defmodule Livebook.UsersTest do
  use ExUnit.Case, async: true

  alias Livebook.Users
  alias Livebook.Users.User

  describe "save/1" do
    test "inserts a new user if they don't exist" do
      user = User.new()
      :ok = Users.save(user)
      assert Users.fetch!(user.id) == user
    end

    test "updates the user if they do exist" do
      user = User.new()
      :ok = Users.save(user)

      updated_user = %{user | name: "Jake Peralta"}
      :ok = Users.save(updated_user)
      assert Users.fetch!(user.id) == updated_user
    end
  end

  describe "exists?/1" do
    test "returns true if the user exists" do
      user = User.new()
      :ok = Users.save(user)
      assert Users.exists?(user.id)
    end

    test "returns false if the user does not exist" do
      refute Users.exists?("nonexistent")
    end
  end

  describe "fetch!/1" do
    test "raises an error if no user with the given id exists" do
      assert_raise RuntimeError, fn ->
        Users.fetch!("nonexistent")
      end
    end
  end

  describe "list_by_ids/1" do
    test "returns an empty list if no user matches the given ids" do
      assert Users.list_by_ids(["nonexistent1", "nonexistent2"]) == []
    end

    test "returns users matching the given ids" do
      user1 = User.new()
      user2 = User.new()
      user3 = User.new()

      for user <- [user1, user2, user3], do: :ok = Users.save(user)

      assert Users.list_by_ids([user1.id, user3.id]) |> Enum.sort() ==
               [user1, user3] |> Enum.sort()
    end
  end
end
