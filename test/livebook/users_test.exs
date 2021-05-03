defmodule Livebook.UsersTest do
  use ExUnit.Case, async: true

  alias Livebook.Users
  alias Livebook.Users.User

  describe "broadcast_change/1" do
    test "notifies subscribers of user change" do
      user = User.new()
      Phoenix.PubSub.subscribe(Livebook.PubSub, "users:#{user.id}")

      Users.broadcast_change(user)

      assert_received {:user_change, ^user}
    end
  end
end
