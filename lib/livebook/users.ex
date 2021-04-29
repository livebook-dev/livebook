defmodule Livebook.Users do
  @moduledoc false

  alias Livebook.Users.User

  # TODO: docs

  @users_table :livebook_users

  def initialize_store() do
    :ets.new(@users_table, [:set, :public, :named_table])
    :ok
  end

  def exists?(user_id) do
    :ets.member(@users_table, user_id)
  end

  def create(attrs \\ %{}) do
    user = User.new(attrs)
    :ets.insert(@users_table, {user.id, user})
    {:ok, user}
  end
end
