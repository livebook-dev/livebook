defmodule Livebook.Users do
  @moduledoc false

  @users_table :livebook_users

  def initialize_store() do
    :ets.new(@users_table, [:set, :public, :named_table])
    :ok
  end

  def exists?(user_id) do
    :ets.member(@users_table, user_id)
  end

  def save(user) do
    :ets.insert(@users_table, {user.id, user})
    broadcast_user_message(user.id, {:user_saved, user})
    :ok
  end

  def list_by_ids(user_ids) do
    match_spec = for user_id <- user_ids, do: {{user_id, :_}, [], [:"$_"]}
    results = :ets.select(@users_table, match_spec)
    Enum.map(results, &elem(&1, 1))
  end

  def fetch!(user_id) do
    case :ets.lookup(@users_table, user_id) do
      [{_id, user}] -> user
      _ -> raise "expected to find user with id #{user_id}, but found none"
    end
  end

  defp broadcast_user_message(user_id, message) do
    Phoenix.PubSub.broadcast(Livebook.PubSub, "users:#{user_id}", message)
  end
end
