defmodule Livebook.Users do
  @moduledoc false

  # Module responsible for user storage
  # and broadcasting change notifications.
  #
  # Users are stored in an ETS table.

  alias Livebook.Users.User

  @users_table :livebook_users

  @doc """
  Creates the ETS users table.
  """
  @spec initialize_store() :: :ok
  def initialize_store() do
    :ets.new(@users_table, [:set, :public, :named_table])
    :ok
  end

  @doc """
  Saves the given user in the store.

  If a user with the given `id` is already in the store
  it gets replaced, so this function either inserts
  or updates the user.

  Broadcasts `{:user_saved, user}` message under the `"user:{id}"` topic.
  """
  @spec save(User.t()) :: :ok
  def save(user) do
    :ets.insert(@users_table, {user.id, user})
    broadcast_user_message(user.id, {:user_saved, user})
    :ok
  end

  @doc """
  Checks if a user with the given id already exists.
  """
  @spec exists?(User.id()) :: boolean()
  def exists?(user_id) do
    :ets.member(@users_table, user_id)
  end

  @doc """
  Find user with the given id in the store.

  Raises and error if no user is found.
  """
  @spec fetch!(User.id()) :: User.t()
  def fetch!(user_id) do
    case :ets.lookup(@users_table, user_id) do
      [{_id, user}] -> user
      _ -> raise "expected to find user with id #{user_id}, but found none"
    end
  end

  @doc """
  Retrieves users from the store for the given list of ids.
  """
  @spec list_by_ids(list(User.id())) :: list(User.t())
  def list_by_ids(user_ids) do
    match_spec = for user_id <- user_ids, do: {{user_id, :_}, [], [:"$_"]}
    results = :ets.select(@users_table, match_spec)
    Enum.map(results, &elem(&1, 1))
  end

  defp broadcast_user_message(user_id, message) do
    Phoenix.PubSub.broadcast(Livebook.PubSub, "users:#{user_id}", message)
  end
end
