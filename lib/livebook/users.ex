defmodule Livebook.Users do
  @moduledoc false

  alias Livebook.Users.User

  @doc """
  Notifies interested processes about user data change.

  Broadcasts `{:user_change, user}` message under the `"user:{id}"` topic.
  """
  @spec broadcast_change(User.t()) :: :ok
  def broadcast_change(user) do
    broadcast_user_message(user.id, {:user_change, user})
    :ok
  end

  defp broadcast_user_message(user_id, message) do
    Phoenix.PubSub.broadcast(Livebook.PubSub, "users:#{user_id}", message)
  end

  @doc """
  Subscribes to updates in user information.

  ## Messages

    * `{:user_change, user}`

  """
  @spec subscribe(User.id()) :: :ok | {:error, term()}
  def subscribe(user_id) do
    Phoenix.PubSub.subscribe(Livebook.PubSub, "users:#{user_id}")
  end

  @doc """
  Unsubscribes from `subscribe/1`.
  """
  @spec unsubscribe(User.id()) :: :ok
  def unsubscribe(user_id) do
    Phoenix.PubSub.unsubscribe(Livebook.PubSub, "users:#{user_id}")
  end
end
