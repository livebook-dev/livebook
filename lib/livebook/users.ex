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
end
