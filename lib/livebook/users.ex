defmodule Livebook.Users do
  @moduledoc false

  alias Livebook.Users.User

  @doc """
  Returns an `%Ecto.Changeset{}` for tracking user changes.

  ## Examples

      Livebook.Users.change_user(%Livebook.Users.User{})
      %Ecto.Changeset{data: %Livebook.Users.User{}, action: :validate}

  """
  @spec change_user(User.t(), map()) :: Ecto.Changeset.t()
  def change_user(%User{} = user, attrs \\ %{}) do
    user
    |> User.changeset(attrs)
    |> Map.put(:action, :validate)
  end

  @doc """
  Updates an User from given changeset.

  With success, notifies interested processes about user data change.
  Otherwise, it will return an error tuple with changeset.
  """
  @spec update_user(Ecto.Changeset.t()) :: {:ok, User.t()} | {:error, Ecto.Changeset.t()}
  def update_user(%Ecto.Changeset{} = changeset) do
    with {:ok, user} <- Ecto.Changeset.apply_action(changeset, :update) do
      broadcast_change(user)
      {:ok, user}
    end
  end

  @doc """
  Notifies interested processes about user data change.

  Broadcasts `{:user_change, user}` message under the `"user:{id}"` topic.
  """
  @spec broadcast_change(User.t()) :: :ok
  def broadcast_change(%User{} = user) do
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
