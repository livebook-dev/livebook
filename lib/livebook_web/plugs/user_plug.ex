defmodule LivebookWeb.UserPlug do
  @moduledoc false

  # Ensures a valid user in the session.
  #
  # The first time someone visits Livebook
  # this plug generates a user for them and stores
  # the id in the session under `:current_user_id`.
  #
  # If the request cookies include `"user_data"` JSON,
  # this data is used to initialize the new user.
  # This cookie serves as a decentralized backup of user attributes,
  # because Livebook doesn't use persistent storage.

  @behaviour Plug

  import Plug.Conn

  alias Livebook.Users
  alias Livebook.Users.User

  @impl true
  def init(opts), do: opts

  @impl true
  def call(conn, _opts) do
    with user_id when user_id != nil <- get_session(conn, :current_user_id),
         true <- Users.exists?(user_id) do
      conn
    else
      _ ->
        user =
          User.new()
          |> User.change(get_user_attrs(conn))
          |> case do
            {:ok, user} -> user
            {:error, _errors, user} -> user
          end

        :ok = Users.save(user)
        put_session(conn, :current_user_id, user.id)
    end
  end

  defp get_user_attrs(conn) do
    case Map.fetch(conn.req_cookies, "user_data") do
      {:ok, user_data} ->
        Jason.decode!(user_data)

      :error ->
        %{}
    end
  end
end
