defmodule LivebookWeb.UserPlug do
  @moduledoc false

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

        Users.save(user)
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
