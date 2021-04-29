defmodule LivebookWeb.UserPlug do
  @moduledoc false

  @behaviour Plug

  import Plug.Conn

  alias Livebook.Users

  @impl true
  def init(opts), do: opts

  @impl true
  def call(conn, _opts) do
    with user_id when user_id != nil <- get_session(conn, :current_user_id),
         true <- Users.exists?(user_id) do
      conn
    else
      _ ->
        {:ok, user} = Users.create(get_user_data(conn))
        put_session(conn, :current_user_id, user.id)
    end
  end

  defp get_user_data(conn) do
    # TODO: where to validate?
    case Map.fetch(conn.req_cookies, "user_data") do
      {:ok, user_data} ->
        user_data = Jason.decode!(user_data)
        # TODO: handle properly validations etc
        %{name: user_data["name"], color: user_data["color"]}

      :error ->
        %{}
    end
  end
end
