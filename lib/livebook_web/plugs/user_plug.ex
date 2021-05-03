defmodule LivebookWeb.UserPlug do
  @moduledoc false

  # Initializes the session and cookies with user-related info.
  #
  # The first time someone visits Livebook
  # this plug stores a new random user id
  # in the session under `:current_user_id`.
  #
  # Additionally the cookies are checked for the presence
  # of `"user_data"` and if there is none, a new user
  # attributes are stored there. This makes sure
  # the client-side can always access some `"user_data"`
  # for `connect_params` of the socket connection.

  @behaviour Plug

  import Plug.Conn

  alias Livebook.Users.User

  @impl true
  def init(opts), do: opts

  @impl true
  def call(conn, _opts) do
    conn
    |> ensure_current_user_id()
    |> ensure_user_data()
  end

  defp ensure_current_user_id(conn) do
    if get_session(conn, :current_user_id) do
      conn
    else
      user_id = Livebook.Utils.random_id()
      put_session(conn, :current_user_id, user_id)
    end
  end

  defp ensure_user_data(conn) do
    if Map.has_key?(conn.req_cookies, "user_data") do
      conn
    else
      user_data = user_data(User.new())
      encoded = user_data |> Jason.encode!() |> Base.encode64()
      # Set `http_only` to `false`, so that it can be accessed on the client
      # Set expiration in 5 years
      put_resp_cookie(conn, "user_data", encoded, http_only: false, max_age: 157_680_000)
    end
  end

  defp user_data(user) do
    user
    |> Map.from_struct()
    |> Map.delete(:id)
  end
end
