defmodule Livebook.ZTA.BasicAuth do
  def child_spec(opts) do
    %{id: __MODULE__, start: {__MODULE__, :start_link, [opts]}}
  end

  def start_link(options) do
    name = Keyword.fetch!(options, :name)
    identity_key = Keyword.fetch!(options, :identity_key)
    [username, password] = String.split(identity_key, ":", parts: 2)

    Livebook.ZTA.put(name, {username, password})
    :ignore
  end

  def authenticate(name, conn, _options) do
    user_credentials = Plug.BasicAuth.parse_basic_auth(conn)
    app_credentials = Livebook.ZTA.get(name)

    {conn, authenticate_user(user_credentials, app_credentials)}
  end

  defp authenticate_user({username, password}, {app_username, app_password}) do
    if Plug.Crypto.secure_compare(username, app_username) and
         Plug.Crypto.secure_compare(password, app_password) do
      %{payload: %{}}
    end
  end

  defp authenticate_user(_, _), do: nil
end
