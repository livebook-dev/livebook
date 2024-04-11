defmodule Livebook.ZTA.BasicAuth do
  use GenServer
  require Logger

  defstruct [:identity_key]

  def start_link(options) do
    name = Keyword.fetch!(options, :name)
    GenServer.start_link(__MODULE__, options, name: name)
  end

  def authenticate(name, conn, _options) do
    user_credentials = Plug.BasicAuth.parse_basic_auth(conn)
    server_credentials = GenServer.call(name, :credentials, :infinity)

    {conn, authenticate_user(user_credentials, server_credentials)}
  end

  @impl true
  def init(options) do
    identity_key = Keyword.fetch!(options, :identity_key)
    {:ok, %__MODULE__{identity_key: identity_key}}
  end

  @impl true
  def handle_call(:credentials, _, state) do
    {:reply, state.identity_key, state}
  end

  defp authenticate_user({username, password}, {username, password}) do
    %{payload: %{}, id: username}
  end

  defp authenticate_user(_, _), do: nil
end
