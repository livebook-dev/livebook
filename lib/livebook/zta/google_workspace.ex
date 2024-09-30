defmodule Livebook.ZTA.GoogleWorkspace do
  use GenServer
  use LivebookWeb, :verified_routes

  require Logger

  import Plug.Conn
  import Phoenix.Controller

  @behaviour Livebook.ZTA

  @base_url "https://www.googleapis.com"
  @userinfo_path "/oauth2/v3/userinfo"
  @provider "google_workspace"
  @access_token_key "goog-jwt-access-token"

  defstruct [:name, :req, :org_name, :hub]

  def start_link(opts) do
    name = Keyword.fetch!(opts, :name)
    GenServer.start_link(__MODULE__, opts, name: name)
  end

  @impl true
  def authenticate(name, %{params: %{@access_token_key => token}} = conn, _opts) do
    {hub, req} = Livebook.ZTA.get(name)
    url = zta_url(hub)

    case validate_access_token(req, token) do
      {:ok, metadata} ->
        conn = put_session(conn, :identity_data, metadata)
        {redirect(conn, to: ~p"/"), metadata}

      :error ->
        {conn
         |> redirect(external: url)
         |> halt(), nil}
    end
  end

  def authenticate(name, conn, _opts) do
    {hub, req} = Livebook.ZTA.get(name)
    url = zta_url(hub)

    with %{"identity_data" => identity_data} <- get_session(conn),
         {:ok, metadata} <- validate_access_token(req, identity_data.payload["token"]) do
      {conn, metadata}
    else
      _ ->
        {conn
         |> redirect(external: url)
         |> halt(), nil}
    end
  end

  @impl true
  def init(opts) do
    name = Keyword.fetch!(opts, :name)
    org_name = Keyword.fetch!(opts, :identity_key)
    req = opts[:req] || Req.new(base_url: @base_url)
    hub = opts[:hub] || Livebook.Hubs.fetch_hub!("team-#{org_name}")

    Livebook.ZTA.put(name, {hub, req})
    state = %__MODULE__{name: name, org_name: org_name, hub: hub, req: req}

    {:ok, state}
  end

  @impl true
  def handle_info(_, state) do
    {:noreply, state}
  end

  defp zta_url(hub) do
    deployment_group_id = Livebook.Hubs.TeamClient.get_deployment_group_id(hub.id)
    false = is_nil(deployment_group_id)
    "team-" <> org_name = hub.id

    query = %{
      redirect_uri: LivebookWeb.Endpoint.url(),
      deployment_group_id: deployment_group_id,
      org_name: org_name
    }

    query_string = URI.encode_query(query)

    URI.parse(Livebook.Config.teams_url())
    |> URI.append_path("/zta/#{@provider}")
    |> URI.append_query(query_string)
    |> URI.to_string()
  end

  defp validate_access_token(_req, nil) do
    :error
  end

  defp validate_access_token(req, token) do
    Logger.debug("[#{inspect(__MODULE__)}] requesting google api")

    case Req.get(req, url: @userinfo_path, params: %{access_token: token}) do
      {:ok, %{body: %{"sub" => id, "name" => name, "email" => email}, status: 200}} ->
        metadata = %{id: id, name: name, email: email, payload: %{"token" => token}}
        Logger.debug("[#{inspect(__MODULE__)}] received from google api: #{inspect(metadata)}")

        {:ok, metadata}

      {:ok, %{body: %{"error" => %{"message" => message}}, status: _}} ->
        Logger.debug("[#{inspect(__MODULE__)}] received error from google api: #{message}")
        :error

      {:error, reason} ->
        Logger.debug("[#{inspect(__MODULE__)}] failed to validate token: #{inspect(reason)}")
        :error
    end
  end
end
