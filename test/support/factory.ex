defmodule Livebook.Factory do
  def build(:user) do
    %Livebook.Users.User{
      id: Livebook.Utils.random_long_id(),
      name: "Jose Valim",
      hex_color: Livebook.EctoTypes.HexColor.random()
    }
  end

  def build(:team_metadata) do
    :team |> build() |> Livebook.Hubs.Provider.to_metadata()
  end

  def build(:team) do
    org = build(:org)

    %Livebook.Hubs.Team{
      id: "team-#{org.name}",
      hub_name: org.name,
      hub_emoji: "🏭",
      org_id: 1,
      user_id: 1,
      org_key_id: 1,
      org_public_key: Livebook.Hubs.Team.public_key_prefix() <> Livebook.Utils.random_long_id(),
      teams_key: org.teams_key,
      session_token: Livebook.Utils.random_short_id(),
      offline: nil
    }
  end

  def build(:personal_metadata) do
    :personal |> build() |> Livebook.Hubs.Provider.to_metadata()
  end

  def build(:personal) do
    %Livebook.Hubs.Personal{
      id: Livebook.Hubs.Personal.id(),
      hub_name: "My Hub",
      hub_emoji: "🏠"
    }
  end

  def build(:env_var) do
    %Livebook.Settings.EnvVar{
      name: unique_value("BAR_"),
      value: Livebook.Utils.random_short_id()
    }
  end

  def build(:secret) do
    %Livebook.Secrets.Secret{
      name: unique_value("FOO_"),
      value: Livebook.Utils.random_short_id(),
      hub_id: Livebook.Hubs.Personal.id(),
      deployment_group_id: nil
    }
  end

  def build(:deployment_group) do
    %Livebook.Teams.DeploymentGroup{
      name: unique_value("FOO_"),
      mode: :offline,
      agent_keys: [],
      secrets: [],
      environment_variables: []
    }
  end

  def build(:org) do
    %Livebook.Teams.Org{
      id: nil,
      emoji: "🏭",
      name: "org-name-#{System.unique_integer([:positive])}",
      teams_key: Livebook.Teams.Org.teams_key(),
      user_code: nil
    }
  end

  def build(:fs_s3) do
    bucket_url = "https://#{unique_value("mybucket-")}.s3.amazonaws.com"
    hash = :crypto.hash(:sha256, bucket_url)
    hub_id = Livebook.Hubs.Personal.id()

    %Livebook.FileSystem.S3{
      id: "#{hub_id}-s3-#{Base.url_encode64(hash, padding: false)}",
      bucket_url: bucket_url,
      external_id: nil,
      region: "us-east-1",
      access_key_id: "key",
      secret_access_key: "secret",
      hub_id: hub_id
    }
  end

  def build(:agent_key) do
    %Livebook.Teams.AgentKey{
      id: "1",
      key: "lb_ak_zj9tWM1rEVeweYR7DbH_2VK5_aKtWfptcL07dBncqg",
      deployment_group_id: "1"
    }
  end

  def build(:app_deployment) do
    slug = Livebook.Utils.random_short_id()
    content = :crypto.strong_rand_bytes(1024 * 1024)
    md5_hash = :crypto.hash(:md5, content)
    shasum = Base.encode16(md5_hash, case: :lower)

    deployed_at =
      DateTime.utc_now()
      |> DateTime.truncate(:second)

    {seconds, 0} = DateTime.to_gregorian_seconds(deployed_at)

    %Livebook.Teams.AppDeployment{
      id: "1",
      title: unique_value("MyNotebook-"),
      sha: shasum,
      version: "1-#{shasum}-#{seconds}",
      slug: slug,
      file: content,
      multi_session: false,
      access_type: :protected,
      hub_id: Livebook.Hubs.Personal.id(),
      deployment_group_id: "1",
      deployed_by: "Ada Lovelace",
      deployed_at: deployed_at
    }
  end

  def build(:agent) do
    %Livebook.Teams.Agent{
      id: "agent_name-#{Livebook.Utils.random_short_id()}",
      name: unique_value("agent_name"),
      hub_id: Livebook.Hubs.Personal.id(),
      org_id: "1",
      deployment_group_id: "1"
    }
  end

  def build(factory_name, attrs) do
    factory_name |> build() |> struct!(attrs)
  end

  def params_for(factory_name, attrs \\ []) do
    factory_name |> build() |> struct!(attrs) |> Map.from_struct()
  end

  def insert_secret(attrs \\ %{}) do
    secret = build(:secret, attrs)
    hub = Livebook.Hubs.fetch_hub!(secret.hub_id)
    :ok = Livebook.Hubs.create_secret(hub, secret)
    secret
  end

  def insert_deployment_group(attrs \\ %{}) do
    attrs = params_for(:deployment_group, attrs)
    hub = Livebook.Hubs.fetch_hub!(attrs.hub_id)
    {:ok, deployment_group} = Livebook.Teams.create_deployment_group(hub, attrs)
    deployment_group
  end

  def insert_env_var(factory_name, attrs \\ %{}) do
    env_var = build(factory_name, attrs)
    attributes = env_var |> Map.from_struct() |> Map.to_list()
    Livebook.Storage.insert(:env_vars, env_var.name, attributes)

    env_var
  end

  # Creates an online hub with offline connection, which is safe to
  # used in tests without hubs server.
  def insert_fake_online_hub() do
    hub = Livebook.Factory.build(:team)

    # Save the hub and start the TeamClient as an offline hub
    hub
    |> Map.put(:offline, %Livebook.Hubs.Team.Offline{})
    |> Livebook.Hubs.save_hub()

    # Save the hub as an online hub (with the offline TeamClient
    # already running)
    Livebook.Hubs.save_hub(hub)
  end

  def unique_value(prefix \\ nil) do
    value = unique_integer()
    if prefix, do: "#{prefix}#{value}", else: value
  end

  defp unique_integer(), do: System.unique_integer([:positive])
end
