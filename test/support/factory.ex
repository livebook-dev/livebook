defmodule Livebook.Factory do
  def build(:user) do
    %Livebook.Users.User{
      id: Livebook.Utils.random_id(),
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
      org_public_key: Livebook.Hubs.Team.public_key_prefix() <> Livebook.Utils.random_id(),
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
      name: "BAR",
      value: "foo"
    }
  end

  def build(:secret) do
    %Livebook.Secrets.Secret{
      name: "FOO",
      value: "123",
      hub_id: Livebook.Hubs.Personal.id()
    }
  end

  def build(:deployment_group) do
    %Livebook.Teams.DeploymentGroup{
      name: "FOO",
      mode: "offline"
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
    bucket_url = "https://mybucket.s3.amazonaws.com"
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

  def build(factory_name, attrs) do
    factory_name |> build() |> struct!(attrs)
  end

  def params_for(factory_name, attrs) do
    factory_name |> build() |> struct!(attrs) |> Map.from_struct()
  end

  def insert_secret(attrs \\ %{}) do
    secret = build(:secret, attrs)
    hub = Livebook.Hubs.fetch_hub!(secret.hub_id)
    :ok = Livebook.Hubs.create_secret(hub, secret)
    secret
  end

  def insert_deployment_group(attrs \\ %{}) do
    deployment_group = build(:deployment_group, attrs)
    hub = Livebook.Hubs.fetch_hub!(deployment_group.hub_id)
    {:ok, _id} = Livebook.Teams.create_deployment_group(hub, deployment_group)
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
end
