defmodule Livebook.Factory do
  @moduledoc false

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

  def build(:org) do
    %Livebook.Teams.Org{
      id: nil,
      emoji: "🏭",
      name: "org-name-#{System.unique_integer([:positive])}",
      teams_key: Livebook.Teams.Org.teams_key(),
      user_code: nil
    }
  end

  def build(factory_name, attrs) do
    factory_name |> build() |> struct!(attrs)
  end

  def params_for(factory_name, attrs) do
    factory_name |> build() |> struct!(attrs) |> Map.from_struct()
  end

  def insert_hub(factory_name, attrs \\ %{}) do
    factory_name
    |> build(attrs)
    |> Livebook.Hubs.save_hub()
  end

  def insert_secret(attrs \\ %{}) do
    secret = build(:secret, attrs)
    hub = Livebook.Hubs.fetch_hub!(secret.hub_id)
    :ok = Livebook.Hubs.create_secret(hub, secret)
    secret
  end

  def insert_env_var(factory_name, attrs \\ %{}) do
    env_var = build(factory_name, attrs)
    attributes = env_var |> Map.from_struct() |> Map.to_list()
    Livebook.Storage.insert(:env_vars, env_var.name, attributes)

    env_var
  end
end
