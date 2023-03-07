defmodule Livebook.Factory do
  @moduledoc false

  def build(:user) do
    %Livebook.Users.User{
      id: Livebook.Utils.random_id(),
      name: "Jose Valim",
      hex_color: Livebook.EctoTypes.HexColor.random()
    }
  end

  def build(:fly_metadata) do
    :fly |> build() |> Livebook.Hubs.Provider.to_metadata()
  end

  def build(:fly) do
    %Livebook.Hubs.Fly{
      id: "fly-foo-bar-baz",
      hub_name: "My Personal Hub",
      hub_emoji: "🚀",
      access_token: Livebook.Utils.random_cookie(),
      organization_id: Livebook.Utils.random_id(),
      organization_type: "PERSONAL",
      organization_name: "Foo",
      application_id: "foo-bar-baz"
    }
  end

  def build(:enterprise_metadata) do
    :enterprise |> build() |> Livebook.Hubs.Provider.to_metadata()
  end

  def build(:enterprise) do
    id = Livebook.Utils.random_id()

    %Livebook.Hubs.Enterprise{
      id: "enterprise-#{id}",
      hub_name: "Enterprise",
      hub_emoji: "🏭",
      external_id: id,
      token: Livebook.Utils.random_cookie(),
      url: "http://localhost"
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
      hub_id: Livebook.Hubs.Personal.id(),
      readonly: false
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
