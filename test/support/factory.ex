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
    %Livebook.Hubs.Metadata{
      id: "fly-foo-bar-baz",
      name: "My Personal Hub",
      color: "#FF00FF",
      provider: build(:fly)
    }
  end

  def build(:fly) do
    %Livebook.Hubs.Fly{
      id: "fly-foo-bar-baz",
      hub_name: "My Personal Hub",
      hub_color: "#FF00FF",
      access_token: Livebook.Utils.random_cookie(),
      organization_id: Livebook.Utils.random_id(),
      organization_type: "PERSONAL",
      organization_name: "Foo",
      application_id: "foo-bar-baz"
    }
  end

  def build(:env_var) do
    %Livebook.Settings.EnvVar{
      name: "BAR",
      value: "foo"
    }
  end

  def build(factory_name, attrs \\ %{}) do
    factory_name |> build() |> struct!(attrs)
  end

  def params_for(factory_name, attrs \\ %{}) do
    factory_name |> build() |> struct!(attrs) |> Map.from_struct()
  end

  def insert_hub(factory_name, attrs \\ %{}) do
    factory_name
    |> build(attrs)
    |> Livebook.Hubs.save_hub()
  end

  def insert_env_var(factory_name, attrs \\ %{}) do
    env_var = build(factory_name, attrs)
    attributes = env_var |> Map.from_struct() |> Map.to_list()
    Livebook.Storage.current().insert(:env_vars, env_var.name, attributes)

    env_var
  end
end
