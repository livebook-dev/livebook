defmodule Livebook.Factory do
  use ExMachina

  def user_factory do
    %Livebook.Users.User{
      id: Livebook.Utils.random_id(),
      name: "Jose Valim",
      hex_color: Livebook.Users.User.random_hex_color()
    }
  end

  def fly_metadata_factory do
    %Livebook.Hubs.Metadata{
      id: "fly-foo-bar-baz",
      name: "My Personal Hub",
      color: "#FF00FF",
      provider: build(:fly)
    }
  end

  def fly_factory do
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

  def insert_hub(factory_name, attrs \\ %{}) do
    factory_name
    |> build(attrs)
    |> Livebook.Hubs.save_hub()
  end
end
