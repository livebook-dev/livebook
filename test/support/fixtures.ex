defmodule Livebook.Fixtures do
  @moduledoc false

  def create_fly(id, attrs \\ %{}) do
    attrs
    |> fly_fixture()
    |> Map.replace!(:id, id)
    |> Livebook.Hubs.save_hub()
  end

  def fly_fixture(attrs \\ %{}) do
    fly = %Livebook.Hubs.Fly{
      id: "fly-foo-bar-baz",
      hub_name: "My Personal Hub",
      hub_color: "#FF00FF",
      access_token: Livebook.Utils.random_cookie(),
      organization_id: Livebook.Utils.random_id(),
      organization_type: "PERSONAL",
      organization_name: "Foo",
      application_id: "foo-bar-baz"
    }

    for {key, value} <- attrs, reduce: fly do
      acc -> Map.replace!(acc, key, value)
    end
  end
end
