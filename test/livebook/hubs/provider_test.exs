defmodule Livebook.Hubs.ProviderTest do
  use ExUnit.Case

  import Livebook.Fixtures

  alias Livebook.Hubs.{Fly, Metadata, Provider}

  describe "Fly" do
    test "normalize/1" do
      fly = fly_fixture()

      assert Provider.normalize(fly) == %Metadata{
               id: fly.id,
               name: fly.hub_name,
               color: fly.hub_color,
               provider: fly
             }
    end

    test "load/2" do
      fly = fly_fixture()
      fields = Map.from_struct(fly)

      assert Provider.load(%Fly{}, fields) == fly
    end

    test "type/1" do
      assert Provider.type(%Fly{}) == "fly"
    end
  end
end
