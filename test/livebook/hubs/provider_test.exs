defmodule Livebook.Hubs.ProviderTest do
  use Livebook.DataCase

  alias Livebook.Hubs.{Fly, Metadata, Provider}

  describe "Fly" do
    test "normalize/1" do
      fly = build(:fly)

      assert Provider.normalize(fly) == %Metadata{
               id: fly.id,
               name: fly.hub_name,
               emoji: fly.hub_emoji,
               provider: fly
             }
    end

    test "load/2" do
      fly = build(:fly)
      fields = Map.from_struct(fly)

      assert Provider.load(%Fly{}, fields) == fly
    end

    test "type/1" do
      assert Provider.type(%Fly{}) == "fly"
    end
  end
end
