defmodule Livebook.Hubs.HubProviderTest do
  use ExUnit.Case

  import Livebook.Fixtures

  alias Livebook.Hubs.{Hub, HubProvider}

  test "Fly to_hub/1" do
    fly = fly_fixture()

    assert HubProvider.to_hub(fly) == %Hub{
             id: fly.id,
             type: "fly",
             name: fly.name,
             color: fly.color,
             provider: fly.organization
           }
  end
end
