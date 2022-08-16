defmodule Livebook.HubsTest do
  use ExUnit.Case

  import Livebook.Fixtures

  alias Livebook.Hubs
  alias Livebook.Hubs.Hub

  setup do
    on_exit(&Hubs.clean_hubs/0)

    :ok
  end

  test "fetch_hubs/0 returns a list of persisted hubs" do
    id = Livebook.Utils.random_id()
    fly_id = "fly-#{id}"
    fly = create_fly(id)

    assert Hubs.fetch_hubs() == [
             %Hub{
               id: fly.id,
               type: "fly",
               name: fly.name,
               color: fly.color,
               provider: fly.organization
             }
           ]

    Livebook.Hubs.delete_entry(fly_id)
    assert Hubs.fetch_hubs() == []
  end

  test "fetch_fly!/1 returns one persisted fly" do
    id = Livebook.Utils.random_id()
    fly_id = "fly-#{id}"

    assert_raise Hubs.NotFoundError,
                 ~s/could not find a fly entry matching "#{id}"/,
                 fn ->
                   Hubs.fetch_fly!(fly_id)
                 end

    fly = create_fly(id)

    assert Hubs.fetch_fly!(fly_id) == fly
  end

  test "provider_by_id!/1 returns one persisted provider" do
    id = Livebook.Utils.random_id()
    fly_id = "fly-#{id}"

    assert_raise Hubs.NotFoundError,
                 ~s/could not find a fly entry matching "#{id}"/,
                 fn ->
                   Hubs.provider_by_id!(fly_id)
                 end

    create_fly(id)

    assert Hubs.provider_by_id!(fly_id) == "fly"
  end

  test "fly_exists?/1" do
    id = Livebook.Utils.random_id()
    fly = fly_fixture(%{id: id, organization: %{id: id}})
    refute Hubs.fly_exists?(fly.organization)

    Hubs.save_fly(fly)
    assert Hubs.fly_exists?(fly.organization)
  end
end
