defmodule Livebook.HubsTest do
  use ExUnit.Case

  alias Livebook.Hubs
  alias Livebook.Hubs.Hub

  @hub_id Livebook.Utils.random_id()

  setup do
    on_exit(fn ->
      for %{id: hub_id} <- Hubs.fetch_hubs() do
        Livebook.Storage.current().delete(:hub, hub_id)
      end
    end)

    :ok
  end

  test "fetch_hubs/0 returns a list of persisted hubs" do
    Hubs.save_hub(hub())

    assert [
             %{
               color: "#FF00FF",
               id: @hub_id,
               name: "My Foo",
               label: "Foo org",
               token: "foo",
               type: "fly"
             }
           ] == Hubs.fetch_hubs()

    Livebook.Storage.current().delete(:hub, @hub_id)
    assert [] == Hubs.fetch_hubs()
  end

  test "hub_by_id!/1 returns one persisted hub" do
    assert_raise Hubs.NotFoundError,
                 ~s/could not find a hub matching "#{@hub_id}"/,
                 fn ->
                   Hubs.hub_by_id!(@hub_id)
                 end

    Hubs.save_hub(hub())

    assert hub() == Hubs.hub_by_id!(@hub_id)
  end

  test "hub_exists?/1" do
    refute Hubs.hub_exists?(hub())

    Hubs.save_hub(hub())
    assert Hubs.hub_exists?(hub())
  end

  defp hub do
    %Hub{
      id: @hub_id,
      type: "fly",
      name: "My Foo",
      label: "Foo org",
      token: "foo",
      color: "#FF00FF"
    }
  end
end
