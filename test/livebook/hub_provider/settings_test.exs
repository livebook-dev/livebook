defmodule Livebook.HubProvider.SettingsTest do
  use ExUnit.Case

  alias Livebook.HubProvider.{Hub, Settings}

  @hub_id Livebook.Utils.random_id()

  test "fetch_hubs/0 returns a list of persisted hubs" do
    clean_hubs()

    Settings.save_hub(hub())

    assert [
             %{
               color: "#FF00FF",
               id: @hub_id,
               name: "My Foo",
               label: "Foo org",
               token: "foo",
               type: "fly"
             }
           ] == Settings.fetch_hubs()

    Livebook.Storage.current().delete(:hub, @hub_id)
    assert [] == Settings.fetch_hubs()
  end

  test "hub_by_id!/1 returns one persisted hub" do
    clean_hubs()

    assert_raise Settings.NotFoundError,
                 ~s/could not find a hub matching "#{@hub_id}"/,
                 fn ->
                   Settings.hub_by_id!(@hub_id)
                 end

    Settings.save_hub(hub())

    assert hub() == Settings.hub_by_id!(@hub_id)
  end

  test "hub_exists?/1" do
    clean_hubs()
    refute Settings.hub_exists?(hub())

    Settings.save_hub(hub())
    assert Settings.hub_exists?(hub())
  end

  defp clean_hubs do
    for %{id: hub_id} <- Settings.fetch_hubs() do
      Livebook.Storage.current().delete(:hub, hub_id)
    end
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
