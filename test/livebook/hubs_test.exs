defmodule Livebook.HubsTest do
  use ExUnit.Case

  import Livebook.Fixtures

  alias Livebook.Hubs

  setup do
    on_exit(&Hubs.clean_hubs/0)

    :ok
  end

  test "fetch_hubs/0 returns a list of persisted hubs" do
    fly = create_fly("fly-baz")
    assert Hubs.fetch_hubs() == [fly]

    Hubs.delete_hub("fly-baz")
    assert Hubs.fetch_hubs() == []
  end

  test "fetch_metadata/0 returns a list of persisted hubs normalized" do
    fly = create_fly("fly-livebook")

    assert Hubs.fetch_metadatas() == [
             %Hubs.Metadata{
               id: "fly-livebook",
               color: fly.hub_color,
               name: fly.hub_name,
               provider: fly
             }
           ]

    Hubs.delete_hub("fly-livebook")
    assert Hubs.fetch_metadatas() == []
  end

  test "fetch_hub!/1 returns one persisted fly" do
    assert_raise Hubs.NotFoundError,
                 ~s/could not find a hub matching "fly-foo"/,
                 fn ->
                   Hubs.fetch_hub!("fly-foo")
                 end

    fly = create_fly("fly-foo")

    assert Hubs.fetch_hub!("fly-foo") == fly
  end

  test "hub_exists?/1" do
    refute Hubs.hub_exists?("fly-bar")
    create_fly("fly-bar")
    assert Hubs.hub_exists?("fly-bar")
  end

  test "save_hub/1 persists hub" do
    fly = fly_fixture(id: "fly-foo")
    Hubs.save_hub(fly)

    assert Hubs.fetch_hub!("fly-foo") == fly
  end

  test "save_hub/1 updates hub" do
    fly = create_fly("fly-foo2")
    Hubs.save_hub(%{fly | hub_color: "#FFFFFF"})

    refute Hubs.fetch_hub!("fly-foo2") == fly
    assert Hubs.fetch_hub!("fly-foo2").hub_color == "#FFFFFF"
  end
end
