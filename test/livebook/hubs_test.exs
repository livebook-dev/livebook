defmodule Livebook.HubsTest do
  use Livebook.DataCase

  alias Livebook.Hubs

  setup do
    on_exit(&Hubs.clean_hubs/0)

    :ok
  end

  test "get_hubs/0 returns a list of persisted hubs" do
    fly = insert_hub(:fly, id: "fly-baz")
    assert Hubs.get_hubs() == [fly]

    Hubs.delete_hub("fly-baz")
    assert Hubs.get_hubs() == []
  end

  test "get_metadata/0 returns a list of persisted hubs normalized" do
    fly = insert_hub(:fly, id: "fly-livebook")

    assert Hubs.get_metadatas() == [
             %Hubs.Metadata{
               id: "fly-livebook",
               color: fly.hub_color,
               name: fly.hub_name,
               provider: fly
             }
           ]

    Hubs.delete_hub("fly-livebook")
    assert Hubs.get_metadatas() == []
  end

  test "get_hub!/1 returns one persisted fly" do
    assert_raise Livebook.Storage.NotFoundError,
                 ~s/could not find entry in \"hubs\" with ID "fly-foo"/,
                 fn ->
                   Hubs.get_hub!("fly-foo")
                 end

    fly = insert_hub(:fly, id: "fly-foo")

    assert Hubs.get_hub!("fly-foo") == fly
  end

  test "hub_exists?/1" do
    refute Hubs.hub_exists?("fly-bar")
    insert_hub(:fly, id: "fly-bar")
    assert Hubs.hub_exists?("fly-bar")
  end

  test "save_hub/1 persists hub" do
    fly = build(:fly, id: "fly-foo")
    Hubs.save_hub(fly)

    assert Hubs.get_hub!("fly-foo") == fly
  end

  test "save_hub/1 updates hub" do
    fly = insert_hub(:fly, id: "fly-foo2")
    Hubs.save_hub(%{fly | hub_color: "#FFFFFF"})

    refute Hubs.get_hub!("fly-foo2") == fly
    assert Hubs.get_hub!("fly-foo2").hub_color == "#FFFFFF"
  end
end
