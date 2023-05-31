defmodule Livebook.HubsTest do
  use Livebook.TeamsIntegrationCase, async: true

  alias Livebook.Hubs

  test "get_hubs/0 returns a list of persisted hubs" do
    team = insert_hub(:team, id: "team-baz")
    assert team in Hubs.get_hubs()

    Hubs.delete_hub("team-baz")
    refute team in Hubs.get_hubs()
  end

  test "get_metadata/0 returns a list of persisted hubs normalized" do
    team = insert_hub(:team, id: "team-livebook")
    metadata = Hubs.Provider.to_metadata(team)

    assert metadata in Hubs.get_metadatas()

    Hubs.delete_hub("team-livebook")
    refute metadata in Hubs.get_metadatas()
  end

  test "fetch_hub!/1 returns one persisted team" do
    assert_raise Livebook.Storage.NotFoundError,
                 ~s/could not find entry in \"hubs\" with ID "team-exception-foo"/,
                 fn ->
                   Hubs.fetch_hub!("team-exception-foo")
                 end

    team = insert_hub(:team, id: "team-exception-foo")

    assert Hubs.fetch_hub!("team-exception-foo") == team
  end

  test "hub_exists?/1" do
    refute Hubs.hub_exists?("team-bar")
    insert_hub(:team, id: "team-bar")
    assert Hubs.hub_exists?("team-bar")
  end

  test "save_hub/1 persists hub" do
    team = build(:team, id: "team-foo")
    Hubs.save_hub(team)

    assert Hubs.fetch_hub!("team-foo") == team
  end

  test "save_hub/1 updates hub" do
    team = insert_hub(:team, id: "team-foo2")
    Hubs.save_hub(%{team | hub_emoji: "🐈"})

    refute Hubs.fetch_hub!("team-foo2") == team
    assert Hubs.fetch_hub!("team-foo2").hub_emoji == "🐈"
  end
end
