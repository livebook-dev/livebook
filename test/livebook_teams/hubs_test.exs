defmodule Livebook.HubsTest do
  use Livebook.TeamsIntegrationCase, async: true

  alias Livebook.Hubs
  @sample_hub "team-foo-bar-baz"

  setup do
    on_exit(fn -> Hubs.delete_hub(@sample_hub) end)
    :ok
  end

  test "get_hubs/0 returns a list of persisted hubs" do
    team = insert_hub(:team, id: @sample_hub)
    assert team in Hubs.get_hubs()

    Hubs.delete_hub(@sample_hub)
    refute team in Hubs.get_hubs()
  end

  test "get_metadata/0 returns a list of persisted hubs normalized" do
    team = insert_hub(:team, id: @sample_hub)
    metadata = Hubs.Provider.to_metadata(team)

    assert metadata in Hubs.get_metadatas()

    Hubs.delete_hub(@sample_hub)
    refute metadata in Hubs.get_metadatas()
  end

  test "fetch_hub!/1 returns one persisted team" do
    assert_raise Livebook.Storage.NotFoundError,
                 ~s/could not find entry in "hubs" with ID "#{@sample_hub}"/,
                 fn ->
                   Hubs.fetch_hub!(@sample_hub)
                 end

    team = insert_hub(:team, id: @sample_hub)

    assert Hubs.fetch_hub!(@sample_hub) == team
  end

  test "hub_exists?/1" do
    refute Hubs.hub_exists?(@sample_hub)
    insert_hub(:team, id: @sample_hub)
    assert Hubs.hub_exists?(@sample_hub)
  end

  test "save_hub/1 persists hub" do
    team = build(:team, id: @sample_hub)
    Hubs.save_hub(team)

    assert Hubs.fetch_hub!(@sample_hub) == team
  end

  test "save_hub/1 updates hub" do
    team = insert_hub(:team, id: @sample_hub)
    Hubs.save_hub(%{team | hub_emoji: "🐈"})

    refute Hubs.fetch_hub!(@sample_hub) == team
    assert Hubs.fetch_hub!(@sample_hub).hub_emoji == "🐈"
  end
end
