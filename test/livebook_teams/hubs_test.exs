defmodule Livebook.HubsTest do
  use Livebook.TeamsIntegrationCase, async: true

  alias Livebook.Hubs

  test "get_hubs/0 returns a list of persisted hubs", %{user: user, node: node} do
    team = create_team_hub(user, node)
    assert team in Hubs.get_hubs()

    Hubs.delete_hub(team.id)
    refute team in Hubs.get_hubs()
  end

  test "get_metadata/0 returns a list of persisted hubs normalized", %{user: user, node: node} do
    team = create_team_hub(user, node)
    metadata = Hubs.Provider.to_metadata(team)

    assert metadata in Hubs.get_metadata()

    Hubs.delete_hub(team.id)
    refute metadata in Hubs.get_metadata()
  end

  test "fetch_hub!/1 returns one persisted team", %{user: user, node: node} do
    assert_raise Livebook.Storage.NotFoundError,
                 ~s/could not find entry in "hubs" with ID "nonexistent"/,
                 fn ->
                   Hubs.fetch_hub!("nonexistent")
                 end

    team = create_team_hub(user, node)

    assert Hubs.fetch_hub!(team.id) == team
  end

  test "hub_exists?/1", %{user: user, node: node} do
    team = build_team_hub(user, node)
    refute Hubs.hub_exists?(team.id)
    Hubs.save_hub(team)
    assert Hubs.hub_exists?(team.id)
  end

  test "save_hub/1 persists hub", %{user: user, node: node} do
    team = build_team_hub(user, node)
    Hubs.save_hub(team)

    assert Hubs.fetch_hub!(team.id) == team
  end

  test "save_hub/1 updates hub", %{user: user, node: node} do
    team = create_team_hub(user, node)
    Hubs.save_hub(%{team | hub_emoji: "🐈"})

    assert Hubs.fetch_hub!(team.id).hub_emoji == "🐈"
  end
end
