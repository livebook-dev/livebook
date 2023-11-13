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

  describe "save_hub/1" do
    test "persists hub", %{user: user, node: node} do
      team = build_team_hub(user, node)
      Hubs.save_hub(team)

      assert Hubs.fetch_hub!(team.id) == team
    end

    test "updates hub", %{user: user, node: node} do
      team = create_team_hub(user, node)
      Hubs.save_hub(%{team | hub_emoji: "🐈"})

      assert Hubs.fetch_hub!(team.id).hub_emoji == "🐈"
    end
  end

  describe "create_secret/2" do
    test "creates a new secret", %{user: user, node: node} do
      hub = create_team_hub(user, node)
      secret = build(:secret, name: "FOO", value: "BAR")

      assert Hubs.create_secret(hub, secret) == :ok

      # Guarantee uniqueness
      assert {:error, changeset} = Hubs.create_secret(hub, secret)
      assert "has already been taken" in errors_on(changeset).name
    end

    test "returns changeset errors when data is invalid", %{user: user, node: node} do
      hub = create_team_hub(user, node)
      secret = build(:secret, name: "LB_FOO", value: "BAR")

      assert {:error, changeset} = Hubs.create_secret(hub, secret)
      assert "cannot start with the LB_ prefix" in errors_on(changeset).name
    end
  end

  describe "update_secret/2" do
    test "updates a secret", %{user: user, node: node} do
      hub = create_team_hub(user, node)
      secret = build(:secret, name: "UPDATE_ME", value: "BAR")

      assert Hubs.create_secret(hub, secret) == :ok

      update_secret = Map.replace!(secret, :value, "BAZ")
      assert Hubs.update_secret(hub, update_secret) == :ok
    end

    test "returns changeset errors when data is invalid", %{user: user, node: node} do
      hub = create_team_hub(user, node)
      secret = build(:secret, name: "FIX_ME", value: "BAR")

      assert Hubs.create_secret(hub, secret) == :ok

      update_secret = Map.replace!(secret, :value, "")

      assert {:error, changeset} = Hubs.update_secret(hub, update_secret)
      assert "can't be blank" in errors_on(changeset).value
    end
  end

  describe "delete_secret/2" do
    test "deletes a secret", %{user: user, node: node} do
      hub = create_team_hub(user, node)
      secret = build(:secret, name: "DELETE_ME", value: "BAR")

      assert Hubs.create_secret(hub, secret) == :ok
      assert Hubs.delete_secret(hub, secret) == :ok

      # Guarantee it's been removed and will return HTTP status 404
      assert Hubs.delete_secret(hub, secret) ==
               {:transport_error,
                "Something went wrong, try again later or please file a bug if it persists"}
    end

    test "returns transport errors when secret doesn't exists", %{user: user, node: node} do
      hub = create_team_hub(user, node)
      secret = build(:secret, name: "I_CANT_EXIST", value: "BAR")

      # Guarantee it doesn't exists and will return HTTP status 404
      assert Hubs.delete_secret(hub, secret) ==
               {:transport_error,
                "Something went wrong, try again later or please file a bug if it persists"}
    end
  end

  describe "create_file_system/2" do
    test "creates a new file system", %{user: user, node: node} do
      hub = create_team_hub(user, node)
      file_system = build(:fs_s3, bucket_url: "https://file_system_created.s3.amazonaws.com")

      assert Hubs.create_file_system(hub, file_system) == :ok

      # Guarantee uniqueness
      assert {:error, changeset} = Hubs.create_file_system(hub, file_system)
      assert "has already been taken" in errors_on(changeset).bucket_url
    end

    test "returns changeset errors when data is invalid", %{user: user, node: node} do
      hub = create_team_hub(user, node)
      file_system = build(:fs_s3, bucket_url: nil)

      assert {:error, changeset} = Hubs.create_file_system(hub, file_system)
      assert "can't be blank" in errors_on(changeset).bucket_url
    end
  end

  describe "update_file_system/2" do
    test "updates a file system", %{user: user, node: node} do
      hub = create_team_hub(user, node)
      teams_file_system = create_teams_file_system(hub, node)

      file_system =
        build(:fs_s3,
          bucket_url: teams_file_system.name,
          region: "us-east-1",
          external_id: to_string(teams_file_system.id)
        )

      update_file_system = Map.replace!(file_system, :region, "eu-central-1")
      assert Hubs.update_file_system(hub, update_file_system) == :ok
    end

    test "returns changeset errors when data is invalid", %{user: user, node: node} do
      hub = create_team_hub(user, node)
      teams_file_system = create_teams_file_system(hub, node)

      file_system =
        build(:fs_s3,
          bucket_url: "https://fix_me.s3.amazonaws.com",
          external_id: to_string(teams_file_system.id)
        )

      update_file_system = Map.replace!(file_system, :bucket_url, "")

      assert {:error, changeset} = Hubs.update_file_system(hub, update_file_system)
      assert "can't be blank" in errors_on(changeset).bucket_url
    end
  end

  describe "delete_file_system/2" do
    test "deletes a file system", %{user: user, node: node} do
      hub = create_team_hub(user, node)
      teams_file_system = create_teams_file_system(hub, node)

      file_system =
        build(:fs_s3,
          bucket_url: teams_file_system.name,
          region: "us-east-1",
          external_id: to_string(teams_file_system.id)
        )

      assert Hubs.delete_file_system(hub, file_system) == :ok

      # Guarantee it's been removed and will return HTTP status 404
      assert Hubs.delete_file_system(hub, file_system) ==
               {:transport_error,
                "Something went wrong, try again later or please file a bug if it persists"}
    end

    test "returns transport errors when file system doesn't exists", %{user: user, node: node} do
      hub = create_team_hub(user, node)

      file_system =
        build(:fs_s3,
          bucket_url: "https://i_cant_exist.s3.amazonaws.com",
          external_id: "123456789"
        )

      # Guarantee it doesn't exists and will return HTTP status 404
      assert Hubs.delete_file_system(hub, file_system) ==
               {:transport_error,
                "Something went wrong, try again later or please file a bug if it persists"}
    end
  end
end
