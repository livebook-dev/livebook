defmodule Livebook.HubsTest do
  use Livebook.TeamsIntegrationCase, async: true

  alias Livebook.Hubs

  setup do
    Livebook.Hubs.Broadcasts.subscribe([:connection, :file_systems, :secrets])
    Livebook.Teams.Broadcasts.subscribe([:clients])

    :ok
  end

  test "get_hubs/0 returns a list of persisted hubs", %{user: user, node: node} do
    team = connect_to_teams(user, node)
    assert team in Hubs.get_hubs()

    Hubs.delete_hub(team.id)
    refute team in Hubs.get_hubs()
  end

  test "get_metadata/0 returns a list of persisted hubs normalized", %{user: user, node: node} do
    team = connect_to_teams(user, node)
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

    team = connect_to_teams(user, node)

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
      team = connect_to_teams(user, node)
      Hubs.save_hub(%{team | hub_emoji: "🐈"})

      assert Hubs.fetch_hub!(team.id).hub_emoji == "🐈"
    end
  end

  describe "create_secret/2" do
    test "creates a new secret", %{user: user, node: node} do
      hub = connect_to_teams(user, node)
      name = secret_name(hub)
      value = hub.id
      secret = build(:secret, name: name, value: value, hub_id: hub.id)

      assert :ok = Hubs.create_secret(hub, secret)
      assert secret in Hubs.get_secrets(hub)

      # Guarantee uniqueness
      assert {:error, errors} = Hubs.create_secret(hub, secret)
      assert "has already been taken" in errors[:name]
    end

    test "returns changeset errors when data is invalid", %{user: user, node: node} do
      hub = connect_to_teams(user, node)
      secret = build(:secret, name: "LB_FOO", value: "BAR", hub_id: hub.id)

      assert {:error, errors} = Hubs.create_secret(hub, secret)
      assert "cannot start with the LB_ prefix" in errors[:name]
    end
  end

  describe "update_secret/2" do
    test "updates a secret", %{user: user, node: node} do
      hub = connect_to_teams(user, node)
      name = secret_name(hub)
      value = hub.id
      secret = build(:secret, name: name, value: value, hub_id: hub.id)

      assert Hubs.create_secret(hub, secret) == :ok
      assert_receive {:secret_created, %{name: ^name, value: ^value, hub_id: ^value}}
      assert secret in Hubs.get_secrets(hub)

      new_value = "BAZ"
      updated_secret = Map.replace!(secret, :value, new_value)

      assert Hubs.update_secret(hub, updated_secret) == :ok
      assert_receive {:secret_updated, %{name: ^name, value: ^new_value, hub_id: ^value}}
      refute secret in Hubs.get_secrets(hub)
      assert updated_secret in Hubs.get_secrets(hub)
    end

    test "returns changeset errors when data is invalid", %{user: user, node: node} do
      hub = connect_to_teams(user, node)
      name = secret_name(hub)
      value = hub.id
      secret = build(:secret, name: name, value: value, hub_id: hub.id)

      assert Hubs.create_secret(hub, secret) == :ok
      assert_receive {:secret_created, %{name: ^name, value: ^value}}

      updated_secret = Map.replace!(secret, :value, "")

      assert {:error, errors} = Hubs.update_secret(hub, updated_secret)
      assert "can't be blank" in errors[:value]
    end
  end

  test "delete_secret/2 deletes a secret", %{user: user, node: node} do
    hub = connect_to_teams(user, node)
    name = secret_name(hub)
    value = hub.id
    secret = build(:secret, name: name, value: value, hub_id: hub.id)

    assert Hubs.create_secret(hub, secret) == :ok
    assert_receive {:secret_created, %{name: ^name, value: ^value, hub_id: ^value}}
    assert secret in Hubs.get_secrets(hub)

    assert Hubs.delete_secret(hub, secret) == :ok
    assert_receive {:secret_deleted, %{name: ^name, value: ^value, hub_id: ^value}}
    refute secret in Hubs.get_secrets(hub)

    # Guarantee it's been removed and will return HTTP status 404
    assert Hubs.delete_secret(hub, secret) ==
             {:transport_error,
              "Something went wrong, try again later or please file a bug if it persists"}
  end

  describe "create_file_system/2" do
    test "creates a new file system", %{user: user, node: node} do
      hub = connect_to_teams(user, node)

      bucket_url = "https://#{hub.id}.s3.amazonaws.com"
      file_system = build(:fs_s3, bucket_url: bucket_url)
      region = file_system.region

      assert Hubs.create_file_system(hub, file_system) == :ok
      assert_receive {:file_system_created, %{bucket_url: ^bucket_url, region: ^region}}

      # Guarantee uniqueness
      assert {:error, errors} = Hubs.create_file_system(hub, file_system)
      assert "has already been taken" in errors[:bucket_url]
    end

    test "returns changeset errors when data is invalid", %{user: user, node: node} do
      hub = connect_to_teams(user, node)
      file_system = build(:fs_s3, bucket_url: nil)

      assert {:error, errors} = Hubs.create_file_system(hub, file_system)
      assert "can't be blank" in errors[:bucket_url]
    end
  end

  describe "update_file_system/2" do
    test "updates a file system", %{user: user, node: node} do
      hub = connect_to_teams(user, node)
      bucket_url = "https://#{hub.id}.s3.amazonaws.com"
      file_system = build(:fs_s3, bucket_url: bucket_url)

      assert Hubs.create_file_system(hub, file_system) == :ok

      assert_receive {:file_system_created,
                      %{bucket_url: ^bucket_url, external_id: external_id} = file_system}

      assert file_system in Hubs.get_file_systems(hub)

      updated_file_system = Map.replace!(file_system, :region, "eu-central-1")

      assert Hubs.update_file_system(hub, updated_file_system) == :ok
      assert_receive {:file_system_updated, %{external_id: ^external_id, region: "eu-central-1"}}
      refute file_system in Hubs.get_file_systems(hub)
      assert updated_file_system in Hubs.get_file_systems(hub)
    end

    test "returns changeset errors when data is invalid", %{user: user, node: node} do
      hub = connect_to_teams(user, node)
      teams_file_system = create_teams_file_system(hub, node)

      file_system =
        build(:fs_s3,
          bucket_url: "https://fix_me.s3.amazonaws.com",
          external_id: to_string(teams_file_system.id)
        )

      update_file_system = Map.replace!(file_system, :bucket_url, "")

      assert {:error, errors} = Hubs.update_file_system(hub, update_file_system)
      assert "can't be blank" in errors[:bucket_url]
    end
  end

  test "delete_file_system/2 deletes a file system", %{user: user, node: node} do
    hub = connect_to_teams(user, node)
    bucket_url = "https://#{hub.id}.s3.amazonaws.com"
    file_system = build(:fs_s3, bucket_url: bucket_url)

    assert Hubs.create_file_system(hub, file_system) == :ok

    assert_receive {:file_system_created,
                    %{bucket_url: ^bucket_url, external_id: external_id} = file_system}

    assert file_system in Hubs.get_file_systems(hub)

    assert Hubs.delete_file_system(hub, file_system) == :ok
    assert_receive {:file_system_deleted, %{external_id: ^external_id}}
    refute file_system in Hubs.get_file_systems(hub)

    # Guarantee it's been removed and will return HTTP status 404
    assert Hubs.delete_file_system(hub, file_system) ==
             {:transport_error,
              "Something went wrong, try again later or please file a bug if it persists"}
  end

  test "generates and verifies stamp for a notebook", %{user: user, node: node} do
    team = connect_to_teams(user, node)

    notebook_source = """
    # Team notebook

    # Intro

    ```elixir
    IO.puts("Hello!")
    ```
    """

    metadata = %{"key" => "value"}

    assert {:ok, stamp} = Hubs.Provider.notebook_stamp(team, notebook_source, metadata)
    assert {:ok, ^metadata} = Hubs.Provider.verify_notebook_stamp(team, notebook_source, stamp)

    assert {:error, :invalid} =
             Hubs.Provider.verify_notebook_stamp(team, notebook_source <> "change\n", stamp)
  end

  defp secret_name(%{id: id}) do
    id
    |> String.replace("-", "_")
    |> String.upcase()
  end
end
