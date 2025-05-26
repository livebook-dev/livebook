defmodule Livebook.HubsTest do
  use Livebook.TeamsIntegrationCase, async: true
  alias Livebook.Hubs

  @moduletag workspace_for: :user
  setup :workspace

  @moduletag subscribe_to_hubs_topics: [:connection, :file_systems, :secrets]
  @moduletag subscribe_to_teams_topics: [:clients]

  test "get_hubs/0 returns a list of persisted hubs", %{team: team} do
    assert team in Hubs.get_hubs()
    assert Hubs.hub_exists?(team.id)
  end

  test "get_metadata/0 returns a list of persisted hubs normalized", %{team: team} do
    assert %{provider: ^team} = metadata = Hubs.Provider.to_metadata(team)
    assert metadata in Hubs.get_metadata()
  end

  @tag workspace_persisted: false
  test "fetch_hub!/1 returns one persisted team", %{team: team} do
    assert_raise Livebook.Storage.NotFoundError,
                 ~s/could not find entry in "hubs" with ID "#{team.id}"/,
                 fn ->
                   Hubs.fetch_hub!(team.id)
                 end

    Hubs.save_hub(team)
    assert Hubs.fetch_hub!(team.id) == team
  end

  describe "save_hub/1" do
    @tag workspace_persisted: false
    test "persists hub", %{team: team} do
      refute Hubs.hub_exists?(team.id)
      Hubs.save_hub(team)
      assert Hubs.fetch_hub!(team.id) == team
    end

    test "updates hub", %{team: team} do
      Hubs.save_hub(%{team | hub_emoji: "🐈"})
      assert Hubs.fetch_hub!(team.id).hub_emoji == "🐈"
    end
  end

  describe "create_secret/2" do
    test "creates a new secret", %{team: team} do
      name = secret_name(team)
      value = team.id
      secret = build(:secret, name: name, value: value, hub_id: team.id)

      assert :ok = Hubs.create_secret(team, secret)
      assert secret in Hubs.get_secrets(team)

      # Guarantee uniqueness
      assert {:error, errors} = Hubs.create_secret(team, secret)
      assert "has already been taken" in errors[:name]
    end

    test "returns changeset errors when data is invalid", %{team: team} do
      secret = build(:secret, name: "LB_FOO", value: "BAR", hub_id: team.id)

      assert {:error, errors} = Hubs.create_secret(team, secret)
      assert "cannot start with the LB_ prefix" in errors[:name]
    end
  end

  describe "update_secret/2" do
    test "updates a secret", %{team: team} do
      name = secret_name(team)
      value = team.id
      secret = build(:secret, name: name, value: value, hub_id: team.id)

      assert Hubs.create_secret(team, secret) == :ok
      assert_receive {:secret_created, %{name: ^name, value: ^value, hub_id: ^value}}
      assert secret in Hubs.get_secrets(team)

      new_value = "BAZ"
      updated_secret = Map.replace!(secret, :value, new_value)

      assert Hubs.update_secret(team, updated_secret) == :ok
      assert_receive {:secret_updated, %{name: ^name, value: ^new_value, hub_id: ^value}}
      refute secret in Hubs.get_secrets(team)
      assert updated_secret in Hubs.get_secrets(team)
    end

    test "returns changeset errors when data is invalid", %{team: team} do
      name = secret_name(team)
      value = team.id
      secret = build(:secret, name: name, value: value, hub_id: team.id)

      assert Hubs.create_secret(team, secret) == :ok
      assert_receive {:secret_created, %{name: ^name, value: ^value}}

      updated_secret = Map.replace!(secret, :value, "")

      assert {:error, errors} = Hubs.update_secret(team, updated_secret)
      assert "can't be blank" in errors[:value]
    end
  end

  test "delete_secret/2 deletes a secret", %{team: team} do
    name = secret_name(team)
    value = team.id
    secret = build(:secret, name: name, value: value, hub_id: team.id)

    assert Hubs.create_secret(team, secret) == :ok
    assert_receive {:secret_created, %{name: ^name, value: ^value, hub_id: ^value}}
    assert secret in Hubs.get_secrets(team)

    assert Hubs.delete_secret(team, secret) == :ok
    assert_receive {:secret_deleted, %{name: ^name, value: ^value, hub_id: ^value}}
    refute secret in Hubs.get_secrets(team)

    # Guarantee it's been removed and will return HTTP status 404
    assert Hubs.delete_secret(team, secret) ==
             {:transport_error,
              "Something went wrong, try again later or please file a bug if it persists"}
  end

  describe "create_file_system/2" do
    test "creates a new file system", %{team: team} do
      bucket_url = "https://#{team.id}.s3.amazonaws.com"
      file_system = build(:fs_s3, bucket_url: bucket_url)
      region = file_system.region

      assert Hubs.create_file_system(team, file_system) == :ok
      assert_receive {:file_system_created, %{bucket_url: ^bucket_url, region: ^region}}

      # Guarantee uniqueness
      assert {:error, errors} = Hubs.create_file_system(team, file_system)
      assert "has already been taken" in errors[:bucket_url]
    end

    test "returns changeset errors when data is invalid", %{team: team} do
      file_system = build(:fs_s3, bucket_url: nil)

      assert {:error, errors} = Hubs.create_file_system(team, file_system)
      assert "can't be blank" in errors[:bucket_url]
    end
  end

  describe "update_file_system/2" do
    test "updates a file system", %{team: team} do
      bucket_url = "https://#{team.id}.s3.amazonaws.com"
      file_system = build(:fs_s3, bucket_url: bucket_url)

      assert Hubs.create_file_system(team, file_system) == :ok

      assert_receive {:file_system_created,
                      %{bucket_url: ^bucket_url, external_id: external_id} = file_system}

      assert file_system in Hubs.get_file_systems(team)

      updated_file_system = Map.replace!(file_system, :region, "eu-central-1")

      assert Hubs.update_file_system(team, updated_file_system) == :ok
      assert_receive {:file_system_updated, %{external_id: ^external_id, region: "eu-central-1"}}
      refute file_system in Hubs.get_file_systems(team)
      assert updated_file_system in Hubs.get_file_systems(team)
    end

    test "returns changeset errors when data is invalid",
         %{team: team, node: node, org_key: org_key} do
      file_system = TeamsRPC.create_file_system(node, team, org_key)
      update_file_system = Map.replace!(file_system, :bucket_url, "")

      assert {:error, errors} = Hubs.update_file_system(team, update_file_system)
      assert "can't be blank" in errors[:bucket_url]
    end
  end

  test "delete_file_system/2 deletes a file system", %{team: team} do
    bucket_url = "https://#{team.id}.s3.amazonaws.com"
    file_system = build(:fs_s3, bucket_url: bucket_url)

    assert Hubs.create_file_system(team, file_system) == :ok

    assert_receive {:file_system_created,
                    %{bucket_url: ^bucket_url, external_id: external_id} = file_system}

    assert file_system in Hubs.get_file_systems(team)

    assert Hubs.delete_file_system(team, file_system) == :ok
    assert_receive {:file_system_deleted, %{external_id: ^external_id}}
    refute file_system in Hubs.get_file_systems(team)

    # Guarantee it's been removed and will return HTTP status 404
    assert Hubs.delete_file_system(team, file_system) ==
             {:transport_error,
              "Something went wrong, try again later or please file a bug if it persists"}
  end

  test "generates and verifies stamp for a notebook", %{team: team} do
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
