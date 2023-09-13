defmodule Livebook.Hubs.TeamClientTest do
  use Livebook.TeamsIntegrationCase, async: true

  alias Livebook.Hubs.TeamClient

  @moduletag :capture_log

  setup do
    Livebook.Hubs.subscribe([:connection, :file_systems, :secrets])
    :ok
  end

  describe "start_link/1" do
    test "successfully authenticates the web socket connection", %{user: user, node: node} do
      team = build_team_hub(user, node)
      id = team.id

      refute TeamClient.connected?(team.id)

      TeamClient.start_link(team)
      assert_receive {:hub_connected, ^id}
      assert TeamClient.connected?(team.id)
    end

    test "rejects the web socket connection with invalid credentials", %{user: user, token: token} do
      team =
        build(:team,
          user_id: user.id,
          org_id: 123_456,
          org_key_id: 123_456,
          session_token: token
        )

      id = team.id

      TeamClient.start_link(team)

      assert_receive {:hub_server_error, ^id, error}

      assert error ==
               "#{team.hub_name}: Your session is out-of-date. Please re-join the organization."

      refute Livebook.Hubs.hub_exists?(team.id)
    end
  end

  describe "handle events" do
    test "receives the secret_created event", %{user: user, node: node} do
      team = create_team_hub(user, node)
      id = team.id

      assert_receive {:hub_connected, ^id}

      secret = build(:secret, name: "SECRET_CREATED_FOO", value: "BAR")
      assert Livebook.Teams.create_secret(team, secret) == :ok

      name = secret.name
      value = secret.value

      # receives `{:event, :secret_created, secret_created}` event
      # with the value decrypted
      assert_receive {:secret_created, %{name: ^name, value: ^value}}
    end

    test "receives the secret_updated event", %{user: user, node: node} do
      team = create_team_hub(user, node)
      id = team.id

      assert_receive {:hub_connected, ^id}

      secret = build(:secret, name: "SECRET_UPDATED_FOO", value: "BAR")
      assert Livebook.Teams.create_secret(team, secret) == :ok

      name = secret.name
      value = secret.value

      # receives `{:secret_created, secret_created}` event
      assert_receive {:secret_created, %{name: ^name, value: ^value}}

      # updates the secret
      update_secret = Map.replace!(secret, :value, "BAZ")
      assert Livebook.Teams.update_secret(team, update_secret) == :ok

      new_value = update_secret.value

      # receives `{:secret_updated, secret_updated}` event
      # with the value decrypted
      assert_receive {:secret_updated, %{name: ^name, value: ^new_value}}
    end

    test "receives the secret_deleted event", %{user: user, node: node} do
      team = create_team_hub(user, node)
      id = team.id

      assert_receive {:hub_connected, ^id}

      secret = build(:secret, name: "SECRET_DELETED_FOO", value: "BAR")
      assert Livebook.Teams.create_secret(team, secret) == :ok

      name = secret.name
      value = secret.value

      # receives `{:secret_created, secret_created}` event
      assert_receive {:secret_created, %{name: ^name, value: ^value}}

      # deletes the secret
      assert Livebook.Teams.delete_secret(team, secret) == :ok

      # receives `{:secret_deleted, secret_deleted}` event
      assert_receive {:secret_deleted, %{name: ^name, value: ^value}}
    end

    test "receives the file_system_created event", %{user: user, node: node} do
      team = create_team_hub(user, node)
      id = team.id

      assert_receive {:hub_connected, ^id}

      file_system = build(:fs_s3, bucket_url: "https://file_system_created.s3.amazonaws.com")
      assert Livebook.Teams.create_file_system(team, file_system) == :ok

      bucket_url = file_system.bucket_url

      # receives `{:event, :file_system_created, file_system_created}` event
      assert_receive {:file_system_created, %{bucket_url: ^bucket_url}}
    end

    test "receives the file_system_updated event", %{user: user, node: node} do
      team = create_team_hub(user, node)
      id = team.id

      assert_receive {:hub_connected, ^id}

      file_system =
        build(:fs_s3,
          bucket_url: "https://file_system_updated.s3.amazonaws.com",
          region: "us-east-1"
        )

      assert Livebook.Teams.create_file_system(team, file_system) == :ok

      bucket_url = file_system.bucket_url
      region = file_system.region

      # receives `{:file_system_created, file_system_created}` event
      assert_receive {:file_system_created,
                      %{external_id: id, bucket_url: ^bucket_url, region: ^region}}

      # updates the file system
      update_file_system = %{file_system | region: "eu-central-1", external_id: id}
      assert Livebook.Teams.update_file_system(team, update_file_system) == :ok

      new_region = update_file_system.region

      # receives `{:file_system_updated, file_system_updated}` event
      assert_receive {:file_system_updated,
                      %{external_id: ^id, bucket_url: ^bucket_url, region: ^new_region}}
    end

    test "receives the file_system_deleted event", %{user: user, node: node} do
      team = create_team_hub(user, node)
      id = team.id

      assert_receive {:hub_connected, ^id}

      file_system = build(:fs_s3, bucket_url: "https://file_system_deleted.s3.amazonaws.com")
      assert Livebook.Teams.create_file_system(team, file_system) == :ok

      bucket_url = file_system.bucket_url

      # receives `{:file_system_created, file_system_created}` event
      assert_receive {:file_system_created, %{external_id: id, bucket_url: ^bucket_url}}

      # deletes the file system
      delete_file_system = %{file_system | external_id: id}
      assert Livebook.Teams.delete_file_system(team, delete_file_system) == :ok

      # receives `{:file_system_deleted, file_system_deleted}` event
      assert_receive {:file_system_deleted, %{external_id: ^id, bucket_url: ^bucket_url}}
    end
  end

  describe "user connected event" do
    test "fills the secrets list", %{user: user, node: node} do
      team = build_team_hub(user, node)
      id = team.id

      secret =
        build(:secret,
          name: "SECRET_CREATED",
          value: "an encrypted value",
          hub_id: id
        )

      {secret_key, sign_secret} = Livebook.Teams.derive_keys(team.teams_key)
      secret_value = Livebook.Teams.encrypt(secret.value, secret_key, sign_secret)
      livebook_proto_secret = LivebookProto.Secret.new!(name: secret.name, value: secret_value)

      user_connected =
        LivebookProto.UserConnected.new!(
          name: team.hub_name,
          secrets: [livebook_proto_secret],
          file_systems: []
        )

      {:ok, pid} = TeamClient.start_link(team)
      assert_receive {:hub_connected, ^id}
      refute_receive {:secret_created, ^secret}

      send(pid, {:event, :user_connected, user_connected})
      assert_receive {:secret_created, ^secret}
      assert secret in TeamClient.get_secrets(team.id)
    end

    test "replaces the secret with updated value", %{user: user, node: node} do
      team = build_team_hub(user, node)
      id = team.id

      secret =
        build(:secret,
          name: "SECRET_UPDATED",
          value: "an encrypted value",
          hub_id: id
        )

      {secret_key, sign_secret} = Livebook.Teams.derive_keys(team.teams_key)
      secret_value = Livebook.Teams.encrypt(secret.value, secret_key, sign_secret)
      livebook_proto_secret = LivebookProto.Secret.new!(name: secret.name, value: secret_value)

      user_connected =
        LivebookProto.UserConnected.new!(
          name: team.hub_name,
          secrets: [livebook_proto_secret],
          file_systems: []
        )

      {:ok, pid} = TeamClient.start_link(team)
      assert_receive {:hub_connected, ^id}
      refute_receive {:secret_created, ^secret}

      send(pid, {:event, :user_connected, user_connected})
      assert_receive {:secret_created, ^secret}

      updated_secret = %{secret | value: "an updated value"}
      secret_value = Livebook.Teams.encrypt(updated_secret.value, secret_key, sign_secret)

      updated_livebook_proto_secret =
        LivebookProto.Secret.new!(name: updated_secret.name, value: secret_value)

      user_connected =
        LivebookProto.UserConnected.new!(
          name: team.hub_name,
          secrets: [updated_livebook_proto_secret],
          file_systems: []
        )

      send(pid, {:event, :user_connected, user_connected})
      assert_receive {:secret_updated, ^updated_secret}

      refute secret in TeamClient.get_secrets(team.id)
      assert updated_secret in TeamClient.get_secrets(team.id)
    end

    test "deletes the secret", %{user: user, node: node} do
      team = build_team_hub(user, node)
      id = team.id

      secret =
        build(:secret,
          name: "SECRET_UPDATED",
          value: "an encrypted value",
          hub_id: id
        )

      {secret_key, sign_secret} = Livebook.Teams.derive_keys(team.teams_key)
      secret_value = Livebook.Teams.encrypt(secret.value, secret_key, sign_secret)
      livebook_proto_secret = LivebookProto.Secret.new!(name: secret.name, value: secret_value)

      user_connected =
        LivebookProto.UserConnected.new!(
          name: team.hub_name,
          secrets: [livebook_proto_secret],
          file_systems: []
        )

      {:ok, pid} = TeamClient.start_link(team)
      assert_receive {:hub_connected, ^id}
      refute_receive {:secret_created, ^secret}

      send(pid, {:event, :user_connected, user_connected})
      assert_receive {:secret_created, ^secret}

      user_connected =
        LivebookProto.UserConnected.new!(
          name: team.hub_name,
          secrets: [],
          file_systems: []
        )

      send(pid, {:event, :user_connected, user_connected})
      assert_receive {:secret_deleted, ^secret}

      refute secret in TeamClient.get_secrets(team.id)
    end

    test "fills the file systems list", %{user: user, node: node} do
      team = build_team_hub(user, node)
      id = team.id

      bucket_url = "https://mybucket.s3.amazonaws.com"
      hash = :crypto.hash(:sha256, bucket_url)
      fs_id = "#{id}-s3-#{Base.url_encode64(hash, padding: false)}"

      file_system = build(:fs_s3, id: fs_id, bucket_url: bucket_url, external_id: "123456")

      type = Livebook.FileSystems.type(file_system)
      %{name: name} = Livebook.FileSystem.external_metadata(file_system)
      attrs = Livebook.FileSystem.dump(file_system)
      credentials = Jason.encode!(attrs)

      {secret_key, sign_secret} = Livebook.Teams.derive_keys(team.teams_key)
      value = Livebook.Teams.encrypt(credentials, secret_key, sign_secret)

      livebook_proto_file_system =
        LivebookProto.FileSystem.new!(
          id: file_system.external_id,
          name: name,
          type: to_string(type),
          value: value
        )

      user_connected =
        LivebookProto.UserConnected.new!(
          name: team.hub_name,
          secrets: [],
          file_systems: [livebook_proto_file_system]
        )

      {:ok, pid} = TeamClient.start_link(team)
      assert_receive {:hub_connected, ^id}
      refute_receive {:file_system_created, ^file_system}

      send(pid, {:event, :user_connected, user_connected})
      assert_receive {:file_system_created, ^file_system}
      assert file_system in TeamClient.get_file_systems(team.id)
    end

    test "replaces the file system with updated value", %{user: user, node: node} do
      team = build_team_hub(user, node)
      id = team.id

      bucket_url = "https://update_fs_994641.s3.amazonaws.com"
      hash = :crypto.hash(:sha256, bucket_url)
      fs_id = "#{id}-s3-#{Base.url_encode64(hash, padding: false)}"

      file_system = build(:fs_s3, id: fs_id, bucket_url: bucket_url, external_id: "994641")

      type = Livebook.FileSystems.type(file_system)
      %{name: name} = Livebook.FileSystem.external_metadata(file_system)
      attrs = Livebook.FileSystem.dump(file_system)
      credentials = Jason.encode!(attrs)

      {secret_key, sign_secret} = Livebook.Teams.derive_keys(team.teams_key)
      value = Livebook.Teams.encrypt(credentials, secret_key, sign_secret)

      livebook_proto_file_system =
        LivebookProto.FileSystem.new!(
          id: file_system.external_id,
          name: name,
          type: to_string(type),
          value: value
        )

      user_connected =
        LivebookProto.UserConnected.new!(
          name: team.hub_name,
          secrets: [],
          file_systems: [livebook_proto_file_system]
        )

      {:ok, pid} = TeamClient.start_link(team)
      assert_receive {:hub_connected, ^id}
      refute_receive {:file_system_created, ^file_system}

      send(pid, {:event, :user_connected, user_connected})
      assert_receive {:file_system_created, ^file_system}

      updated_file_system = %{
        file_system
        | id: "#{id}-s3-ATC52Lo7d-bS21OLjPQ8KFBPJN8ku4hCn2nic2jTGeI",
          bucket_url: "https://updated_name.s3.amazonaws.com"
      }

      updated_attrs = Livebook.FileSystem.dump(updated_file_system)
      updated_credentials = Jason.encode!(updated_attrs)
      updated_value = Livebook.Teams.encrypt(updated_credentials, secret_key, sign_secret)

      updated_livebook_proto_file_system =
        LivebookProto.FileSystem.new!(
          id: updated_file_system.external_id,
          name: updated_file_system.bucket_url,
          type: to_string(type),
          value: updated_value
        )

      user_connected =
        LivebookProto.UserConnected.new!(
          name: team.hub_name,
          secrets: [],
          file_systems: [updated_livebook_proto_file_system]
        )

      send(pid, {:event, :user_connected, user_connected})
      assert_receive {:file_system_updated, ^updated_file_system}

      refute file_system in TeamClient.get_file_systems(team.id)
      assert updated_file_system in TeamClient.get_file_systems(team.id)
    end

    test "deletes the file system", %{user: user, node: node} do
      team = build_team_hub(user, node)
      id = team.id

      bucket_url = "https://delete_fs_45465641.s3.amazonaws.com"
      hash = :crypto.hash(:sha256, bucket_url)
      fs_id = "#{id}-s3-#{Base.url_encode64(hash, padding: false)}"

      file_system = build(:fs_s3, id: fs_id, bucket_url: bucket_url, external_id: "45465641")

      type = Livebook.FileSystems.type(file_system)
      %{name: name} = Livebook.FileSystem.external_metadata(file_system)
      attrs = Livebook.FileSystem.dump(file_system)
      credentials = Jason.encode!(attrs)

      {secret_key, sign_secret} = Livebook.Teams.derive_keys(team.teams_key)
      value = Livebook.Teams.encrypt(credentials, secret_key, sign_secret)

      livebook_proto_file_system =
        LivebookProto.FileSystem.new!(
          id: file_system.external_id,
          name: name,
          type: to_string(type),
          value: value
        )

      user_connected =
        LivebookProto.UserConnected.new!(
          name: team.hub_name,
          secrets: [],
          file_systems: [livebook_proto_file_system]
        )

      {:ok, pid} = TeamClient.start_link(team)
      assert_receive {:hub_connected, ^id}
      refute_receive {:file_system_created, ^file_system}

      send(pid, {:event, :user_connected, user_connected})
      assert_receive {:file_system_created, ^file_system}

      user_connected =
        LivebookProto.UserConnected.new!(
          name: team.hub_name,
          secrets: [],
          file_systems: []
        )

      send(pid, {:event, :user_connected, user_connected})
      assert_receive {:file_system_deleted, ^file_system}

      refute file_system in TeamClient.get_file_systems(team.id)
    end
  end
end
