defmodule Livebook.Teams.ConnectionTest do
  alias Livebook.FileSystem
  use Livebook.TeamsIntegrationCase, async: true

  @moduletag :capture_log

  alias Livebook.Teams.Connection

  describe "connect" do
    test "successfully authenticates the websocket connection", %{user: user, node: node} do
      {_, headers} = build_team_headers(user, node)

      assert {:ok, _conn} = Connection.start_link(self(), headers)
      assert_receive :connected
    end

    test "rejects the websocket connection with invalid credentials", %{user: user} do
      headers = [
        {"x-user", to_string(user.id)},
        {"x-org", to_string(user.id)},
        {"x-org-key", to_string(user.id)},
        {"x-session-token", "foo"}
      ]

      assert {:ok, _conn} = Connection.start_link(self(), headers)

      assert_receive {:server_error,
                      "Your session is out-of-date. Please re-join the organization."}

      assert {:ok, _conn} = Connection.start_link(self(), [])

      assert_receive {:server_error,
                      "Invalid request. Please re-join the organization and update Livebook if the issue persists."}
    end
  end

  describe "handle events" do
    test "receives the secret_created event", %{user: user, node: node} do
      {hub, headers} = build_team_headers(user, node)

      assert {:ok, _conn} = Connection.start_link(self(), headers)
      assert_receive :connected

      # creates a new secret
      secret = build(:secret, name: "FOO", value: "BAR")
      assert Livebook.Hubs.create_secret(hub, secret) == :ok

      # receives `{:event, :secret_created, secret_created}` event
      # without decrypting the value
      assert_receive {:event, :secret_created, secret_created}
      assert secret_created.name == secret.name
      refute secret_created.value == secret.value
    end

    test "receives the file_system_created event", %{user: user, node: node} do
      {hub, headers} = build_team_headers(user, node)

      assert {:ok, _conn} = Connection.start_link(self(), headers)
      assert_receive :connected

      # creates a new file system
      file_system = build(:fs_s3, bucket_url: "https://file_system_created.s3.amazonaws.com")
      assert Livebook.Hubs.create_file_system(hub, file_system) == :ok
      type = Livebook.FileSystems.type(file_system)
      %{name: name} = FileSystem.external_metadata(file_system)

      # receives `{:event, :file_system_created, file_system_created}` event
      # without decrypting the value
      assert_receive {:event, :file_system_created, file_system_created}
      assert file_system_created.name == name
      assert file_system_created.type == to_string(type)
      refute file_system_created.value == FileSystem.dump(file_system)
      assert is_binary(file_system_created.value)
    end

    test "receives the deployment_group_created event", %{user: user, node: node} do
      {hub, headers} = build_team_headers(user, node)

      assert {:ok, _conn} = Connection.start_link(self(), headers)
      assert_receive :connected

      # creates a new deployment group
      deployment_group = build(:deployment_group, name: "FOO", mode: :offline)

      assert {:ok, _id} =
               Livebook.Teams.create_deployment_group(hub, deployment_group)

      # deployment_group name and mode are not encrypted
      assert_receive {:event, :deployment_group_created, deployment_group_created}
      assert deployment_group_created.name == deployment_group.name
      assert String.to_existing_atom(deployment_group_created.mode) == deployment_group.mode
    end
  end
end
