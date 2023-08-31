defmodule Livebook.TeamsTest do
  use Livebook.TeamsIntegrationCase, async: true

  alias Livebook.Teams
  alias Livebook.Teams.Org

  describe "create_org/1" do
    test "returns the device flow data to confirm the org creation" do
      org = build(:org)

      assert {:ok,
              %{
                "device_code" => _device_code,
                "expires_in" => 300,
                "id" => _org_id,
                "user_code" => _user_code,
                "verification_uri" => _verification_uri
              }} = Teams.create_org(org, %{})
    end

    test "returns changeset errors when data is invalid" do
      org = build(:org)

      assert {:error, changeset} = Teams.create_org(org, %{name: nil})
      assert "can't be blank" in errors_on(changeset).name
    end
  end

  describe "join_org/1" do
    test "returns the device flow data to confirm the org creation", %{user: user, node: node} do
      org = build(:org)
      key_hash = Org.key_hash(org)

      teams_org = :erpc.call(node, Hub.Integration, :create_org, [[name: org.name]])
      :erpc.call(node, Hub.Integration, :create_org_key, [[org: teams_org, key_hash: key_hash]])
      :erpc.call(node, Hub.Integration, :create_user_org, [[org: teams_org, user: user]])

      assert {:ok,
              %{
                "device_code" => _device_code,
                "expires_in" => 300,
                "id" => _org_id,
                "user_code" => _user_code,
                "verification_uri" => _verification_uri
              }} = Teams.join_org(org, %{})
    end

    test "returns changeset errors when data is invalid" do
      org = build(:org)

      assert {:error, changeset} = Teams.join_org(org, %{name: nil})
      assert "can't be blank" in errors_on(changeset).name
    end

    test "returns changeset errors when org doesn't exist" do
      org = build(:org)

      assert {:error, changeset} = Teams.join_org(org, %{})
      assert "does not exist" in errors_on(changeset).name
      assert "does not match existing key" in errors_on(changeset).teams_key
    end
  end

  describe "get_org_request_completion_data/1" do
    test "returns the org data when it has been confirmed", %{node: node, user: user} do
      teams_key = Teams.Org.teams_key()
      key_hash = :crypto.hash(:sha256, teams_key) |> Base.url_encode64(padding: false)

      org_request = :erpc.call(node, Hub.Integration, :create_org_request, [[key_hash: key_hash]])
      org_request = :erpc.call(node, Hub.Integration, :confirm_org_request, [org_request, user])

      org =
        build(:org,
          id: org_request.id,
          name: org_request.name,
          teams_key: teams_key,
          user_code: org_request.user_code
        )

      %{
        token: token,
        user_org: %{
          org: %{
            id: id,
            name: name,
            keys: [%{id: org_key_id}],
            key_pair: %{public_key: org_public_key}
          },
          user: %{id: user_id}
        }
      } = org_request.user_org_session

      assert Teams.get_org_request_completion_data(org, org_request.device_code) ==
               {:ok,
                %{
                  "id" => id,
                  "name" => name,
                  "org_key_id" => org_key_id,
                  "org_public_key" => org_public_key,
                  "session_token" => token,
                  "user_id" => user_id
                }}
    end

    test "returns the org request awaiting confirmation", %{node: node} do
      teams_key = Teams.Org.teams_key()
      key_hash = :crypto.hash(:sha256, teams_key) |> Base.url_encode64(padding: false)

      org_request = :erpc.call(node, Hub.Integration, :create_org_request, [[key_hash: key_hash]])

      org =
        build(:org,
          id: org_request.id,
          name: org_request.name,
          teams_key: teams_key,
          user_code: org_request.user_code
        )

      assert Teams.get_org_request_completion_data(org, org_request.device_code) ==
               {:ok, :awaiting_confirmation}
    end

    test "returns error when org request doesn't exist" do
      org = build(:org, id: 0)
      assert {:transport_error, _embarrassing} = Teams.get_org_request_completion_data(org, "")
    end

    test "returns error when org request expired", %{node: node} do
      now = NaiveDateTime.truncate(NaiveDateTime.utc_now(), :second)
      expires_at = NaiveDateTime.add(now, -5000)
      teams_key = Teams.Org.teams_key()
      key_hash = :crypto.hash(:sha256, teams_key) |> Base.url_encode64(padding: false)

      org_request =
        :erpc.call(node, Hub.Integration, :create_org_request, [
          [expires_at: expires_at, key_hash: key_hash]
        ])

      org =
        build(:org,
          id: org_request.id,
          name: org_request.name,
          teams_key: teams_key,
          user_code: org_request.user_code
        )

      assert Teams.get_org_request_completion_data(org, org_request.device_code) ==
               {:error, :expired}
    end
  end

  describe "create_secret/2" do
    test "creates a new secret", %{user: user, node: node} do
      hub = create_team_hub(user, node)
      secret = build(:secret, name: "FOO", value: "BAR")

      assert Teams.create_secret(hub, secret) == :ok

      # Guarantee uniqueness
      assert {:error, changeset} = Teams.create_secret(hub, secret)
      assert "has already been taken" in errors_on(changeset).name
    end

    test "returns changeset errors when data is invalid", %{user: user, node: node} do
      hub = create_team_hub(user, node)
      secret = build(:secret, name: "LB_FOO", value: "BAR")

      assert {:error, changeset} = Teams.create_secret(hub, secret)
      assert "cannot start with the LB_ prefix" in errors_on(changeset).name
    end
  end

  describe "update_secret/2" do
    test "updates a secret", %{user: user, node: node} do
      hub = create_team_hub(user, node)
      secret = build(:secret, name: "UPDATE_ME", value: "BAR")

      assert Teams.create_secret(hub, secret) == :ok

      update_secret = Map.replace!(secret, :value, "BAZ")
      assert Teams.update_secret(hub, update_secret) == :ok
    end

    test "returns changeset errors when data is invalid", %{user: user, node: node} do
      hub = create_team_hub(user, node)
      secret = build(:secret, name: "FIX_ME", value: "BAR")

      assert Teams.create_secret(hub, secret) == :ok

      update_secret = Map.replace!(secret, :value, "")

      assert {:error, changeset} = Teams.update_secret(hub, update_secret)
      assert "can't be blank" in errors_on(changeset).value
    end
  end

  describe "delete_secret/2" do
    test "deletes a secret", %{user: user, node: node} do
      hub = create_team_hub(user, node)
      secret = build(:secret, name: "DELETE_ME", value: "BAR")

      assert Teams.create_secret(hub, secret) == :ok
      assert Teams.delete_secret(hub, secret) == :ok

      # Guarantee it's been removed and will return HTTP status 404
      assert Teams.delete_secret(hub, secret) ==
               {:transport_error,
                "Something went wrong, try again later or please file a bug if it persists"}
    end

    test "returns transport errors when secret doesn't exists", %{user: user, node: node} do
      hub = create_team_hub(user, node)
      secret = build(:secret, name: "I_CANT_EXIST", value: "BAR")

      # Guarantee it doesn't exists and will return HTTP status 404
      assert Teams.delete_secret(hub, secret) ==
               {:transport_error,
                "Something went wrong, try again later or please file a bug if it persists"}
    end
  end

  describe "create_file_system/2" do
    test "creates a new file system", %{user: user, node: node} do
      hub = create_team_hub(user, node)
      file_system = build(:fs_s3, bucket_url: "https://file_system_created.s3.amazonaws.com")

      assert Teams.create_file_system(hub, file_system) == :ok

      # Guarantee uniqueness
      assert {:error, changeset} = Teams.create_file_system(hub, file_system)
      assert "has already been taken" in errors_on(changeset).bucket_url
    end

    test "returns changeset errors when data is invalid", %{user: user, node: node} do
      hub = create_team_hub(user, node)
      file_system = build(:fs_s3, bucket_url: nil)

      assert {:error, changeset} = Teams.create_file_system(hub, file_system)
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
      assert Teams.update_file_system(hub, update_file_system) == :ok
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

      assert {:error, changeset} = Teams.update_file_system(hub, update_file_system)
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

      assert Teams.delete_file_system(hub, file_system) == :ok

      # Guarantee it's been removed and will return HTTP status 404
      assert Teams.delete_file_system(hub, file_system) ==
               {:transport_error,
                "Something went wrong, try again later or please file a bug if it persists"}
    end

    test "returns transport errors when file system doesn't exists", %{user: user, node: node} do
      hub = create_team_hub(user, node)
      file_system = build(:fs_s3, bucket_url: "https://i_cant_exist.s3.amazonaws.com")

      # Guarantee it doesn't exists and will return HTTP status 404
      assert Teams.delete_file_system(hub, file_system) ==
               {:transport_error,
                "Something went wrong, try again later or please file a bug if it persists"}
    end
  end

  defp create_teams_file_system(hub, node) do
    org_key = :erpc.call(node, Hub.Integration, :get_org_key!, [hub.org_key_id])

    :erpc.call(node, Hub.Integration, :create_file_system, [[org_key: org_key]])
  end
end
