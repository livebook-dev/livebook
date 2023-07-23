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
      org = :erpc.call(node, Hub.Integration, :create_org, [])
      org_key = :erpc.call(node, Hub.Integration, :create_org_key, [[org: org]])
      token = :erpc.call(node, Hub.Integration, :associate_user_with_org, [user, org])

      hub =
        build(:team,
          user_id: user.id,
          org_id: org.id,
          org_key_id: org_key.id,
          session_token: token
        )

      secret = build(:secret, name: "FOO", value: "BAR")

      assert Teams.create_secret(hub, secret) == :ok

      # Guarantee uniqueness
      assert {:error, changeset} = Teams.create_secret(hub, secret)
      assert "has already been taken" in errors_on(changeset).name
    end

    test "returns changeset errors when data is invalid", %{user: user, node: node} do
      org = :erpc.call(node, Hub.Integration, :create_org, [])
      org_key = :erpc.call(node, Hub.Integration, :create_org_key, [[org: org]])
      token = :erpc.call(node, Hub.Integration, :associate_user_with_org, [user, org])

      hub =
        build(:team,
          user_id: user.id,
          org_id: org.id,
          org_key_id: org_key.id,
          session_token: token
        )

      secret = build(:secret, name: "LB_FOO", value: "BAR")

      assert {:error, changeset} = Teams.create_secret(hub, secret)
      assert "cannot start with the LB_ prefix" in errors_on(changeset).name
    end
  end
end
