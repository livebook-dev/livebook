defmodule Livebook.Hubs.TeamTest do
  use Livebook.TeamsIntegrationCase, async: true

  alias Livebook.Hubs.Provider

  @moduletag :capture_log

  describe "stamping" do
    test "generates and verifies stamp for a notebook", %{user: user, node: node} do
      org = :erpc.call(node, Hub.Integration, :create_org, [])
      org_key = :erpc.call(node, Hub.Integration, :create_org_key, [[org: org]])
      org_key_pair = :erpc.call(node, Hub.Integration, :create_org_key_pair, [[org: org]])
      token = :erpc.call(node, Hub.Integration, :associate_user_with_org, [user, org])

      team =
        build(:team,
          id: "team-#{org.name}",
          hub_name: org.name,
          user_id: user.id,
          org_id: org.id,
          org_key_id: org_key.id,
          org_public_key: org_key_pair.public_key,
          session_token: token
        )

      notebook_source = """
      # Team notebook

      # Intro

      ```elixir
      IO.puts("Hello!")
      ```
      """

      metadata = %{"key" => "value"}

      assert {:ok, stamp} = Provider.notebook_stamp(team, notebook_source, metadata)

      assert {:ok, ^metadata} = Provider.verify_notebook_stamp(team, notebook_source, stamp)

      assert :error = Provider.verify_notebook_stamp(team, notebook_source <> "change\n", stamp)
    end
  end
end
