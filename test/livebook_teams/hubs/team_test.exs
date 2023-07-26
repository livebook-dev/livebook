defmodule Livebook.Hubs.TeamTest do
  use Livebook.TeamsIntegrationCase, async: true

  alias Livebook.Hubs.Provider

  @moduletag :capture_log

  describe "stamping" do
    test "generates and verifies stamp for a notebook", %{user: user, node: node} do
      team = create_team_hub(user, node)

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
