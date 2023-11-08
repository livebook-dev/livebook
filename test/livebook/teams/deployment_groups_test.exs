defmodule Livebook.Teams.DeploymentGroupsTest do
  use Livebook.DataCase, async: true

  alias Livebook.Teams.DeploymentGroups
  alias Livebook.Teams.DeploymentGroups.DeploymentGroup

  describe "update_deployment_group/2" do
    test "returns a valid deployment group" do
      attrs = params_for(:deployment_group, name: "FOO", mode: :online)

      assert {:ok, deployment_group} =
               DeploymentGroups.update_deployment_group(%DeploymentGroup{}, attrs)

      assert attrs.name == deployment_group.name
      assert attrs.mode == deployment_group.mode
      assert attrs.hub_id == deployment_group.hub_id
    end

    test "returns changeset error" do
      attrs = params_for(:deployment_group, name: "", mode: "ofline")

      assert {:error, changeset} =
               DeploymentGroups.update_deployment_group(%DeploymentGroup{}, attrs)

      assert "can't be blank" in errors_on(changeset).name

      attrs = params_for(:deployment_group, name: "@name", mode: "")

      assert {:error, changeset} =
               DeploymentGroups.update_deployment_group(%DeploymentGroup{}, attrs)

      assert "can't be blank" in errors_on(changeset).mode
    end
  end
end
