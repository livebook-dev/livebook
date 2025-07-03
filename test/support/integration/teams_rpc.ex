defmodule Livebook.TeamsRPC do
  alias Livebook.Factory

  def subscribe(node, parent, deployment_group, org) do
    :erpc.call(node, TeamsRPC, :subscribe, [parent, deployment_group, org])
  end

  # Read resource

  def get_org_request!(node, id) do
    :erpc.call(node, TeamsRPC, :get_org_request!, [id])
  end

  def get_org_request_by!(node, opts) do
    :erpc.call(node, TeamsRPC, :get_org_request_by!, [opts])
  end

  def get_org_key!(node, id) do
    :erpc.call(node, TeamsRPC, :get_org_key!, [id])
  end

  def get_deployment_group!(node, id) do
    :erpc.call(node, TeamsRPC, :get_deployment_group!, [id])
  end

  def get_auth_request_by_code!(node, code) do
    :erpc.call(node, TeamsRPC, :get_auth_request_by_code!, [code])
  end

  def get_apps_metadatas(node, id) do
    :erpc.call(node, TeamsRPC, :get_apps_metadatas, [id])
  end

  # Create resource

  def create_user(node, attrs \\ []) do
    :erpc.call(node, TeamsRPC, :create_user, [attrs])
  end

  def create_secret(node, team, org_key, attrs \\ []) do
    attrs = Keyword.merge(attrs, hub_id: team.id)
    secret = Factory.build(:secret, attrs)

    derived_key = Livebook.Teams.derive_key(team.teams_key)
    value = Livebook.Teams.encrypt(secret.value, derived_key)

    attrs =
      %{
        name: secret.name,
        value: value,
        org_key: org_key
      }

    :erpc.call(node, TeamsRPC, :create_secret, [attrs])
    secret
  end

  def create_org(node, attrs \\ []) do
    :erpc.call(node, TeamsRPC, :create_org, [attrs])
  end

  def create_org_key(node, attrs \\ []) do
    :erpc.call(node, TeamsRPC, :create_org_key, [attrs])
  end

  def create_org_key_pair(node, attrs \\ []) do
    :erpc.call(node, TeamsRPC, :create_org_key_pair, [attrs])
  end

  def create_org_request(node, attrs \\ []) do
    :erpc.call(node, TeamsRPC, :create_org_request, [attrs])
  end

  def create_user_org(node, attrs \\ []) do
    :erpc.call(node, TeamsRPC, :create_user_org, [attrs])
  end

  def create_file_system(node, team, org_key, file_system \\ nil) do
    file_system = if file_system, do: file_system, else: Factory.build(:fs_s3)
    derived_key = Livebook.Teams.derive_key(team.teams_key)
    name = Livebook.FileSystem.external_metadata(file_system).name
    type = Livebook.FileSystems.type(file_system)
    attrs = Livebook.FileSystem.dump(file_system)
    json = JSON.encode!(attrs)
    value = Livebook.Teams.encrypt(json, derived_key)

    attrs = %{name: name, type: String.to_atom(type), value: value, org_key: org_key}
    external_id = :erpc.call(node, TeamsRPC, :create_file_system, [attrs]).id
    Map.replace!(file_system, :external_id, external_id)
  end

  def create_deployment_group(node, attrs \\ []) do
    :erpc.call(node, TeamsRPC, :create_deployment_group, [attrs])
  end

  def create_deployment_group_secret(node, team, org_key, deployment_group, attrs \\ []) do
    attrs =
      Keyword.merge(attrs,
        deployment_group_id: to_string(deployment_group.id),
        hub_id: team.id
      )

    secret = Factory.build(:secret, attrs)

    derived_key = Livebook.Teams.derive_key(team.teams_key)
    value = Livebook.Teams.encrypt(secret.value, derived_key)

    attrs =
      %{
        name: secret.name,
        value: value,
        org_key: org_key,
        deployment_group: deployment_group
      }

    :erpc.call(node, TeamsRPC, :create_deployment_group_secret, [attrs])
    secret
  end

  def create_agent_key(node, attrs \\ []) do
    :erpc.call(node, TeamsRPC, :create_agent_key, [attrs])
  end

  def create_environment_variable(node, attrs \\ []) do
    :erpc.call(node, TeamsRPC, :create_environment_variable, [attrs])
  end

  def create_billing_subscription(node, org) do
    :erpc.call(node, TeamsRPC, :create_billing_subscription, [org])
  end

  def create_oidc_provider(node, org) do
    :erpc.call(node, TeamsRPC, :create_oidc_provider, [org])
  end

  def create_authorization_group(node, attrs \\ []) do
    :erpc.call(node, TeamsRPC, :create_authorization_group, [attrs])
  end

  def create_deploy_key(node, attrs \\ []) do
    key = :erpc.call(node, TeamsRPC, :generate_deploy_key, [])
    {key, :erpc.call(node, TeamsRPC, :create_deploy_key, [key, attrs])}
  end

  # Update resource

  def update_authorization_group(node, authorization_group, attrs) do
    :erpc.call(node, TeamsRPC, :update_authorization_group, [authorization_group, attrs])
  end

  def update_user_info_groups(node, code, groups) do
    :erpc.call(node, TeamsRPC, :update_user_info_groups, [code, groups])
  end

  def update_deployment_group(node, deployment_group, attrs) do
    :erpc.call(node, TeamsRPC, :update_deployment_group, [deployment_group, attrs])
  end

  # Delete resource

  def delete_user_org(node, user_id, org_id) do
    :erpc.call(node, TeamsRPC, :delete_user_org, [user_id, org_id])
  end

  def delete_deployment_group(node, deployment_group) do
    :erpc.call(node, TeamsRPC, :delete_deployment_group, [deployment_group])
  end

  # Actions

  def upload_app_deployment(
        node,
        org,
        deployment_group,
        livebook_app_deployment,
        encrypted_content,
        broadast? \\ false
      ) do
    :erpc.call(node, TeamsRPC, :upload_app_deployment, [
      org,
      deployment_group,
      livebook_app_deployment,
      encrypted_content,
      broadast?
    ])
  end

  def confirm_org_request(node, org_request, user) do
    :erpc.call(node, TeamsRPC, :confirm_org_request, [org_request, user])
  end

  def associate_user_with_org(node, user, org) do
    :erpc.call(node, TeamsRPC, :associate_user_with_org, [user, org])
  end

  def toggle_app_deployment(node, id, org_id) do
    :erpc.call(node, TeamsRPC, :toggle_app_deployment, [id, org_id])
  end

  def allow_auth_request(node, token) do
    :erpc.call(node, TeamsRPC, :allow_auth_request, [token])
  end

  def toggle_teams_authentication(node, deployment_group) do
    :erpc.call(node, TeamsRPC, :toggle_teams_authentication, [deployment_group])
  end

  def toggle_groups_authorization(node, deployment_group) do
    :erpc.call(node, TeamsRPC, :toggle_groups_authorization, [deployment_group])
  end
end
