defmodule LivebookProto.AgentConnected do
  use Protobuf, protoc_gen_elixir_version: "0.14.1", syntax: :proto3

  field :name, 2, type: :string
  field :public_key, 3, type: :string, json_name: "publicKey"
  field :deployment_group_id, 4, type: :int32, json_name: "deploymentGroupId"
  field :secrets, 5, repeated: true, type: LivebookProto.Secret
  field :file_systems, 6, repeated: true, type: LivebookProto.FileSystem, json_name: "fileSystems"

  field :deployment_groups, 7,
    repeated: true,
    type: LivebookProto.DeploymentGroup,
    json_name: "deploymentGroups"

  field :app_deployments, 8,
    repeated: true,
    type: LivebookProto.AppDeployment,
    json_name: "appDeployments"

  field :agents, 9, repeated: true, type: LivebookProto.Agent
  field :billing_status, 10, type: LivebookProto.BillingStatus, json_name: "billingStatus"
end
