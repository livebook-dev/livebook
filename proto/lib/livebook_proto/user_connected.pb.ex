defmodule LivebookProto.UserConnected do
  use Protobuf, protoc_gen_elixir_version: "0.14.1", syntax: :proto3

  field :name, 1, type: :string
  field :secrets, 2, repeated: true, type: LivebookProto.Secret
  field :file_systems, 3, repeated: true, type: LivebookProto.FileSystem, json_name: "fileSystems"

  field :deployment_groups, 4,
    repeated: true,
    type: LivebookProto.DeploymentGroup,
    json_name: "deploymentGroups"

  field :app_deployments, 5,
    repeated: true,
    type: LivebookProto.AppDeployment,
    json_name: "appDeployments"

  field :agents, 6, repeated: true, type: LivebookProto.Agent
  field :billing_status, 7, type: LivebookProto.BillingStatus, json_name: "billingStatus"
end
