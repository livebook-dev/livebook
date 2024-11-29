defmodule LivebookProto.Event do
  use Protobuf, syntax: :proto3, protoc_gen_elixir_version: "0.13.0"

  oneof :type, 0

  field :secret_created, 1,
    type: LivebookProto.SecretCreated,
    json_name: "secretCreated",
    oneof: 0

  field :secret_updated, 2,
    type: LivebookProto.SecretUpdated,
    json_name: "secretUpdated",
    oneof: 0

  field :secret_deleted, 3,
    type: LivebookProto.SecretDeleted,
    json_name: "secretDeleted",
    oneof: 0

  field :user_connected, 4,
    type: LivebookProto.UserConnected,
    json_name: "userConnected",
    oneof: 0

  field :file_system_created, 5,
    type: LivebookProto.FileSystemCreated,
    json_name: "fileSystemCreated",
    oneof: 0

  field :file_system_updated, 6,
    type: LivebookProto.FileSystemUpdated,
    json_name: "fileSystemUpdated",
    oneof: 0

  field :file_system_deleted, 7,
    type: LivebookProto.FileSystemDeleted,
    json_name: "fileSystemDeleted",
    oneof: 0

  field :deployment_group_created, 8,
    type: LivebookProto.DeploymentGroupCreated,
    json_name: "deploymentGroupCreated",
    oneof: 0

  field :deployment_group_updated, 9,
    type: LivebookProto.DeploymentGroupUpdated,
    json_name: "deploymentGroupUpdated",
    oneof: 0

  field :deployment_group_deleted, 10,
    type: LivebookProto.DeploymentGroupDeleted,
    json_name: "deploymentGroupDeleted",
    oneof: 0

  field :agent_connected, 11,
    type: LivebookProto.AgentConnected,
    json_name: "agentConnected",
    oneof: 0

  field :app_deployment_started, 12,
    type: LivebookProto.AppDeploymentStarted,
    json_name: "appDeploymentStarted",
    oneof: 0

  field :user_deleted, 13, type: LivebookProto.UserDeleted, json_name: "userDeleted", oneof: 0
  field :agent_joined, 14, type: LivebookProto.AgentJoined, json_name: "agentJoined", oneof: 0
  field :agent_left, 15, type: LivebookProto.AgentLeft, json_name: "agentLeft", oneof: 0

  field :app_deployment_stopped, 16,
    type: LivebookProto.AppDeploymentStopped,
    json_name: "appDeploymentStopped",
    oneof: 0

  field :org_updated, 17, type: LivebookProto.OrgUpdated, json_name: "orgUpdated", oneof: 0
end
