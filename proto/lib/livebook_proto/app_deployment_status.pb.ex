defmodule LivebookProto.AppDeploymentStatus do
  use Protobuf, syntax: :proto3, protoc_gen_elixir_version: "0.12.0"

  field :id, 1, type: :string
  field :deployment_group_id, 2, type: :string, json_name: "deploymentGroupId"
  field :version, 3, type: :string
  field :status, 4, type: LivebookProto.AppDeploymentStatusType, enum: true
end
