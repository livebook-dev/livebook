defmodule LivebookProto.AppDeploymentStatusReport do
  use Protobuf, syntax: :proto3, protoc_gen_elixir_version: "0.12.0"

  field :id, 1, type: :string
  field :sha, 2, type: :string
  field :revision_id, 3, type: :string, json_name: "revisionId"
  field :slug, 4, type: :string
  field :deployment_group_id, 5, type: :string, json_name: "deploymentGroupId"
  field :version, 6, type: :string
  field :status, 7, type: :string
end
