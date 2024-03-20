defmodule LivebookProto.AppDeployment do
  use Protobuf, syntax: :proto3, protoc_gen_elixir_version: "0.12.0"

  field :id, 1, type: :string
  field :title, 2, type: :string
  field :sha, 3, type: :string
  field :revision_id, 4, type: :string, json_name: "revisionId"
  field :slug, 5, type: :string
  field :deployment_group_id, 6, type: :string, json_name: "deploymentGroupId"
  field :deployed_by, 7, type: :string, json_name: "deployedBy"
  field :deployed_at, 8, type: :string, json_name: "deployedAt"
end
