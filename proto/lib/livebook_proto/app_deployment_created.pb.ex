defmodule LivebookProto.AppDeploymentCreated do
  use Protobuf, syntax: :proto3, protoc_gen_elixir_version: "0.12.0"

  field :id, 1, type: :string
  field :title, 2, type: :string
  field :sha, 3, type: :string
  field :archive_url, 4, type: :string, json_name: "archiveUrl"
  field :app_id, 5, type: :string, json_name: "appId"
  field :slug, 6, type: :string
  field :deployment_group_id, 7, type: :string, json_name: "deploymentGroupId"
  field :deployed_by, 8, type: :string, json_name: "deployedBy"
  field :deployed_at, 9, type: :string, json_name: "deployedAt"
end
