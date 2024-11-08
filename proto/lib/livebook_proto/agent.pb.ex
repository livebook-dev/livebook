defmodule LivebookProto.Agent do
  use Protobuf, syntax: :proto3, protoc_gen_elixir_version: "0.13.0"

  field :id, 1, type: :string
  field :name, 2, type: :string
  field :org_id, 3, type: :string, json_name: "orgId"
  field :deployment_group_id, 4, type: :string, json_name: "deploymentGroupId"
end
