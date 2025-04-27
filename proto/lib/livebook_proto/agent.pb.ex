defmodule LivebookProto.Agent do
  use Protobuf, protoc_gen_elixir_version: "0.14.1", syntax: :proto3

  field :id, 1, type: :string
  field :name, 2, type: :string
  field :org_id, 3, type: :string, json_name: "orgId"
  field :deployment_group_id, 4, type: :string, json_name: "deploymentGroupId"
end
