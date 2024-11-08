defmodule LivebookProto.AgentKey do
  use Protobuf, syntax: :proto3, protoc_gen_elixir_version: "0.13.0"

  field :id, 1, type: :string
  field :key, 2, type: :string
  field :deployment_group_id, 3, type: :string, json_name: "deploymentGroupId"
end
