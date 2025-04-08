defmodule LivebookProto.DeploymentGroupSecret do
  use Protobuf, protoc_gen_elixir_version: "0.14.1", syntax: :proto3

  field :name, 1, type: :string
  field :value, 2, type: :string
  field :deployment_group_id, 3, type: :string, json_name: "deploymentGroupId"
end
