defmodule LivebookProto.DeploymentUser do
  use Protobuf, full_name: "DeploymentUser", protoc_gen_elixir_version: "0.16.0", syntax: :proto3

  field :user_id, 1, type: :string, json_name: "userId"
  field :deployment_group_id, 2, type: :string, json_name: "deploymentGroupId"
end
