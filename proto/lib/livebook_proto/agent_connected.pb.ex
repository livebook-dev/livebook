defmodule LivebookProto.AgentConnected do
  use Protobuf, syntax: :proto3, protoc_gen_elixir_version: "0.12.0"

  field :id, 1, type: :int32
  field :name, 2, type: :string
  field :public_key, 3, type: :string, json_name: "publicKey"
  field :deployment_group, 4, type: LivebookProto.DeploymentGroup, json_name: "deploymentGroup"
  field :secrets, 5, repeated: true, type: LivebookProto.Secret
  field :file_systems, 6, repeated: true, type: LivebookProto.FileSystem, json_name: "fileSystems"
end
