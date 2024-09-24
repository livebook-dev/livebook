defmodule LivebookProto.DeploymentGroupUpdated do
  use Protobuf, syntax: :proto3, protoc_gen_elixir_version: "0.12.0"

  field :id, 1, type: :string
  field :name, 2, type: :string
  field :secrets, 3, repeated: true, type: LivebookProto.DeploymentGroupSecret
  field :clustering, 4, type: :string
  field :zta_provider, 5, type: :string, json_name: "ztaProvider"
  field :zta_key, 6, type: :string, json_name: "ztaKey"
  field :agent_keys, 7, repeated: true, type: LivebookProto.AgentKey, json_name: "agentKeys"
  field :url, 8, type: :string
end
