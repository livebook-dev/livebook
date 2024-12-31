defmodule LivebookProto.DeploymentGroupCreated do
  use Protobuf, syntax: :proto3, protoc_gen_elixir_version: "0.13.0"

  field :id, 1, type: :string
  field :name, 2, type: :string
  field :mode, 3, type: :string
  field :clustering, 5, type: :string
  field :zta_provider, 6, type: :string, json_name: "ztaProvider", deprecated: true
  field :zta_key, 7, type: :string, json_name: "ztaKey", deprecated: true
  field :agent_keys, 8, repeated: true, type: LivebookProto.AgentKey, json_name: "agentKeys"
  field :url, 9, type: :string
  field :teams_auth, 10, type: :bool, json_name: "teamsAuth"
end
