defmodule LivebookProto.DeploymentGroupUpdated do
  use Protobuf, syntax: :proto3, protoc_gen_elixir_version: "0.13.0"

  field :id, 1, type: :string
  field :name, 2, type: :string
  field :secrets, 3, repeated: true, type: LivebookProto.DeploymentGroupSecret
  field :clustering, 4, type: :string
  field :zta_provider, 5, type: :string, json_name: "ztaProvider", deprecated: true
  field :zta_key, 6, type: :string, json_name: "ztaKey", deprecated: true
  field :agent_keys, 7, repeated: true, type: LivebookProto.AgentKey, json_name: "agentKeys"
  field :url, 8, type: :string

  field :environment_variables, 9,
    repeated: true,
    type: LivebookProto.EnvironmentVariable,
    json_name: "environmentVariables"

  field :teams_auth, 10, type: :bool, json_name: "teamsAuth"
end
