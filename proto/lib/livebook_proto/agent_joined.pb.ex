defmodule LivebookProto.AgentJoined do
  use Protobuf, full_name: "AgentJoined", protoc_gen_elixir_version: "0.16.0", syntax: :proto3

  field :agent, 1, type: LivebookProto.Agent
end
