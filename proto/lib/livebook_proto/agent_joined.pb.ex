defmodule LivebookProto.AgentJoined do
  use Protobuf, syntax: :proto3, protoc_gen_elixir_version: "0.13.0"

  field :agent, 1, type: LivebookProto.Agent
end
