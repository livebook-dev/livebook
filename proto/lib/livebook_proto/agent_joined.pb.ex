defmodule LivebookProto.AgentJoined do
  use Protobuf, protoc_gen_elixir_version: "0.14.1", syntax: :proto3

  field :agent, 1, type: LivebookProto.Agent
end
