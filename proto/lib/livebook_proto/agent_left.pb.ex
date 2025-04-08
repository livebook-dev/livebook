defmodule LivebookProto.AgentLeft do
  use Protobuf, protoc_gen_elixir_version: "0.14.1", syntax: :proto3

  field :id, 1, type: :string
end
