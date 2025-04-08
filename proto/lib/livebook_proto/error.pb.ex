defmodule LivebookProto.Error do
  use Protobuf, protoc_gen_elixir_version: "0.14.1", syntax: :proto3

  field :details, 1, type: :string
end
