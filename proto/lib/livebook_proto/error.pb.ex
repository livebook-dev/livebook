defmodule LivebookProto.Error do
  use Protobuf, syntax: :proto3, protoc_gen_elixir_version: "0.13.0"

  field :details, 1, type: :string
end
