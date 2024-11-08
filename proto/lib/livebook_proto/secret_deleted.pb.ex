defmodule LivebookProto.SecretDeleted do
  use Protobuf, syntax: :proto3, protoc_gen_elixir_version: "0.13.0"

  field :name, 1, type: :string
end
