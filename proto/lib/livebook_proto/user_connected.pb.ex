defmodule LivebookProto.UserConnected do
  use Protobuf, syntax: :proto3, protoc_gen_elixir_version: "0.12.0"

  field :name, 1, type: :string
  field :secrets, 2, repeated: true, type: LivebookProto.Secret
end
