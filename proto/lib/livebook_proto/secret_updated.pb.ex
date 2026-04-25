defmodule LivebookProto.SecretUpdated do
  use Protobuf, full_name: "SecretUpdated", protoc_gen_elixir_version: "0.16.0", syntax: :proto3

  field :name, 1, type: :string
  field :value, 2, type: :string
end
