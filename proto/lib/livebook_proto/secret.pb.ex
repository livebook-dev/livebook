defmodule LivebookProto.Secret do
  use Protobuf, full_name: "Secret", protoc_gen_elixir_version: "0.16.0", syntax: :proto3

  field :name, 1, type: :string
  field :value, 2, type: :string
end
