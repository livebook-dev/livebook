defmodule LivebookProto.FileSystem do
  use Protobuf, syntax: :proto3, protoc_gen_elixir_version: "0.13.0"

  field :id, 1, type: :string
  field :name, 2, type: :string
  field :type, 3, type: :string
  field :value, 4, type: :string
end
