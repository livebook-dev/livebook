defmodule LivebookProto.FileSystem do
  use Protobuf, protoc_gen_elixir_version: "0.14.1", syntax: :proto3

  field :id, 1, type: :string
  field :name, 2, type: :string
  field :type, 3, type: :string
  field :value, 4, type: :string
end
