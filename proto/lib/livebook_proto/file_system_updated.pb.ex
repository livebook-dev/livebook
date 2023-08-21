defmodule LivebookProto.FileSystemUpdated do
  use Protobuf, syntax: :proto3, protoc_gen_elixir_version: "0.12.0"

  field :name, 1, type: :string
  field :type, 2, type: :string
  field :value, 3, type: :string
end
