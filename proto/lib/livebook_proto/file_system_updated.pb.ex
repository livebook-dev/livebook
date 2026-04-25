defmodule LivebookProto.FileSystemUpdated do
  use Protobuf,
    full_name: "FileSystemUpdated",
    protoc_gen_elixir_version: "0.16.0",
    syntax: :proto3

  field :id, 1, type: :string
  field :name, 2, type: :string
  field :type, 3, type: :string
  field :value, 4, type: :string
end
