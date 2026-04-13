defmodule LivebookProto.FileSystemDeleted do
  use Protobuf,
    full_name: "FileSystemDeleted",
    protoc_gen_elixir_version: "0.16.0",
    syntax: :proto3

  field :id, 1, type: :string
end
