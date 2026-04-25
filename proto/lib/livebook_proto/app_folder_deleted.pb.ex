defmodule LivebookProto.AppFolderDeleted do
  use Protobuf,
    full_name: "AppFolderDeleted",
    protoc_gen_elixir_version: "0.16.0",
    syntax: :proto3

  field :id, 1, type: :string
end
