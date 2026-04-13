defmodule LivebookProto.AppFolderUpdated do
  use Protobuf,
    full_name: "AppFolderUpdated",
    protoc_gen_elixir_version: "0.16.0",
    syntax: :proto3

  field :app_folder, 1, type: LivebookProto.AppFolder, json_name: "appFolder"
end
