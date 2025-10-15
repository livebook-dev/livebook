defmodule LivebookProto.AppFolderCreated do
  use Protobuf, protoc_gen_elixir_version: "0.14.1", syntax: :proto3

  field :app_folder, 1, type: LivebookProto.AppFolder, json_name: "appFolder"
end
