defmodule LivebookProto.AuthorizationGroup do
  use Protobuf,
    full_name: "AuthorizationGroup",
    protoc_gen_elixir_version: "0.16.0",
    syntax: :proto3

  field :provider_id, 1, type: :string, json_name: "providerId"
  field :group_name, 2, type: :string, json_name: "groupName"
  field :app_folder_id, 3, type: :string, json_name: "appFolderId"
end
