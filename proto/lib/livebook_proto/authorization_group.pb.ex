defmodule LivebookProto.AuthorizationGroup do
  use Protobuf, protoc_gen_elixir_version: "0.14.1", syntax: :proto3

  field :provider_id, 1, type: :string, json_name: "providerId"
  field :group_name, 2, type: :string, json_name: "groupName"
end
