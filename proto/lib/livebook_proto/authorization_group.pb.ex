defmodule LivebookProto.AuthorizationGroup do
  use Protobuf, protoc_gen_elixir_version: "0.14.1", syntax: :proto3

  field :oidc_provider_id, 1, type: :string, json_name: "oidcProviderId"
  field :group_name, 2, type: :string, json_name: "groupName"
end
