defmodule LivebookProto.Event do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  oneof :type, 0

  field :secret_created, 1,
    type: LivebookProto.SecretCreated,
    json_name: "secretCreated",
    oneof: 0

  field :secret_updated, 2,
    type: LivebookProto.SecretUpdated,
    json_name: "secretUpdated",
    oneof: 0

  field :secret_deleted, 3,
    type: LivebookProto.SecretDeleted,
    json_name: "secretDeleted",
    oneof: 0

  field :user_connected, 4,
    type: LivebookProto.UserConnected,
    json_name: "userConnected",
    oneof: 0
end
