defmodule LivebookProto.Response do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  oneof :type, 0

  field :id, 1, type: :int32
  field :error, 2, type: LivebookProto.Error, oneof: 0
  field :changeset, 3, type: LivebookProto.ChangesetError, oneof: 0
  field :handshake, 4, type: LivebookProto.HandshakeResponse, oneof: 0

  field :create_secret, 5,
    type: LivebookProto.CreateSecretResponse,
    json_name: "createSecret",
    oneof: 0
end
