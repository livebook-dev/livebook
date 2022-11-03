defmodule LivebookProto.Response do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  oneof :type, 0

  field :error, 1, type: LivebookProto.Error, oneof: 0
  field :session, 2, type: LivebookProto.SessionResponse, oneof: 0
end
