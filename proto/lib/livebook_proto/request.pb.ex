defmodule LivebookProto.Request do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  oneof :type, 0

  field :id, 1, type: :int32
  field :session, 2, type: LivebookProto.SessionRequest, oneof: 0
end
