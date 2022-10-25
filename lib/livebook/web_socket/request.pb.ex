defmodule Livebook.WebSocket.Request do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  oneof :type, 0

  field :session, 1, type: Livebook.WebSocket.SessionRequest, oneof: 0
end
