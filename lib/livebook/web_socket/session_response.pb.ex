defmodule Livebook.WebSocket.SessionResponse do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :id, 1, type: :string
  field :user, 2, type: Livebook.WebSocket.User
end