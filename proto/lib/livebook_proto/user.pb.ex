defmodule LivebookProto.User do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :id, 1, type: :int32
  field :email, 2, type: :string
end
