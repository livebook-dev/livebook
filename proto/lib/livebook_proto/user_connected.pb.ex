defmodule LivebookProto.UserConnected do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :name, 1, type: :string
  field :secrets, 2, repeated: true, type: LivebookProto.Secret
end
