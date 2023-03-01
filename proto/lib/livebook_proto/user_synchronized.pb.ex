defmodule LivebookProto.UserSynchronized do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :id, 1, type: :string
  field :name, 2, type: :string
  field :secrets, 3, repeated: true, type: LivebookProto.Secret
end
