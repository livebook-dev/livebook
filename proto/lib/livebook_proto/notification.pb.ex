defmodule LivebookProto.Notification do
  use Protobuf, full_name: "Notification", protoc_gen_elixir_version: "0.16.0", syntax: :proto3

  field :id, 1, type: :string
  field :message, 2, type: :string
  field :kind, 3, type: :string
end
