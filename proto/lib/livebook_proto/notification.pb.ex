defmodule LivebookProto.Notification do
  use Protobuf, full_name: "Notification", protoc_gen_elixir_version: "0.16.0", syntax: :proto3

  field :id, 1, type: :string
  field :message, 2, type: :string
  field :kind, 3, type: :string
  field :type, 4, type: :string
  field :min_version, 5, type: :string, json_name: "minVersion"
end
