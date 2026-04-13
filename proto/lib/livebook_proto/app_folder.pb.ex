defmodule LivebookProto.AppFolder do
  use Protobuf, full_name: "AppFolder", protoc_gen_elixir_version: "0.16.0", syntax: :proto3

  field :id, 1, type: :string
  field :name, 2, type: :string
end
