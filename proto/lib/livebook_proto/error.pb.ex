defmodule LivebookProto.Error do
  use Protobuf, full_name: "Error", protoc_gen_elixir_version: "0.16.0", syntax: :proto3

  field :details, 1, type: :string
end
