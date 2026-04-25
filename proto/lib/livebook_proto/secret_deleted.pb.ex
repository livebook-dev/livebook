defmodule LivebookProto.SecretDeleted do
  use Protobuf, full_name: "SecretDeleted", protoc_gen_elixir_version: "0.16.0", syntax: :proto3

  field :name, 1, type: :string
end
