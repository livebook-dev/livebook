defmodule LivebookProto.FieldError do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :field, 1, type: :string
  field :details, 2, repeated: true, type: :string
end
