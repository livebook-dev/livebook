defmodule LivebookProto.ChangesetError do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :errors, 1, repeated: true, type: LivebookProto.FieldError
end
