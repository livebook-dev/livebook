defmodule LivebookProto.HandshakeRequest do
  @moduledoc false
  use Protobuf, protoc_gen_elixir_version: "0.11.0", syntax: :proto3

  field :app_version, 1, type: :string, json_name: "appVersion"
end
