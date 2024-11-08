defmodule LivebookProto.AppDeploymentStatusType do
  use Protobuf, enum: true, syntax: :proto3, protoc_gen_elixir_version: "0.13.0"

  field :preparing, 0
  field :available, 1
end
