defmodule LivebookProto.AppDeploymentStatusType do
  use Protobuf, enum: true, syntax: :proto3, protoc_gen_elixir_version: "0.12.0"

  field :connecting, 0
  field :preparing, 1
  field :available, 2
  field :deactivated, 4
end
