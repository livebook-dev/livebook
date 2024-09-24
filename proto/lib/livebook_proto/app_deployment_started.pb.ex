defmodule LivebookProto.AppDeploymentStarted do
  use Protobuf, syntax: :proto3, protoc_gen_elixir_version: "0.12.0"

  field :app_deployment, 1, type: LivebookProto.AppDeployment, json_name: "appDeployment"
end
