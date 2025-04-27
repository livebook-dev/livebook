defmodule LivebookProto.AppDeploymentStarted do
  use Protobuf, protoc_gen_elixir_version: "0.14.1", syntax: :proto3

  field :app_deployment, 1, type: LivebookProto.AppDeployment, json_name: "appDeployment"
end
