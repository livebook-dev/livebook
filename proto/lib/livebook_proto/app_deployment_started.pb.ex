defmodule LivebookProto.AppDeploymentStarted do
  use Protobuf,
    full_name: "AppDeploymentStarted",
    protoc_gen_elixir_version: "0.16.0",
    syntax: :proto3

  field :app_deployment, 1, type: LivebookProto.AppDeployment, json_name: "appDeployment"
end
