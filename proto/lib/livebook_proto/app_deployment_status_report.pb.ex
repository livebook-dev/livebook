defmodule LivebookProto.AppDeploymentStatusReport do
  use Protobuf,
    full_name: "AppDeploymentStatusReport",
    protoc_gen_elixir_version: "0.16.0",
    syntax: :proto3

  field :app_deployment_statuses, 1,
    repeated: true,
    type: LivebookProto.AppDeploymentStatus,
    json_name: "appDeploymentStatuses"
end
