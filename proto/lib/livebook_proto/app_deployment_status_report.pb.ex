defmodule LivebookProto.AppDeploymentStatusReport do
  use Protobuf, syntax: :proto3, protoc_gen_elixir_version: "0.13.0"

  field :app_deployment_statuses, 1,
    repeated: true,
    type: LivebookProto.AppDeploymentStatus,
    json_name: "appDeploymentStatuses"
end
