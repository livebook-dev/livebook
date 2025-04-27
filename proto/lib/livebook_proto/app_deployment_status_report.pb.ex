defmodule LivebookProto.AppDeploymentStatusReport do
  use Protobuf, protoc_gen_elixir_version: "0.14.1", syntax: :proto3

  field :app_deployment_statuses, 1,
    repeated: true,
    type: LivebookProto.AppDeploymentStatus,
    json_name: "appDeploymentStatuses"
end
