defmodule LivebookProto.AppDeploymentReport do
  use Protobuf, syntax: :proto3, protoc_gen_elixir_version: "0.12.0"

  field :app_deployment_statuses, 1,
    repeated: true,
    type: LivebookProto.AppDeploymentStatusReport,
    json_name: "appDeploymentStatuses"
end
