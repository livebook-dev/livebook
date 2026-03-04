defmodule LivebookProto.DeploymentStatusUpdated do
  use Protobuf,
    full_name: "DeploymentStatusUpdated",
    protoc_gen_elixir_version: "0.16.0",
    syntax: :proto3

  field :id, 1, type: :string
  field :deployed_apps_counter, 2, type: :int32, json_name: "deployedAppsCounter"
end
