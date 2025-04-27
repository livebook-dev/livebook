defmodule LivebookProto.AppDeploymentStatusType do
  @moduledoc """
  We're only using Enum in this case because
  the Client is the source of the information,
  and the Server will always be up-to-date.

  Otherwise, it shouldn't be used.
  """

  use Protobuf, enum: true, protoc_gen_elixir_version: "0.14.1", syntax: :proto3

  field :preparing, 0
  field :available, 1
end
