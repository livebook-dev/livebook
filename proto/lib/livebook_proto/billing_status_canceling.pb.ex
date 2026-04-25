defmodule LivebookProto.BillingStatusCanceling do
  use Protobuf,
    full_name: "BillingStatusCanceling",
    protoc_gen_elixir_version: "0.16.0",
    syntax: :proto3

  field :cancel_at, 1, type: :int64, json_name: "cancelAt"
end
