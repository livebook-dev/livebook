defmodule LivebookProto.BillingStatusCanceling do
  use Protobuf, protoc_gen_elixir_version: "0.14.1", syntax: :proto3

  field :cancel_at, 1, type: :int64, json_name: "cancelAt"
end
