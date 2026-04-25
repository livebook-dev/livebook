defmodule LivebookProto.BillingStatusTrialing do
  use Protobuf,
    full_name: "BillingStatusTrialing",
    protoc_gen_elixir_version: "0.16.0",
    syntax: :proto3

  field :trial_ends_at, 1, type: :int64, json_name: "trialEndsAt"
end
