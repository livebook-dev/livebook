defmodule LivebookProto.BillingStatusTrialEnded do
  use Protobuf,
    full_name: "BillingStatusTrialEnded",
    protoc_gen_elixir_version: "0.16.0",
    syntax: :proto3

  field :trial_ends_at, 1, type: :int64, json_name: "trialEndsAt"
end
