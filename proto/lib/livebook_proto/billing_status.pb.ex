defmodule LivebookProto.BillingStatus do
  use Protobuf, protoc_gen_elixir_version: "0.13.0", syntax: :proto3

  oneof :type, 0

  field :disabled, 1, type: :bool
  field :trialing, 2, type: LivebookProto.BillingStatusTrialing, oneof: 0

  field :trial_ended, 3,
    type: LivebookProto.BillingStatusTrialEnded,
    json_name: "trialEnded",
    oneof: 0

  field :canceling, 4, type: LivebookProto.BillingStatusCanceling, oneof: 0
  field :canceled, 5, type: LivebookProto.BillingStatusCanceled, oneof: 0
end
