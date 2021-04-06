import Config

# Configure the type of names used for distribution and the hostname.
# Note that the name is randomly generate for each Livebook instance.
config :livebook, :node, {:shortnames, ""}
# config :livebook, :node, {:longnames, "127.0.0.1"}

if config_env() == :prod do
  # We don't need persistent session, so it's fine to just
  # generate a new key everytime the app starts
  secret_key_base = :crypto.strong_rand_bytes(48) |> Base.encode64()

  config :livebook, LivebookWeb.Endpoint, secret_key_base: secret_key_base
end
