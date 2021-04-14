import Config

# Configure the type of names used for distribution and the node name.
# By default a random short name is used.
# config :livebook, :node, {:shortnames, "livebook"}
# config :livebook, :node, {:longnames, :"livebook@127.0.0.1"}

if System.get_env("USE_PASSWORD") do
  config :livebook, password_authentication: true, password: System.fetch_env!("AUTH_PASSWORD")
else
  config :livebook, password_authentication: false
end

if config_env() == :prod do
  # We don't need persistent session, so it's fine to just
  # generate a new key everytime the app starts
  secret_key_base = :crypto.strong_rand_bytes(48) |> Base.encode64()

  config :livebook, LivebookWeb.Endpoint, secret_key_base: secret_key_base
end
