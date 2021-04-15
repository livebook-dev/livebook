import Config

# Configure the type of names used for distribution and the node name.
# By default a random short name is used.
# config :livebook, :node, {:shortnames, "livebook"}
# config :livebook, :node, {:longnames, :"livebook@127.0.0.1"}

if password = System.get_env("LIVEBOOK_PASSWORD") do
  config :livebook,
    authentication_mode: :password,
    password: password
end

if config_env() == :prod do
  # In order to persist sessions between deployments (desirable when using password authentication mode)
  # allow to customize secret_key_base. Otherwise the secret will change every time app starts.
  secret_key_base =
    if secret = System.get_env("SECRET_KEY_BASE") do
      secret
    else
      :crypto.strong_rand_bytes(48) |> Base.encode64()
    end

  config :livebook, LivebookWeb.Endpoint, secret_key_base: secret_key_base
end
