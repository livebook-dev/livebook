import Config

if System.get_env("PHX_SERVER") do
  config :example, ExampleWeb.Endpoint, server: true
end

config :example, ExampleWeb.Endpoint,
  http: [ip: {127, 0, 0, 1}, port: String.to_integer(System.get_env("PORT", "4000"))]

if config_env() == :prod do
  secret_key_base =
    System.get_env("SECRET_KEY_BASE") ||
      raise "environment variable SECRET_KEY_BASE is missing"

  config :example, ExampleWeb.Endpoint,
    url: [host: "127.0.0.1"],
    secret_key_base: secret_key_base
end
