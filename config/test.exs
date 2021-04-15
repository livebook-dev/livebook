import Config

# We don't run a server during test. If one is required,
# you can enable the server option below.
config :livebook, LivebookWeb.Endpoint,
  http: [port: 4002],
  server: false

# Print only warnings and errors during test
config :logger, level: :warn

config :livebook, :authentication_mode, :disabled

# Use longnames when running tests in CI, so that no host resolution is required,
# see https://github.com/elixir-nx/livebook/pull/173#issuecomment-819468549
if System.get_env("CI") == "true" do
  config :livebook, :node, {:longnames, :"livebook@127.0.0.1"}
end
