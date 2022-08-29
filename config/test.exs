import Config

# We don't run a server during test. If one is required,
# you can enable the server option below.
config :livebook, LivebookWeb.Endpoint,
  http: [port: 4002],
  server: false

config :livebook, :iframe_port, 4003

# Print only warnings and errors during test
config :logger, level: :warning

# Disable authentication mode during test
config :livebook, :authentication_mode, :disabled

data_path = Path.expand("tmp/livebook_data/test")

# Clear data path for tests
if File.exists?(data_path) do
  File.rm_rf!(data_path)
end

config :livebook, :data_path, data_path

# Feature flags
config :livebook, :feature_flags,
  hub: true,
  localhost_hub: true

# Use longnames when running tests in CI, so that no host resolution is required,
# see https://github.com/livebook-dev/livebook/pull/173#issuecomment-819468549
if System.get_env("CI") == "true" do
  config :livebook, :node, {:longnames, :"livebook@127.0.0.1"}
end
