import Config

# We don't run a server during test. If one is required,
# you can enable the server option below.
config :livebook, LivebookWeb.Endpoint,
  http: [port: 4002],
  server: false

# Print only warnings and errors during test
config :logger, level: :warning

# Disable authentication in tests
config :livebook,
  authentication: :disabled,
  check_completion_data_interval: 300,
  iframe_port: 4003

data_path = Path.expand("tmp/livebook_data/test")

# Clear data path for tests
if File.exists?(data_path) do
  File.rm_rf!(data_path)
end

config :livebook,
  data_path: data_path,
  agent_name: "chonky-cat"

config :livebook, Livebook.Apps.Manager, retry_backoff_base_ms: 0
