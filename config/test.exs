import Config

# We don't run a server during test. If one is required,
# you can enable the server option below.
config :livebook, LivebookWeb.Endpoint,
  http: [port: 4002],
  server: false

# Print only warnings and errors during test
config :logger, level: :warn

# Disable authentication mode during test
config :livebook, :authentication_mode, :disabled

# Use the embedded runtime in tests by default, so they
# are cheaper to run. Other runtimes can be tested by starting
# and setting them explicitly
config :livebook, :default_runtime, {Livebook.Runtime.Embedded, []}

# Use longnames when running tests in CI, so that no host resolution is required,
# see https://github.com/livebook-dev/livebook/pull/173#issuecomment-819468549
if System.get_env("CI") == "true" do
  config :livebook, :node, {:longnames, :"livebook@127.0.0.1"}
end
