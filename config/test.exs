import Config

# We don't run a server during test. If one is required,
# you can enable the server option below.
config :live_book, LiveBookWeb.Endpoint,
  http: [port: 4002],
  server: false

# Print only warnings and errors during test
config :logger, level: :warn

# Use a different node name for tests to avoid interfering
# with a running development node.
config :live_book, :node_name, {:shortnames, :live_book_test}
