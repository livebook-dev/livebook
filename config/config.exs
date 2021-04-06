import Config

# Configures the endpoint
config :livebook, LivebookWeb.Endpoint,
  url: [host: "localhost"],
  secret_key_base: "9hHHeOiAA8wrivUfuS//jQMurHxoMYUtF788BQMx2KO7mYUE8rVrGGG09djBNQq7",
  pubsub_server: Livebook.PubSub,
  live_view: [signing_salt: "mAPgPEM4"]

# Configures Elixir's Logger
config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id]

# Use Jason for JSON parsing in Phoenix
config :phoenix, :json_library, Jason

# Configure the type of names used for distribution and the hostname.
# A unique node base name is generated for each Livebook instance.
config :livebook, :node, {:shortnames, ""}
# config :livebook, :node, {:longnames, "127.0.0.1"}

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
import_config "#{Mix.env()}.exs"
