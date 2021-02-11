# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
#
# This configuration file is loaded before any dependency and
# is restricted to this project.

# General application configuration
use Mix.Config

# Configures the endpoint
config :live_book, LiveBookWeb.Endpoint,
  url: [host: "localhost"],
  secret_key_base: "9hHHeOiAA8wrivUfuS//jQMurHxoMYUtF788BQMx2KO7mYUE8rVrGGG09djBNQq7",
  render_errors: [view: LiveBookWeb.ErrorView, accepts: ~w(html json), layout: false],
  pubsub_server: LiveBook.PubSub,
  live_view: [signing_salt: "mAPgPEM4"]

# Configures Elixir's Logger
config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id]

# Use Jason for JSON parsing in Phoenix
config :phoenix, :json_library, Jason

# Configure the type of names used for distribution
# and the name of the main LiveBook node.
config :live_book, :node_name, {:shortnames, :live_book}
# config :live_book, :node_name, {:longnames, :"livebook@127.0.0.1"}

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
import_config "#{Mix.env()}.exs"
