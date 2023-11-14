import Config

# Configures the endpoint
config :livebook, LivebookWeb.Endpoint,
  adapter: Bandit.PhoenixAdapter,
  url: [host: "localhost", path: "/"],
  pubsub_server: Livebook.PubSub,
  live_view: [signing_salt: "livebook"],
  drainer: [shutdown: 1000],
  render_errors: [formats: [html: LivebookWeb.ErrorHTML], layout: false]

# Configures Elixir's Logger
config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id]

# Use Jason for JSON parsing in Phoenix
config :phoenix, :json_library, Jason

# Additional mime types
config :mime, :types, %{
  "audio/m4a" => ["m4a"],
  "text/plain" => ["livemd"]
}

config :livebook,
  teams_url: "https://teams.livebook.dev",
  app_service_name: nil,
  app_service_url: nil,
  authentication_mode: :token,
  feature_flags: [],
  force_ssl_host: nil,
  learn_notebooks: [],
  plugs: [],
  shutdown_callback: nil,
  update_instructions_url: nil,
  within_iframe: false,
  allowed_uri_schemes: []

config :aws_credentials, fail_if_unavailable: false

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
import_config "#{Mix.env()}.exs"
