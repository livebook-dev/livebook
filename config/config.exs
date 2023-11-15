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

# TODO keen for some advice how best to structure this configuration
# feels a bit jank
config :livebook, Livebook.Copilot,
  enabled: true,
  # backend: Livebook.Copilot.HuggingfaceBackend,
  # backend_config: %{
  #   model: "deepseek-coder-1.3b"
  # }

  backend: Livebook.Copilot.DummyBackend,
  backend_config: %{
    model: "echo"
  }

# backend: Livebook.Copilot.BumblebeeBackend,
# backend_config: %{
#   model: "gpt2"
# }

# backend: Livebook.Copilot.LlamaCppHttpBackend,
# backend_config: %{
#   model: "codellama-7b"
# }

# backend: Livebook.Copilot.Openai,
# backend_config: %{
#   api_key: System.get_env("OPENAI_API_KEY"),
#   model: "gpt-4-1106-preview"
# }

config :nx,
  default_backend: EXLA.Backend,
  client: :host

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
import_config "#{Mix.env()}.exs"
