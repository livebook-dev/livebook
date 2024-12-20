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
  format: "$date $time $metadata[$level] $message\n",
  metadata: [:request_id]

config :phoenix, :json_library, JSON

# Additional mime types
config :mime, :types, %{
  "audio/m4a" => ["m4a"],
  "text/plain" => ["livemd"]
}

config :livebook,
  agent_name: "default",
  allowed_uri_schemes: [],
  app_service_name: nil,
  app_service_url: nil,
  authentication: :token,
  aws_credentials: false,
  feature_flags: [],
  force_ssl_host: nil,
  learn_notebooks: [],
  plugs: [],
  rewrite_on: [],
  shutdown_callback: nil,
  teams_auth?: false,
  teams_url: "https://teams.livebook.dev",
  github_release_info: %{repo: "livebook-dev/livebook", version: Mix.Project.config()[:version]},
  update_instructions_url: nil,
  within_iframe: false,
  k8s_kubeconfig_pipeline: Kubereq.Kubeconfig.Default

config :livebook, Livebook.Apps.Manager, retry_backoff_base_ms: 5_000

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
import_config "#{Mix.env()}.exs"
