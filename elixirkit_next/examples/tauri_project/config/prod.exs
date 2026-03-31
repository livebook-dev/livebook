import Config

config :example, ExampleWeb.Endpoint,
  cache_static_manifest: "priv/static/cache_manifest.json",
  check_origin: false

config :swoosh, api_client: Swoosh.ApiClient.Req

# Do not print debug messages in production
config :logger, level: :info

# Runtime production configuration, including reading
# of environment variables, is done on config/runtime.exs.
