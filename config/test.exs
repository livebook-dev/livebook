import Config

# We don't run a server during test. If one is required,
# you can enable the server option below.
config :livebook, LivebookWeb.Endpoint,
  http: [port: 4002],
  server: false

# Print only warnings and errors during test.
#
# We configure the default handler instead of Logger on purpose,
# as we want all calls to be processed, especially when using
# the JSON formatter.
config :logger, :default_handler, level: :warning

# Also configure the JSON formatter for test.
# We make sure we write all currently available metadata.
path = "tmp/test.log.json"
File.rm(path)

config :livebook, :logger, [
  {:handler, :json_log, :logger_std_h,
   %{
     config: %{file: ~c"#{path}"},
     formatter: {LoggerJSON.Formatters.Basic, %{metadata: [:users, :request_id]}}
   }}
]

# Disable authentication in tests
config :livebook,
  authentication: :disabled,
  check_completion_data_interval: 300,
  iframe_port: 4003

config :livebook,
  data_path: Path.expand("tmp/livebook_data/test"),
  agent_name: "chonky-cat",
  k8s_kubeconfig_pipeline:
    {Kubereq.Kubeconfig.Stub,
     plugs: %{
       "default" => {Req.Test, :k8s_cluster},
       "no-permission" => {Req.Test, :k8s_cluster}
     }}

config :livebook, Livebook.Apps.Manager, retry_backoff_base_ms: 0
