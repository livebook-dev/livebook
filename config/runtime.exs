import Config
Livebook.config_runtime()

if config_env() == :dev do
  if tidewave_client_url = System.get_env("TIDEWAVE_CLIENT_URL") do
    config :tidewave, client_url: tidewave_client_url
  end
end
