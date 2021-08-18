import Config
require Logger

config :livebook, LivebookWeb.Endpoint,
  secret_key_base:
    Livebook.Config.secret!("LIVEBOOK_SECRET_KEY_BASE") ||
      Base.encode64(:crypto.strong_rand_bytes(48))

if password = Livebook.Config.password!("LIVEBOOK_PASSWORD") do
  config :livebook, authentication_mode: :password, password: password
else
  config :livebook, token: Livebook.Utils.random_id()
end

if port = Livebook.Config.port!("LIVEBOOK_PORT") do
  config :livebook, LivebookWeb.Endpoint, http: [port: port]
end

if ip = Livebook.Config.ip!("LIVEBOOK_IP") do
  config :livebook, LivebookWeb.Endpoint, http: [ip: ip]
end

config :livebook,
       :cookie,
       Livebook.Config.cookie!("LIVEBOOK_COOKIE") ||
         Livebook.Config.cookie!("RELEASE_COOKIE") ||
         Livebook.Utils.random_cookie()

config :livebook,
       :default_runtime,
       Livebook.Config.default_runtime!("LIVEBOOK_DEFAULT_RUNTIME") ||
         {Livebook.Runtime.ElixirStandalone, []}

root_path =
  Livebook.Config.root_path!("LIVEBOOK_ROOT_PATH")
  |> Livebook.FileSystem.Utils.ensure_dir_path()

local_file_system = Livebook.FileSystem.Local.new(default_path: root_path)
configured_file_systems = Livebook.Config.file_systems!("LIVEBOOK_FILE_SYSTEM_")

config :livebook, :file_systems, [local_file_system | configured_file_systems]
