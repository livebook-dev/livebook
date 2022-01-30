import Config

# Configures the endpoint
config :livebook, LivebookWeb.Endpoint,
  url: [host: "localhost"],
  pubsub_server: Livebook.PubSub,
  live_view: [signing_salt: "livebook"]

# Configures Elixir's Logger
config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id]

# Use Jason for JSON parsing in Phoenix
config :phoenix, :json_library, Jason

# Add mime type to upload notebooks with `Phoenix.LiveView.Upload`
config :mime, :types, %{
  "text/plain" => ["livemd"]
}

# Sets the default storage backend
config :livebook, :storage, Livebook.Storage.Ets

# Sets the default authentication mode to token
config :livebook, :authentication_mode, :token

# Sets the default runtime to ElixirStandalone.
# This is the desired default most of the time,
# but in some specific use cases you may want
# to configure that to the Embedded or Mix runtime instead.
# Also make sure the configured runtime has
# a synchronous `init` function that takes the
# configured arguments.
config :livebook, :default_runtime, {Livebook.Runtime.ElixirStandalone, []}

# A list of custom plugs in the following format:
#
#    [{plug_module :: module(), opts :: keyword()}]
#
# The plugs are called directly before the Livebook router.
config :livebook, :plugs, []

# A list of additional notebooks to include in the Explore sections.
#
# Note that the notebooks are loaded and embedded in a compiled module,
# so the paths are accessed at compile time only.
#
# ## Example
#
#     config :livebook, :explore_notebooks, [
#       %{
#         # Required notebook path
#         path: "/path/to/notebook.livemd",
#         # Optional notebook identifier for URLs, as in /explore/notebooks/{slug}
#         # By default the slug is inferred from file name, so there is no need to set it
#         slug: "my-notebook"
#         # Optional list of images
#         image_paths: [
#           # This image can be sourced as images/myimage.jpg in the notebook
#           "/path/to/myimage.jpg"
#         ],
#         # Optional details for the notebook card. If omitted, the notebook
#         # is hidden in the UI, but still accessible under /explore/notebooks/{slug}
#         details: %{
#           cover_path: "/path/to/logo.png",
#           description: "My custom notebook that showcases some amazing stuff."
#         }
#       },
#       %{
#         path: "/path/to/other_notebook.livemd"
#       }
#     ]
#
config :livebook, :explore_notebooks, []

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
import_config "#{Mix.env()}.exs"
