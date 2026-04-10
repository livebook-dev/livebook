import Config

if config_env() == :dev do
  config :makeup_syntect,
    register_for_languages: [
      "diff",
      "rust",
      "xml",
      "yaml"
    ]
end
