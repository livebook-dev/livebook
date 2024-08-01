[
  import_deps: [:phoenix, :ecto, :pluggable],
  plugins: [Phoenix.LiveView.HTMLFormatter],
  inputs: ["*.{heex,ex,exs}", "{config,lib,test}/**/*.{heex,ex,exs}", "rel/*/overlays/**/*.exs"]
]
