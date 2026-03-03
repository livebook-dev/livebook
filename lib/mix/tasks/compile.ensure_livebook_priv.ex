defmodule Mix.Tasks.Compile.EnsureLivebookPriv do
  @moduledoc false

  use Mix.Task

  @impl true
  def run(_args) do
    # Livebook may be installed as a depepdency. This is the case in
    # nerves_livebook, but also when using escript.install. In those
    # cases assets.setup is not called, so we use this custom compiler
    # to invoke the assets build and make those work transparently.
    #
    # Note that this is not relevant for escript.install from Hex,
    # because we publish the package with priv/ already built and
    # without the source assets. However, it is relevant when
    # installing escript from GitHub.

    # Use an absolute path, instead of relying on the current mix
    # project, since livebook may be a dependency.
    project_dir = Path.expand("../../..", __DIR__)
    priv_static_dir = Path.join(project_dir, "priv/static")

    if Mix.env() == :prod and not File.exists?(priv_static_dir) do
      # If livebook is a dependency, it doesn't get it's own deps/
      # dir, instead it is a sibling in the deps folder. We rely on
      # deps/ dir being available in assets/package.json. To make it
      # work, we symlink the deps dir. If the deps dir is present,
      # this simply fails.
      _ = File.ln_s(Mix.Project.deps_path(), Path.join(project_dir, "deps"))

      Mix.shell().info("Generating priv/static")

      Mix.Task.run("bun.install", ~w"--if-missing")
      Mix.Task.run("bun", ~w"assets install")
      Mix.Task.run("bun", ~w" assets run build")
    end

    :ok
  end
end
