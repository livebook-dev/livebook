defmodule Mix.Tasks.Compile.EnsureLivebookPriv do
  @moduledoc false

  use Mix.Task

  @impl true
  def run(_args) do
    # Use an absolute path, instead of relying on the current mix
    # project, so the task can be invoked by nerves_livebook.
    project_dir = Path.expand("../../..", __DIR__)
    priv_static_dir = Path.join(project_dir, "priv/static")

    # In case Livebook is used as a library as in nerves_livebook,
    # the assets.setup is not called, so we use this custom compiler
    # to invoke the task and make the assets work transparently.
    if not File.exists?(priv_static_dir) do
      Mix.shell().info("Generating priv/static")

      :ok = Mix.Task.run("assets.setup")
    end

    :ok
  end
end
