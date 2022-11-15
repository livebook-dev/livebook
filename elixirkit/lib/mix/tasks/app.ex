defmodule Mix.Tasks.App do
  use Mix.Task

  def run([]) do
    Mix.Task.run("app.build", [])
    ElixirKit.Bundler.open_release_app()
  end
end
