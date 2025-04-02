defmodule Mix.Tasks.Livebook.GenPriv do
  @moduledoc false

  use Mix.Task

  @gzippable_exts ~w(.js .css .txt .text .html .json .svg .eot .ttf)

  @impl true
  def run([]) do
    # Use absolute paths, instead of relying on the current mix project,
    # so the task can be invoked by nerves_livebook.
    app_path = Application.app_dir(:livebook)
    project_dir = Path.expand("../../..", __DIR__)

    compress_and_copy(Path.join(project_dir, "static"), Path.join(app_path, "priv/static"))

    compress_and_copy(
      Path.join(project_dir, "iframe/priv/static/iframe"),
      Path.join(app_path, "priv/iframe_static")
    )
  end

  defp compress_and_copy(source_dir, target_dir) do
    File.rm_rf!(target_dir)

    source_paths = Path.wildcard(Path.join(source_dir, "**/*"))

    for source_path <- source_paths, File.regular?(source_path) do
      target_path = Path.join(target_dir, Path.relative_to(source_path, source_dir))
      File.mkdir_p!(Path.dirname(target_path))

      if Path.extname(source_path) in @gzippable_exts do
        content = source_path |> File.read!() |> :zlib.gzip()
        File.write!(target_path <> ".gz", content)
      else
        File.cp!(source_path, target_path)
      end
    end

    Mix.shell().info("Generated #{target_dir} with compressed files from #{source_dir}")
  end
end
