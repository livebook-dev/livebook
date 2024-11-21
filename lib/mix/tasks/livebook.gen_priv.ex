defmodule Mix.Tasks.Livebook.GenPriv do
  @moduledoc false

  # Note that we need to include priv/.gitkeep in Dockerfile and Hex
  # package files, so that priv/ is symlinked within _build/, before
  # we generate the actual files.

  use Mix.Task

  @gzippable_exts ~w(.js .css .txt .text .html .json .svg .eot .ttf)

  @impl true
  def run([]) do
    compress_and_copy("static", "priv/static")
    compress_and_copy("iframe/priv/static/iframe", "priv/iframe_static")
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
