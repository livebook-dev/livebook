defmodule Mix.Tasks.Compile.LivebookPriv do
  @moduledoc false

  use Mix.Task

  @gzippable_exts ~w(.js .css .txt .text .html .json .svg .eot .ttf)

  @impl true
  def run(_args) do
    app_path = Mix.Project.app_path()
    manifest_path = Path.join(app_path, "compile.livebook_priv")

    prev_mtime =
      case File.read(manifest_path) do
        {:ok, binary} -> :erlang.binary_to_term(binary)
        {:error, _error} -> nil
      end

    mtime1 =
      compress_and_copy(
        "static",
        Path.join(app_path, "priv/static"),
        prev_mtime
      )

    mtime2 =
      compress_and_copy(
        "iframe/priv/static/iframe",
        Path.join(app_path, "priv/iframe_static"),
        prev_mtime
      )

    mtime = max(mtime1, mtime2)
    File.write!(manifest_path, :erlang.term_to_binary(mtime))
    :ok
  end

  defp compress_and_copy(source_dir, target_dir, prev_mtime) do
    source_paths = Path.wildcard(Path.join(source_dir, "**/*"))
    mtime = paths_mtime(source_paths)

    changed? = prev_mtime == nil or mtime > prev_mtime

    if changed? do
      File.rm_rf!(target_dir)

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

    mtime
  end

  defp paths_mtime(paths) do
    paths
    |> Enum.map(fn path -> File.stat!(path).mtime end)
    |> Enum.max()
  end
end
