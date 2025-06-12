defmodule LivebookCLI do
  def usage() do
    """
    Usage: livebook [command] [options]

    Available commands:

      livebook server    Starts the Livebook web application

    The --help and --version options can be given instead of a command for usage and versioning information.
    """
  end

  def main(args) do
    {:ok, _} = Application.ensure_all_started(:elixir)

    extract_priv!()

    :ok = Application.load(:livebook)

    if unix?() do
      Application.put_env(:elixir, :ansi_enabled, true)
    end

    call(args)
  end

  defp unix?(), do: match?({:unix, _}, :os.type())

  defp call([arg]) when arg in ["--help", "-h"], do: display_help()
  defp call([arg]) when arg in ["--version", "-v"], do: display_version()

  defp call([task_name | args]) do
    case find_task(task_name) do
      nil ->
        IO.ANSI.format([:red, "Unknown command #{task_name}\n"]) |> IO.puts()
        IO.write(usage())

      task ->
        call_task(task, args)
    end
  end

  defp call(_args), do: IO.write(usage())

  defp find_task("server"), do: LivebookCLI.Server
  defp find_task(_), do: nil

  defp call_task(task, [arg]) when arg in ["--help", "-h"] do
    IO.write(task.usage())
  end

  defp call_task(task, args) do
    try do
      task.call(args)
    rescue
      error in OptionParser.ParseError ->
        IO.ANSI.format([
          :red,
          Exception.message(error),
          "\n\nFor more information try --help"
        ])
        |> IO.puts()

      error ->
        IO.ANSI.format([:red, Exception.format(:error, error, __STACKTRACE__), "\n"]) |> IO.puts()
    end
  end

  defp display_help() do
    IO.puts("Livebook is an interactive notebook system for Elixir\n")
    IO.write(usage())
  end

  defp display_version() do
    IO.puts(:erlang.system_info(:system_version))
    IO.puts("Elixir " <> System.build_info()[:build])

    version = Livebook.Config.app_version()
    IO.puts("\nLivebook #{version}")
  end

  import Record
  defrecord(:zip_file, extract(:zip_file, from_lib: "stdlib/include/zip.hrl"))

  defp extract_priv!() do
    archive_dir = Path.join(Livebook.Config.tmp_path(), "escript")
    extracted_path = Path.join(archive_dir, "extracted")
    in_archive_priv_path = ~c"livebook/priv"

    # In dev we want to extract fresh directory on every boot
    if Livebook.Config.app_version() =~ "-dev" do
      File.rm_rf!(archive_dir)
    end

    # When temporary directory is cleaned by the OS, the directories
    # may be left in place, so we use a regular file (extracted) to
    # check if the extracted archive is already available
    if not File.exists?(extracted_path) do
      {:ok, sections} = :escript.extract(:escript.script_name(), [])
      archive = Keyword.fetch!(sections, :archive)

      file_filter = fn zip_file(name: name) ->
        List.starts_with?(name, in_archive_priv_path)
      end

      opts = [cwd: String.to_charlist(archive_dir), file_filter: file_filter]

      with {:error, error} <- :zip.extract(archive, opts) do
        raise "Livebook failed to extract archive files, reason: #{inspect(error)}"
      end

      File.touch!(extracted_path)
    end

    priv_dir = Path.join(archive_dir, in_archive_priv_path)
    Application.put_env(:livebook, :priv_dir, priv_dir, persistent: true)
  end

  @doc """
  Logs an error message.
  """
  def error(message), do: IO.puts(:stderr, IO.ANSI.format(red(message)))

  @doc """
  Logs an info message.
  """
  def info(message), do: IO.puts(IO.ANSI.format(message))

  @doc """
  Logs a debug message.
  """
  def debug(message), do: IO.puts(IO.ANSI.format(cyan(message)))

  @doc """
  Logs a warning message.
  """
  def warning(message), do: IO.warn(message, [])

  @doc """
  Aborts
  """
  @spec raise(String.t()) :: no_return()
  def raise(message) do
    error(message)
    System.halt(1)
  end

  defp red(message), do: [:red, :bright, message]
  defp cyan(message), do: [:cyan, :bright, message]
end
