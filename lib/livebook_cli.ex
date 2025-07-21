defmodule LivebookCLI do
  alias LivebookCLI.{Task, Utils}

  @help_args ["--help", "-h"]
  @version_args ["--version", "-v"]

  def usage,
    do: """
    Usage: livebook [command] [options]

    Available commands:

      livebook server    Starts the Livebook web application
      livebook deploy    Deploys a notebook to Livebook Teams

    The --help and --version options can be given instead of a command for usage and versioning information.\
    """

  def main(args) do
    {:ok, _} = Application.ensure_all_started(:elixir)
    extract_priv!()

    :ok = Application.load(:livebook)

    if unix?() do
      Application.put_env(:elixir, :ansi_enabled, true)
    end

    case args do
      [arg] when arg in @help_args -> display_help()
      [arg] when arg in @version_args -> display_version()
      [name | [arg]] when arg in @help_args -> Task.usage(name)
      [name | args] -> Task.call(name, List.delete(args, name))
      _args -> Utils.print_text(usage())
    end
  end

  defp unix?(), do: match?({:unix, _}, :os.type())

  defp display_help() do
    Utils.print_text("""
    Livebook is an interactive notebook system for Elixir

    #{usage()}\
    """)
  end

  defp display_version() do
    Utils.print_text("""
    #{:erlang.system_info(:system_version)}
    Elixir #{System.build_info()[:build]}

    Livebook #{Livebook.Config.app_version()}\
    """)
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
end
