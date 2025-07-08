defmodule LivebookCLI.Utils do
  def setup do
    {:ok, _} = Application.ensure_all_started(:elixir)

    extract_priv!()

    :ok = Application.load(:livebook)

    if unix?() do
      Application.put_env(:elixir, :ansi_enabled, true)
    end
  end

  defp unix?(), do: match?({:unix, _}, :os.type())

  def option_parse(argv, opts \\ []) do
    {parsed, argv, errors} = OptionParser.parse(argv, opts)
    {Enum.into(parsed, %{}), argv, errors}
  end

  def log_info(message) do
    IO.puts(message)
  end

  if Mix.env() == :dev do
    def log_debug(message) do
      [:cyan, message]
      |> IO.ANSI.format()
      |> IO.puts()
    end
  else
    def log_debug(_message) do
      :ok
    end
  end

  def log_warning(message) do
    [:yellow, message]
    |> IO.ANSI.format()
    |> IO.warn()
  end

  def print_text(message) do
    message
    |> IO.ANSI.format()
    |> IO.puts()
  end

  @spec log_exception(Exception.t(), String.t(), Exception.stacktrace()) :: no_return()
  def log_exception(exception, command_name, stacktrace) when is_exception(exception) do
    [:red, format_exception(exception, command_name, stacktrace)]
    |> IO.ANSI.format()
    |> IO.puts()

    System.halt(1)
  end

  defp format_exception(%OptionParser.ParseError{} = exception, command_name, _) do
    """
    #{Exception.message(exception)}

    For more information try:

       livebook #{command_name} --help
    """
  end

  defp format_exception(%RuntimeError{} = exception, _, _) do
    Exception.message(exception)
  end

  defp format_exception(exception, _, stacktrace) do
    Exception.format(:error, exception, stacktrace)
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
