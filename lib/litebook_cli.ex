defmodule LivebookCLI do
  @moduledoc false

  @usage """
  Usage: livebook [command] [options]

  Available commands:

    livebook server    Starts the Livebook web application

  The --help and --version options can be given instead of a command for usage and versioning information.
  """

  def main(args) do
    {:ok, _} = Application.ensure_all_started(:elixir)
    Application.put_env(:elixir, :ansi_enabled, true)
    :ok = Application.load(:livebook)
    call(args)
  end

  defp call([arg]) when arg in ["--help", "-h"], do: display_help()
  defp call([arg]) when arg in ["--version", "-v"], do: display_version()
  defp call(["server" | args]), do: LivebookCLI.Server.call(args)
  defp call(_args), do: display_usage()

  defp display_usage() do
    IO.write(@usage)
  end

  defp display_help() do
    IO.puts("Livebook is an interactive notebook system for Elixir\n")
    display_usage()
  end

  defp display_version() do
    IO.puts(:erlang.system_info(:system_version))
    IO.puts("Elixir " <> System.build_info()[:build])

    version = Application.spec(:livebook, :vsn)
    IO.puts("\nLivebook #{version}")
  end
end
