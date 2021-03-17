defmodule LivebookCLI do
  @moduledoc false

  def main(args) do
    {:ok, _} = Application.ensure_all_started(:elixir)
    :ok = Application.load(:livebook)

    case check_for_shortcuts(args) do
      :help ->
        IO.puts("Livebook is an interactive notebook system for Elixir\n")
        display_usage()

      :version ->
        display_version()

      nil ->
        proceed(args)
    end
  end

  defp proceed(args) do
    case args do
      ["server" | _args] ->
        start_server()

        IO.puts("Livebook running at #{LivebookWeb.Endpoint.url()}")

        Process.sleep(:infinity)

      _ ->
        display_usage()
    end
  end

  defp start_server() do
    Application.put_env(:phoenix, :serve_endpoints, true, persistent: true)
    {:ok, _} = Application.ensure_all_started(:livebook)
  end

  # Check for --help or --version in the args
  defp check_for_shortcuts([arg]) when arg in ["--help", "-h"], do: :help

  defp check_for_shortcuts([arg]) when arg in ["--version", "-v"], do: :version

  defp check_for_shortcuts(_), do: nil

  defp display_usage() do
    IO.write("""
    Usage: livebook [command]

    Available commands:

      livebook server - Starts the Livebook web application

    The --help and --version options can be given instead of a command for usage and versioning information.
    """)
  end

  defp display_version do
    IO.puts(:erlang.system_info(:system_version))
    IO.puts("Elixir " <> System.build_info()[:build])

    version = Application.spec(:livebook, :vsn)
    IO.puts("\nLivebook #{version}")
  end
end
