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
      ["server" | args] ->
        proceed_server(args)

      _ ->
        display_usage()
    end
  end

  defp proceed_server([arg]) when arg in ["--help", "-h"] do
    IO.write("""
    Usage: livebook server [options]

    Available options:

      --no-token - Disable token authentication

    The --help option can be given for usage information.
    """)
  end

  defp proceed_server(args) do
    {opts, _, _} = OptionParser.parse(args, strict: [token: :boolean])

    token =
      if Keyword.get(opts, :token, true) do
        token = Livebook.Utils.random_id()
        Application.put_env(:livebook, :token, token)
        token
      else
        nil
      end

    start_server()

    url =
      if token do
        LivebookWeb.Endpoint.url() <> "/?token=" <> token
      else
        LivebookWeb.Endpoint.url()
      end

    IO.puts("Livebook running at #{url}")

    Process.sleep(:infinity)
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
    Usage: livebook [command] [options]

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
