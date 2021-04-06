defmodule LivebookCLI.Server do
  @moduledoc false

  @usage """
  Usage: livebook server [options]

  Available options:

    -p, --port    The port to start the web application on, defaults to 8080
    --no-token    Disable token authentication, enabled by default

  The --help option can be given for usage information.
  """

  def call([arg]) when arg in ["--help", "-h"] do
    IO.write(@usage)
  end

  def call(args) do
    opts = parse_options(args)

    if Keyword.get(opts, :token, true) do
      token = Livebook.Utils.random_id()
      Application.put_env(:livebook, :token, token)
    end

    if opts[:port] do
      endpoint_env = Application.get_env(:livebook, LivebookWeb.Endpoint)
      endpoint_env = put_in(endpoint_env[:http][:port], opts[:port])
      Application.put_env(:livebook, LivebookWeb.Endpoint, endpoint_env)
    end

    start_server()

    IO.ANSI.format([:blue, "Livebook running at #{get_url()}"]) |> IO.puts()

    Process.sleep(:infinity)
  end

  defp start_server() do
    Application.put_env(:phoenix, :serve_endpoints, true, persistent: true)
    {:ok, _} = Application.ensure_all_started(:livebook)
  end

  defp get_url() do
    token = Application.get_env(:livebook, :token)
    root_url = LivebookWeb.Endpoint.url()

    if token do
      root_url <> "/?token=" <> token
    else
      root_url
    end
  end

  defp parse_options(args) do
    {opts, _, _} =
      OptionParser.parse(args, strict: [token: :boolean, port: :integer], aliases: [p: :port])

    opts
  end
end
