defmodule LivebookCLI.Server do
  @moduledoc false

  @behaviour LivebookCLI.Task

  @external_resource "README.md"

  [_, environment_variables, _] =
    "README.md"
    |> File.read!()
    |> String.split("<!-- Environment variables -->")

  @environment_variables String.trim(environment_variables)

  @impl true
  def usage() do
    """
    Usage: livebook server [options] [open-command]

    An optional open-command can be given as argument. It will open
    up a browser window according these rules:

      * If the open-command is "@home", the browser window will point
        to the home page

      * If the open-command is "@new", the browser window will point
        to a new notebook

      * If the open-command is a URL, the notebook at the given URL
        will be imported

      * If the open-command is a directory, the browser window will point
        to the open page with the directory selected

      * If the open-command is a notebook file, the browser window will point
        to the opened notebook

    The open-command runs after the server is started. If a server is
    already running, the browser window will point to the server
    currently running.

    ## Available options

      --cookie             Sets a cookie for the app distributed node
      --ip                 The ip address to start the web application on, defaults to 127.0.0.1
                           Must be a valid IPv4 or IPv6 address
      --name               Sets a name for the app distributed node
      -p, --port           The port to start the web application on, defaults to 8080
      --sname              Sets a short name for the app distributed node

    The --help option can be given to print this notice.

    ## Environment variables

    #{@environment_variables}

    ## Examples

    Starts a server:

        livebook server

    Starts a server and opens up a browser at a new notebook:

        livebook server @new

    Starts a server and imports the notebook at the given URL:

        livebook server https://example.com/my-notebook.livemd

    """
  end

  @impl true
  def call(args) do
    {opts, extra_args} = args_to_options(args)
    config_entries = opts_to_config(opts, [])
    put_config_entries(config_entries)

    case Livebook.Config.port() do
      0 ->
        # When a random port is configured, we can assume no collision
        start_server(extra_args)

      port ->
        base_url = "http://localhost:#{port}"

        case check_endpoint_availability(base_url) do
          :livebook_running ->
            IO.puts("Livebook already running on #{base_url}")
            open_from_args(base_url, extra_args)

          :taken ->
            print_error(
              "Another application is already running on port #{port}." <>
                " Either ensure this port is free or specify a different port using the --port option"
            )

          :available ->
            start_server(extra_args)
        end
    end
  end

  defp start_server(extra_args) do
    # We configure the endpoint with `server: true`,
    # so it's gonna start listening
    case Application.ensure_all_started(:livebook) do
      {:ok, _} ->
        open_from_args(LivebookWeb.Endpoint.access_url(), extra_args)
        Process.sleep(:infinity)

      {:error, error} ->
        print_error("Livebook failed to start with reason: #{inspect(error)}")
    end
  end

  # Takes a list of {app, key, value} config entries
  # and overrides the current applications' configuration accordingly.
  # Multiple values for the same key are deeply merged (provided they are keyword lists).
  defp put_config_entries(config_entries) do
    config_entries
    |> Enum.reduce([], fn {app, key, value}, acc ->
      acc = Keyword.put_new_lazy(acc, app, fn -> Application.get_all_env(app) end)
      Config.Reader.merge(acc, [{app, [{key, value}]}])
    end)
    |> Application.put_all_env(persistent: true)
  end

  defp check_endpoint_availability(base_url) do
    Application.ensure_all_started(:inets)

    health_url = set_path(base_url, "/public/health")

    case Livebook.Utils.HTTP.request(:get, health_url) do
      {:ok, status, _headers, body} ->
        with 200 <- status,
             {:ok, body} <- Jason.decode(body),
             %{"application" => "livebook"} <- body do
          :livebook_running
        else
          _ -> :taken
        end

      {:error, _error} ->
        :available
    end
  end

  defp open_from_args(_base_url, []) do
    :ok
  end

  defp open_from_args(base_url, ["@home"]) do
    Livebook.Utils.browser_open(base_url)
  end

  defp open_from_args(base_url, ["@new"]) do
    base_url
    |> set_path("/new")
    |> Livebook.Utils.browser_open()
  end

  defp open_from_args(base_url, ["new"]) do
    IO.warn(~s/passing "new" as an argument is deprecated, use "@new" instead/, [])
    open_from_args(base_url, ["@new"])
  end

  defp open_from_args(base_url, [url_or_file_or_dir]) do
    url = URI.parse(url_or_file_or_dir)
    path = Path.expand(url_or_file_or_dir)

    cond do
      url.scheme in ~w(http https file) ->
        base_url
        |> Livebook.Utils.notebook_import_url(url_or_file_or_dir)
        |> Livebook.Utils.browser_open()

      File.regular?(path) or File.dir?(path) ->
        base_url
        |> Livebook.Utils.notebook_open_url(url_or_file_or_dir)
        |> Livebook.Utils.browser_open()

      true ->
        Livebook.Utils.browser_open(base_url)
    end
  end

  defp open_from_args(_base_url, _extra_args) do
    print_error(
      "Too many arguments entered. Ensure only one argument is used to specify the file path and all other arguments are preceded by the relevant switch"
    )
  end

  @switches [
    cookie: :string,
    ip: :string,
    name: :string,
    port: :integer,
    home: :string,
    sname: :string
  ]

  @aliases [
    p: :port
  ]

  defp args_to_options(args) do
    {opts, extra_args} = OptionParser.parse!(args, strict: @switches, aliases: @aliases)
    validate_options!(opts)
    {opts, extra_args}
  end

  defp validate_options!(opts) do
    if Keyword.has_key?(opts, :name) and Keyword.has_key?(opts, :sname) do
      raise "the provided --sname and --name options are mutually exclusive, please specify only one of them"
    end
  end

  defp opts_to_config([], config), do: config

  defp opts_to_config([{:port, port} | opts], config) do
    opts_to_config(opts, [{:livebook, LivebookWeb.Endpoint, http: [port: port]} | config])
  end

  defp opts_to_config([{:ip, ip} | opts], config) do
    ip = Livebook.Config.ip!("--ip", ip)
    host = Livebook.Utils.ip_to_host(ip)

    opts_to_config(opts, [
      {:livebook, LivebookWeb.Endpoint, http: [ip: ip], url: [host: host]} | config
    ])
  end

  defp opts_to_config([{:sname, sname} | opts], config) do
    sname = String.to_atom(sname)
    opts_to_config(opts, [{:livebook, :node, {:shortnames, sname}} | config])
  end

  defp opts_to_config([{:name, name} | opts], config) do
    name = String.to_atom(name)
    opts_to_config(opts, [{:livebook, :node, {:longnames, name}} | config])
  end

  defp opts_to_config([{:cookie, cookie} | opts], config) do
    cookie = String.to_atom(cookie)
    opts_to_config(opts, [{:livebook, :cookie, cookie} | config])
  end

  defp opts_to_config([_opt | opts], config), do: opts_to_config(opts, config)

  defp set_path(url, path) do
    url
    |> URI.parse()
    |> Map.put(:path, path)
    |> URI.to_string()
  end

  defp print_error(message) do
    IO.ANSI.format([:red, message]) |> IO.puts()
  end
end
