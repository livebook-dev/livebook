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

      * If the open-command is "new", the browser window will point
        to a new notebook

      * If the open-command is a URL, the notebook at the given URL
        will be imported

    The open-command runs after the server is started. If a server is
    already running, the browser window will point to the server
    currently running.

    ## Available options

      --autosave-path      The directory where notebooks with no file are persisted.
                           Defaults to livebook/notebooks/ under the default user cache
                           location. You can pass "none" to disable this behaviour
      --cookie             Sets a cookie for the app distributed node
      --default-runtime    Sets the runtime type that is used by default when none is started
                           explicitly for the given notebook, defaults to standalone
                           Supported options:
                             * standalone - Elixir standalone
                             * mix[:PATH] - Mix standalone
                             * attached:NODE:COOKIE - Attached
                             * embedded - Embedded
      --ip                 The ip address to start the web application on, defaults to 127.0.0.1
                           Must be a valid IPv4 or IPv6 address
      --name               Set a name for the app distributed node
      --no-token           Disable token authentication, enabled by default
                           If LIVEBOOK_PASSWORD is set, it takes precedence over token auth
      --open               Open browser window pointing to the application
      -p, --port           The port to start the web application on, defaults to 8080
      --root-path          The root path to use for file selection
      --sname              Set a short name for the app distributed node

    The --help option can be given to print this notice.

    ## Environment variables

    #{@environment_variables}

    ## Examples

    Starts a server:

        livebook server

    Starts a server and opens up a browser at a new notebook:

        livebook server new

    Starts a server and imports the notebook at the given URL:

        livebook server https://example.com/my-notebook.livemd

    """
  end

  @impl true
  def call(args) do
    {opts, extra_args} = args_to_options(args)
    config_entries = opts_to_config(opts, [])
    put_config_entries(config_entries)

    port = Application.get_env(:livebook, LivebookWeb.Endpoint)[:http][:port]
    base_url = "http://localhost:#{port}"

    case check_endpoint_availability(base_url) do
      :livebook_running ->
        IO.puts("Livebook already running on #{base_url}")
        open_from_options(base_url, opts, extra_args)

      :taken ->
        print_error(
          "Another application is already running on port #{port}." <>
            " Either ensure this port is free or specify a different port using the --port option"
        )

      :available ->
        # We configure the endpoint with `server: true`,
        # so it's gonna start listening
        case Application.ensure_all_started(:livebook) do
          {:ok, _} ->
            open_from_options(LivebookWeb.Endpoint.access_url(), opts, extra_args)
            Process.sleep(:infinity)

          {:error, error} ->
            print_error("Livebook failed to start with reason: #{inspect(error)}")
        end
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

    health_url = append_path(base_url, "/health")

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

  defp open_from_options(base_url, opts, []) do
    if opts[:open] do
      Livebook.Utils.browser_open(base_url)
    end
  end

  defp open_from_options(base_url, _opts, ["new"]) do
    base_url
    |> append_path("/explore/notebooks/new")
    |> Livebook.Utils.browser_open()
  end

  defp open_from_options(base_url, _opts, [url]) do
    base_url
    |> Livebook.Utils.notebook_import_url(url)
    |> Livebook.Utils.browser_open()
  end

  defp open_from_options(_base_url, _opts, _extra_args) do
    print_error(
      "Too many arguments entered. Ensure only one argument is used to specify the file path and all other arguments are preceded by the relevant switch"
    )
  end

  @switches [
    autosave_path: :string,
    cookie: :string,
    default_runtime: :string,
    ip: :string,
    name: :string,
    open: :boolean,
    port: :integer,
    root_path: :string,
    sname: :string,
    token: :boolean
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

  defp opts_to_config([{:token, false} | opts], config) do
    if Livebook.Config.auth_mode() == :token do
      opts_to_config(opts, [{:livebook, :authentication_mode, :disabled} | config])
    else
      opts_to_config(opts, config)
    end
  end

  defp opts_to_config([{:port, port} | opts], config) do
    opts_to_config(opts, [{:livebook, LivebookWeb.Endpoint, http: [port: port]} | config])
  end

  defp opts_to_config([{:ip, ip} | opts], config) do
    ip = Livebook.Config.ip!("--ip", ip)
    opts_to_config(opts, [{:livebook, LivebookWeb.Endpoint, http: [ip: ip]} | config])
  end

  defp opts_to_config([{:root_path, root_path} | opts], config) do
    root_path =
      Livebook.Config.root_path!("--root-path", root_path)
      |> Livebook.FileSystem.Utils.ensure_dir_path()

    local_file_system = Livebook.FileSystem.Local.new(default_path: root_path)
    opts_to_config(opts, [{:livebook, :default_file_systems, [local_file_system]} | config])
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

  defp opts_to_config([{:default_runtime, default_runtime} | opts], config) do
    default_runtime = Livebook.Config.default_runtime!("--default-runtime", default_runtime)
    opts_to_config(opts, [{:livebook, :default_runtime, default_runtime} | config])
  end

  defp opts_to_config([{:autosave_path, path} | opts], config) do
    autosave_path = Livebook.Config.autosave_path!("--autosave-path", path)
    opts_to_config(opts, [{:livebook, :autosave_path, autosave_path} | config])
  end

  defp opts_to_config([_opt | opts], config), do: opts_to_config(opts, config)

  defp append_path(url, path) do
    url
    |> URI.parse()
    |> Map.update!(:path, &((&1 || "") <> path))
    |> URI.to_string()
  end

  defp print_error(message) do
    IO.ANSI.format([:red, message]) |> IO.puts()
  end
end
