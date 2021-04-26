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
    Usage: livebook server [options]

    Available options:

      --cookie      Sets a cookie for the app distributed node
      --ip          The ip address to start the web application on, defaults to 127.0.0.1
                    Must be a valid IPv4 or IPv6 address
      --name        Set a name for the app distributed node
      --no-token    Disable token authentication, enabled by default
                    If LIVEBOOK_PASSWORD is set, it takes precedence over token auth
      -p, --port    The port to start the web application on, defaults to 8080
      --root-path   The root path to use for file selection
      --sname       Set a short name for the app distributed node

    The --help option can be given to print this notice.

    ## Environment variables

    #{@environment_variables}

    """
  end

  @impl true
  def call(args) do
    config_entries = args_to_config(args)
    put_config_entries(config_entries)

    case start_server() do
      :ok ->
        Process.sleep(:infinity)

      :error ->
        IO.ANSI.format([:red, "Livebook failed to start"]) |> IO.puts()
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

  defp start_server() do
    # We configure the endpoint with `server: true`,
    # so it's gonna start listening
    case Application.ensure_all_started(:livebook) do
      {:ok, _} -> :ok
      {:error, _} -> :error
    end
  end

  @switches [
    cookie: :string,
    ip: :string,
    name: :string,
    port: :integer,
    root_path: :string,
    sname: :string,
    token: :boolean
  ]

  @aliases [
    p: :port
  ]

  defp args_to_config(args) do
    {opts, _} = OptionParser.parse!(args, strict: @switches, aliases: @aliases)
    validate_options!(opts)
    opts_to_config(opts, [])
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
    root_path = Livebook.Config.root_path!("--root-path", root_path)
    opts_to_config(opts, [{:livebook, :root_path, root_path} | config])
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
end
