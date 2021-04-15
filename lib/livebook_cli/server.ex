defmodule LivebookCLI.Server do
  @moduledoc false

  @behaviour LivebookCLI.Task
  @external_resource "README.md"

  [_, environment_variables, _] =
    "README.md"
    |> File.read!()
    |> String.split("<!-- Environment variables -->")

  @external_resource "README.md"
  @environment_variables String.trim(environment_variables)

  @impl true
  def usage() do
    """
    Usage: livebook server [options]

    Available options:

      --name        Set a name for the app distributed node
      --no-token    Disable token authentication, enabled by default
                    If LIVEBOOK_PASSWORD is set, it takes precedence over token auth
      --sname       Set a short name for the app distributed node
      -p, --port    The port to start the web application on, defaults to 8080

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
    Application.put_env(:phoenix, :serve_endpoints, true, persistent: true)

    case Application.ensure_all_started(:livebook) do
      {:ok, _} -> :ok
      {:error, _} -> :error
    end
  end

  defp args_to_config(args) do
    {opts, _} =
      OptionParser.parse!(
        args,
        strict: [token: :boolean, port: :integer, name: :string, sname: :string],
        aliases: [p: :port]
      )

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

  defp opts_to_config([{:sname, sname} | opts], config) do
    sname = String.to_atom(sname)
    opts_to_config(opts, [{:livebook, :node, {:shortnames, sname}} | config])
  end

  defp opts_to_config([{:name, name} | opts], config) do
    name = String.to_atom(name)
    opts_to_config(opts, [{:livebook, :node, {:longnames, name}} | config])
  end

  defp opts_to_config([_opt | opts], config), do: opts_to_config(opts, config)
end
