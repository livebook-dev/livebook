defmodule LivebookCLI.Server do
  @moduledoc false

  @behaviour LivebookCLI.Task

  @impl true
  def usage() do
    """
    Usage: livebook server [options]

    Available options:

      -p, --port    The port to start the web application on, defaults to 8080
      --no-token    Disable token authentication, enabled by default
      --sname       Set a shot name for the app distributed node
      --name        Set a name for the app distributed node

    The --help option can be given for usage information.
    """
  end

  @impl true
  def call(args) do
    config_entries = args_to_config(args)
    put_config_entries(config_entries)

    case start_server() do
      :ok ->
        IO.ANSI.format([:blue, "Livebook running at #{get_url()}"]) |> IO.puts()
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
    |> Enum.group_by(fn {app, key, _value} -> {app, key} end, fn {_app, _key, value} -> value end)
    |> Enum.map(fn {{app, key}, values} ->
      value = Enum.reduce(values, &deep_merge/2)
      {app, key, value}
    end)
    |> Enum.group_by(fn {app, _key, _value} -> app end, fn {_app, key, value} -> {key, value} end)
    |> Enum.map(fn {app, env} ->
      current_env = Application.get_all_env(app)
      new_env = deep_merge(current_env, env)
      {app, new_env}
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

  defp get_url() do
    token = Application.get_env(:livebook, :token)
    root_url = LivebookWeb.Endpoint.url()

    if token do
      root_url <> "/?token=" <> token
    else
      root_url
    end
  end

  defp args_to_config(args) do
    {opts, _} =
      OptionParser.parse!(
        args,
        strict: [token: :boolean, port: :integer, name: :string, sname: :string],
        aliases: [p: :port]
      )

    default_opts = [token: true, port: 8080]
    opts = Keyword.merge(default_opts, opts)

    validate_options!(opts)

    opts_to_config(opts, [])
  end

  defp validate_options!(opts) do
    if Keyword.has_key?(opts, :name) and Keyword.has_key?(opts, :sname) do
      raise "The provided --sname and --name options are mutually exclusive, please specify only one of them"
    end
  end

  defp opts_to_config([], config), do: config

  defp opts_to_config([{:token, true} | opts], config) do
    token = Livebook.Utils.random_id()
    opts_to_config(opts, [{:livebook, :token, token} | config])
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

  # Deeply merges options
  defp deep_merge(value1, value2) do
    if Keyword.keyword?(value1) and Keyword.keyword?(value2) do
      Keyword.merge(value1, value2, fn _key, value1, value2 ->
        deep_merge(value1, value2)
      end)
    else
      value2
    end
  end
end
