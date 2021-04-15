defmodule Livebook.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  def start(_type, _args) do
    ensure_distribution!()
    initialize_auth!()

    children = [
      # Start the Telemetry supervisor
      LivebookWeb.Telemetry,
      # Start the PubSub system
      {Phoenix.PubSub, name: Livebook.PubSub},
      # Start the supervisor dynamically managing sessions
      Livebook.SessionSupervisor,
      # Start the server responsible for associating files with sessions
      Livebook.Session.FileGuard,
      # Start the Endpoint (http/https)
      LivebookWeb.Endpoint
    ]

    opts = [strategy: :one_for_one, name: Livebook.Supervisor]

    with {:ok, _} = result <- Supervisor.start_link(children, opts) do
      display_startup_info()
      result
    end
  end

  # Tell Phoenix to update the endpoint configuration
  # whenever the application is updated.
  def config_change(changed, _new, removed) do
    LivebookWeb.Endpoint.config_change(changed, removed)
    :ok
  end

  defp ensure_distribution!() do
    unless Node.alive?() do
      case System.cmd("epmd", ["-daemon"]) do
        {_, 0} ->
          :ok

        _ ->
          abort!("""
          could not start epmd (Erlang Port Mapper Driver). Livebook uses epmd to \
          talk to different runtimes. You may have to start epmd explicitly by calling:

              epmd -daemon

          Or by calling:

              elixir --sname test -e "IO.puts node()"

          Then you can try booting Livebook again
          """)
      end

      {type, name} = get_node_type_and_name()

      case Node.start(name, type) do
        {:ok, _} -> :ok
        {:error, reason} -> abort!("could not start distributed node: #{inspect(reason)}")
      end
    end
  end

  def abort!(message) do
    IO.puts("\nERROR!!! [Livebook] " <> message)
    System.halt(1)
  end

  defp get_node_type_and_name() do
    Application.get_env(:livebook, :node) || {:shortnames, random_short_name()}
  end

  defp random_short_name() do
    :"livebook_#{Livebook.Utils.random_short_id()}"
  end

  defp initialize_auth!() do
    secret_key_base = Application.fetch_env!(:livebook, LivebookWeb.Endpoint)[:secret_key_base]

    if byte_size(secret_key_base) < 64 do
      Livebook.Application.abort!(
      "cannot start Livebook because LIVEBOOK_SECRET_KEY_BASE must be at least 64 characters. " <>
        "Invoke `openssl rand -base64 48` to generate an appropriately long secret."
      )
    end

    if Livebook.Config.auth_mode() == :token do
      token = Livebook.Utils.random_id()
      Application.put_env(:livebook, :token, token)
    end
  end

  defp display_startup_info() do
    if Phoenix.Endpoint.server?(:livebook, LivebookWeb.Endpoint) do
      IO.puts("[Livebook] Application running at #{access_url()}")
    end
  end

  defp access_url() do
    token = Application.get_env(:livebook, :token)
    root_url = LivebookWeb.Endpoint.url()

    if token do
      root_url <> "/?token=" <> token
    else
      root_url
    end
  end
end
