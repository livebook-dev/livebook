defmodule Livebook.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  def start(_type, _args) do
    ensure_distribution!()
    set_cookie()

    children = [
      # Start the Telemetry supervisor
      LivebookWeb.Telemetry,
      # Start the PubSub system
      {Phoenix.PubSub, name: Livebook.PubSub},
      # Start the supervisor dynamically managing sessions
      Livebook.SessionSupervisor,
      # Start the server responsible for associating files with sessions
      Livebook.Session.FileGuard,
      # Start the Node Pool for managing node names
      Livebook.Runtime.NodePool,
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
          Livebook.Config.abort!("""
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
        {:ok, _} ->
          :ok

        {:error, reason} ->
          Livebook.Config.abort!("could not start distributed node: #{inspect(reason)}")
      end
    end
  end

  defp set_cookie() do
    cookie = Application.fetch_env!(:livebook, :cookie)
    Node.set_cookie(cookie)
  end

  defp get_node_type_and_name() do
    Application.get_env(:livebook, :node) || {:shortnames, random_short_name()}
  end

  defp random_short_name() do
    :"livebook_#{Livebook.Utils.random_short_id()}"
  end

  defp display_startup_info() do
    if Phoenix.Endpoint.server?(:livebook, LivebookWeb.Endpoint) do
      IO.puts("[Livebook] Application running at #{access_url()}")
    end
  end

  defp access_url() do
    root_url = LivebookWeb.Endpoint.url()

    if Livebook.Config.auth_mode() == :token do
      token = Application.fetch_env!(:livebook, :token)
      root_url <> "/?token=" <> token
    else
      root_url
    end
  end
end
