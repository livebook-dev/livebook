defmodule Livebook.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  def start(_type, _args) do
    ensure_distribution()

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
    Supervisor.start_link(children, opts)
  end

  # Tell Phoenix to update the endpoint configuration
  # whenever the application is updated.
  def config_change(changed, _new, removed) do
    LivebookWeb.Endpoint.config_change(changed, removed)
    :ok
  end

  defp ensure_distribution() do
    unless Node.alive?() do
      System.cmd("epmd", ["-daemon"])
      {type, name} = get_node_type_and_name()
      Node.start(name, type)
    end
  end

  defp get_node_type_and_name() do
    {type, name} = Application.fetch_env!(:livebook, :node_name)

    {base_name, host_sufix} =
      case name |> to_string() |> String.split("@") do
        [base_name] -> {base_name, ""}
        [base_name, host_name] -> {base_name, "@" <> host_name}
      end

    if name_taken?(base_name) do
      # If there's already Livebook instance started, use a different name
      new_name = :"#{base_name}_#{Livebook.Utils.random_short_id()}#{host_sufix}"
      {type, new_name}
    else
      {type, name}
    end
  end

  # Checks if there's a local node running with the given base name.
  defp name_taken?(name) do
    name = to_string(name)

    case :erl_epmd.names() do
      {:ok, list} ->
        Enum.any?(list, fn {alive_name, _port} ->
          to_string(alive_name) == name
        end)

      {:error, _} ->
        false
    end
  end
end
