defmodule Livebook.Hubs.EnterpriseClient do
  @moduledoc false
  use GenServer

  alias Livebook.Hubs.Enterprise
  alias Livebook.WebSocket
  alias Livebook.WebSocket.Server

  defstruct [:server, :hub, reply: %{}]

  @doc """
  Connects the Enterprise client with WebSocket server.
  """
  @spec start_link(Enterprise.t()) :: GenServer.on_start()
  def start_link(%Enterprise{} = enterprise) do
    GenServer.start_link(__MODULE__, enterprise)
  end

  @doc """
  Sends a Request.
  """
  @spec send_request(pid(), WebSocket.proto()) :: :ok
  def send_request(pid, %_struct{} = data) do
    GenServer.call(pid, {:request, data})
  end

  @doc """
  Disconnects the Enterprise client with WebSocket server.
  """
  @spec disconnect(pid()) :: :ok
  def disconnect(pid) do
    GenServer.cast(pid, :disconnect)
  end

  ## GenServer callbacks

  @impl true
  def init(%Enterprise{url: url, token: token} = enterprise) do
    headers = [{"X-Auth-Token", token}]
    {:ok, pid} = Server.start_link(self(), url, headers)

    {:ok, %__MODULE__{hub: enterprise, server: pid}}
  end

  @impl true
  def handle_call({:request, data}, caller, state) do
    {:ok, id} = Server.send_request(state.server, data)

    {:noreply, %{state | reply: Map.put(state.reply, id, caller)}}
  end

  @impl true
  def handle_cast(:disconnect, state) do
    with :ok <- Server.close(state.server) do
      {:noreply, state}
    end
  end

  @impl true
  def handle_info({:connect, _, _} = message, state) do
    WebSocket.broadcast_message(message)
    {:noreply, state}
  end

  def handle_info({:disconnect, :error, _} = message, state) do
    WebSocket.broadcast_message(message)
    {:noreply, state}
  end

  def handle_info({:disconnect, :ok, :disconnected}, state) do
    {:stop, :normal, state}
  end

  def handle_info({:response, :error, _reason}, state) do
    {:noreply, state}
  end

  def handle_info({:response, id, result}, state) do
    {caller, reply} = Map.pop(state.reply, id)
    if caller, do: GenServer.reply(caller, result)

    {:noreply, %{state | reply: reply}}
  end
end
