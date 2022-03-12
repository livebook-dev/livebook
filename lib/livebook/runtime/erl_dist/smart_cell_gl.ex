defmodule Livebook.Runtime.ErlDist.SmartCellGL do
  @moduledoc false

  use GenServer

  @spec start_link(pid()) :: GenServer.on_start()
  def start_link(runtime_broadcast_to) do
    GenServer.start_link(__MODULE__, {runtime_broadcast_to})
  end

  @impl true
  def init({runtime_broadcast_to}) do
    {:ok, %{runtime_broadcast_to: runtime_broadcast_to}}
  end

  @impl true
  def handle_info({:io_request, from, reply_as, req}, state) do
    case io_request(req, state) do
      :forward ->
        # Forward the request to own group leader
        gl = Process.group_leader()
        send(gl, {:io_request, from, reply_as, req})

      {:reply, reply} ->
        send(from, {:io_reply, reply_as, reply})
    end

    {:noreply, state}
  end

  defp io_request(:livebook_get_broadcast_target, state) do
    {:reply, {:ok, state.runtime_broadcast_to}}
  end

  defp io_request(_req, _state) do
    :forward
  end
end
