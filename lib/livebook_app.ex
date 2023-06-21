if Mix.target() == :app do
  defmodule LivebookApp do
    use GenServer

    def start_link(arg) do
      GenServer.start_link(__MODULE__, arg, name: __MODULE__)
    end

    @impl true
    def init(_) do
      {:ok, pid} = ElixirKit.start()
      ref = Process.monitor(pid)

      ElixirKit.publish("url", LivebookWeb.Endpoint.access_url())

      {:ok, %{ref: ref}}
    end

    @impl true
    def handle_info({:event, "open", url}, state) do
      url
      |> Livebook.Utils.expand_desktop_url()
      |> Livebook.Utils.browser_open()

      {:noreply, state}
    end

    @impl true
    def handle_info({:DOWN, ref, :process, _, :shutdown}, state) when ref == state.ref do
      Livebook.Config.shutdown()
      {:noreply, state}
    end
  end
end
