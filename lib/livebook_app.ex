if Mix.target() in [:app, :app_next] do
  defmodule LivebookApp do
    use GenServer

    def start_link(arg) do
      GenServer.start_link(__MODULE__, arg, name: __MODULE__)
    end

    @impl true
    def init(_) do
      ref = Process.monitor(ElixirKit.PubSub)

      ElixirKit.PubSub.subscribe("messages")
      ElixirKit.PubSub.broadcast("messages", "ready:" <> LivebookWeb.Endpoint.access_url())

      {:ok, %{ref: ref, log_path: System.fetch_env!("LOG_PATH")}}
    end

    @impl true
    def handle_info("open:/logs", state) do
      Livebook.Utils.browser_open("file://" <> state.log_path)

      {:noreply, state}
    end

    @impl true
    def handle_info("open:" <> url, state) do
      url
      |> Livebook.Utils.expand_desktop_url()
      |> Livebook.Utils.browser_open()

      {:noreply, state}
    end

    @impl true
    def handle_info({:DOWN, ref, :process, _, _reason}, state) when ref == state.ref do
      Livebook.Config.shutdown()
      {:noreply, state}
    end
  end
end
