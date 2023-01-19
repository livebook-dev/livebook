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
      open(url)
      {:noreply, state}
    end

    @impl true
    def handle_info({:DOWN, ref, :process, _, :shutdown}, state) when ref == state.ref do
      Livebook.Config.shutdown()
      {:noreply, state}
    end

    defp open("") do
      open(LivebookWeb.Endpoint.access_url())
    end

    defp open("file://" <> path) do
      path
      |> Livebook.Utils.notebook_open_url()
      |> open()
    end

    defp open("livebook://" <> rest) do
      "https://#{rest}"
      |> Livebook.Utils.notebook_import_url()
      |> open()
    end

    defp open("/settings") do
      %{LivebookWeb.Endpoint.access_struct_url() | path: "/settings"}
      |> to_string()
      |> open()
    end

    defp open(url) do
      Livebook.Utils.browser_open(url)
    end
  end
end
