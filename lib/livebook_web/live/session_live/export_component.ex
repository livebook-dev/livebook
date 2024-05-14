defmodule LivebookWeb.SessionLive.ExportComponent do
  use LivebookWeb, :live_component

  alias Livebook.Session

  @impl true
  def update(assigns, socket) do
    {any_stale_cell?, assigns} = Map.pop!(assigns, :any_stale_cell?)
    socket = assign(socket, assigns)

    {:ok,
     socket
     # Note: we need to load the notebook, because the local data
     # has cell contents stripped out
     |> assign_new(:notebook, fn -> Session.get_notebook(socket.assigns.session.pid) end)
     |> assign_new(:any_stale_cell?, fn -> any_stale_cell? end)}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="flex flex-col space-y-3">
      <h3 class="text-2xl font-semibold text-gray-800">
        Export
      </h3>
      <div class="w-full flex-col space-y-5">
        <p class="text-gray-700">
          Here you can preview and directly export the notebook source.
        </p>
        <div class="tabs">
          <.link
            patch={~p"/sessions/#{@session.id}/export/livemd"}
            class={["tab", @tab == "livemd" && "active"]}
          >
            <span class="font-medium">
              Live Markdown
            </span>
          </.link>
          <.link
            patch={~p"/sessions/#{@session.id}/export/exs"}
            class={["tab", @tab == "exs" && "active"]}
          >
            <span class="font-medium">
              IEx session
            </span>
          </.link>
        </div>
        <div>
          <.live_component
            module={component_for_tab(@tab)}
            id={"export-notebook-#{@tab}"}
            session={@session}
            notebook={@notebook}
            any_stale_cell?={@any_stale_cell?}
          />
        </div>
      </div>
    </div>
    """
  end

  defp component_for_tab("livemd"), do: LivebookWeb.SessionLive.ExportLiveMarkdownComponent
  defp component_for_tab("exs"), do: LivebookWeb.SessionLive.ExportElixirComponent
end
