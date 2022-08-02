defmodule LivebookWeb.SessionLive.ExportComponent do
  use LivebookWeb, :live_component

  alias Livebook.Session

  @impl true
  def update(assigns, socket) do
    socket = assign(socket, assigns)

    socket =
      if socket.assigns[:notebook] do
        socket
      else
        # Note: we need to load the notebook, because the local data
        # has cell contents stripped out
        notebook = Session.get_notebook(socket.assigns.session.pid)
        assign(socket, :notebook, notebook)
      end

    {:ok, socket}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="p-6 max-w-4xl flex flex-col space-y-3">
      <h3 class="text-2xl font-semibold text-gray-800">
        Export
      </h3>
      <div class="w-full flex-col space-y-5">
        <p class="text-gray-700">
          Here you can preview and directly export the notebook source.
        </p>
        <div class="tabs">
          <%= live_patch to: Routes.session_path(@socket, :export, @session.id, "livemd"),
                class: "tab #{if(@tab == "livemd", do: "active")}" do %>
            <span class="font-medium">
              Live Markdown
            </span>
          <% end %>
          <%= live_patch to: Routes.session_path(@socket, :export, @session.id, "exs"),
                class: "tab #{if(@tab == "exs", do: "active")}" do %>
            <span class="font-medium">
              IEx session
            </span>
          <% end %>
        </div>
        <div>
          <.live_component
            module={component_for_tab(@tab)}
            id={"export-notebook-#{@tab}"}
            session={@session}
            notebook={@notebook}
          />
        </div>
      </div>
    </div>
    """
  end

  defp component_for_tab("livemd"), do: LivebookWeb.SessionLive.ExportLiveMarkdownComponent
  defp component_for_tab("exs"), do: LivebookWeb.SessionLive.ExportElixirComponent
end
