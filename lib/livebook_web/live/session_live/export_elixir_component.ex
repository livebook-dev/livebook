defmodule LivebookWeb.SessionLive.ExportElixirComponent do
  use LivebookWeb, :live_component

  alias Livebook.Session

  @impl true
  def update(assigns, socket) do
    socket = assign(socket, assigns)

    source = Livebook.Notebook.Export.Elixir.notebook_to_elixir(socket.assigns.notebook)
    socket = assign(socket, :source, source)

    {:ok, socket}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="flex flex-col space-y-6">
      <div class="flex flex-col space-y-1">
        <div class="flex justify-between items-center">
          <span class="text-sm text-gray-700 font-semibold">
            <%= Session.file_name_for_download(@session) <> ".exs" %>
          </span>
          <div class="flex justify-end space-x-2">
            <span class="tooltip left" data-tooltip="Copy source">
              <button
                class="icon-button"
                aria-label="copy source"
                phx-click={JS.dispatch("lb:clipcopy", to: "#export-notebook-source")}
              >
                <.remix_icon icon="clipboard-line" class="text-lg" />
              </button>
            </span>
            <span class="tooltip left" data-tooltip="Download source">
              <a
                class="icon-button"
                aria-label="download source"
                href={Routes.session_path(@socket, :download_source, @session.id, "exs")}
              >
                <.remix_icon icon="download-2-line" class="text-lg" />
              </a>
            </span>
          </div>
        </div>
        <.code_preview source_id="export-notebook-source" language="elixir" source={@source} />
      </div>
    </div>
    """
  end
end
