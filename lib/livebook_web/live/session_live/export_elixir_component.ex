defmodule LivebookWeb.SessionLive.ExportElixirComponent do
  use LivebookWeb, :live_component

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
    <div class="flex flex-col space-y-3">
      <p class="text-gray-700">
        <span class="font-semibold">Note:</span>
        the script export is available as a convenience, rather than
        an exact reproduction of the notebook and in some cases it may
        not even compile. For example, if you define a macro in one cell
        and import it in another cell, it works fine in Livebook,
        because each cell is compiled separately. However, when running
        the script it gets compiled as a whole and consequently doing so
        doesn't work. Additionally, branching sections are commented out.
      </p>
      <div class="flex flex-col space-y-1">
        <div class="flex justify-between items-center">
          <span class="text-sm text-gray-700 font-semibold">
            .exs
          </span>
          <div class="flex justify-end space-x-2">
            <span class="tooltip left" aria-label="Copy source">
              <button class="icon-button"
                id="export-notebook-source-clipcopy"
                phx-hook="ClipCopy"
                data-target-id="export-notebook-source">
                <.remix_icon icon="clipboard-line" class="text-lg" />
              </button>
            </span>
            <span class="tooltip left" aria-label="Download source">
              <a class="icon-button"
                href={Routes.session_path(@socket, :download_source, @session_id, "exs")}>
                <.remix_icon icon="download-2-line" class="text-lg" />
              </a>
            </span>
          </div>
        </div>
        <.code_preview
          source_id="export-notebook-source"
          language="elixir"
          source={@source} />
      </div>
    </div>
    """
  end
end
