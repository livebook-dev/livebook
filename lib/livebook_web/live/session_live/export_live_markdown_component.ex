defmodule LivebookWeb.SessionLive.ExportLiveMarkdownComponent do
  use LivebookWeb, :live_component

  @impl true
  def update(assigns, socket) do
    socket = assign(socket, assigns)

    source = Livebook.LiveMarkdown.Export.notebook_to_markdown(socket.assigns.notebook)
    socket = assign(socket, :source, source)

    {:ok, socket}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="flex flex-col space-y-1">
      <div class="flex justify-between items-center">
        <span class="text-sm text-gray-700 font-semibold">
          .livemd
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
              href={Routes.session_path(@socket, :download_source, @session_id, "livemd")}>
              <.remix_icon icon="download-2-line" class="text-lg" />
            </a>
          </span>
        </div>
      </div>
      <div class="markdown">
      <.code_preview
        source_id="export-notebook-source"
        language="markdown"
        source={@source} />
      </div>
    </div>
    """
  end
end
