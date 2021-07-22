defmodule LivebookWeb.SessionLive.ExportComponent do
  use LivebookWeb, :live_component

  alias Livebook.Session

  @impl true
  def update(assigns, socket) do
    socket = assign(socket, assigns)

    socket =
      if socket.assigns[:source] do
        socket
      else
        # Note: we need to load the notebook, because the local data
        # has cell contents stripped out
        notebook = Session.get_notebook(socket.assigns.session_id)
        source = Livebook.LiveMarkdown.Export.notebook_to_markdown(notebook)
        assign(socket, :source, source)
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
                  href={Routes.session_path(@socket, :download_source, @session_id)}>
                  <.remix_icon icon="download-2-line" class="text-lg" />
                </a>
              </span>
            </div>
          </div>
          <div class="markdown">
            <pre><code
              class="tiny-scrollbar"
              id="export-notebook-source"
              phx-hook="Highlight"
              data-language="markdown"><%= @source %></code></pre>
          </div>
        </div>
      </div>
    </div>
    """
  end
end
