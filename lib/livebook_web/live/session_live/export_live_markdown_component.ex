defmodule LivebookWeb.SessionLive.ExportLiveMarkdownComponent do
  use LivebookWeb, :live_component

  @impl true
  def mount(socket) do
    {:ok, assign(socket, include_outputs: false)}
  end

  @impl true
  def update(assigns, socket) do
    {:ok, socket |> assign(assigns) |> assign_source()}
  end

  defp assign_source(%{assigns: assigns} = socket) do
    source =
      Livebook.LiveMarkdown.Export.notebook_to_markdown(assigns.notebook,
        include_outputs: assigns.include_outputs
      )

    assign(socket, :source, source)
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="flex flex-col space-y-6">
      <div class="flex">
        <form phx-change="set_options" onsubmit="return false;" phx-target={@myself}>
          <.switch_checkbox
            name="include_outputs"
            label="Include outputs"
            checked={@include_outputs} />
        </form>
      </div>
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
                href={Routes.session_path(@socket, :download_source, @session_id, "livemd", include_outputs: @include_outputs)}>
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
    </div>
    """
  end

  @impl true
  def handle_event("set_options", %{"include_outputs" => include_outputs}, socket) do
    include_outputs = include_outputs == "true"
    {:noreply, socket |> assign(include_outputs: include_outputs) |> assign_source()}
  end
end
