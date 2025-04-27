defmodule LivebookWeb.SessionLive.ExportLiveMarkdownComponent do
  use LivebookWeb, :live_component

  alias Livebook.Session

  @impl true
  def update(assigns, socket) do
    socket = assign(socket, assigns)

    {:ok,
     socket
     |> assign_new(:include_outputs, fn -> socket.assigns.notebook.persist_outputs end)
     |> assign_source()}
  end

  defp assign_source(%{assigns: assigns} = socket) do
    {source, _warnings} =
      Livebook.LiveMarkdown.notebook_to_livemd(assigns.notebook,
        include_outputs: assigns.include_outputs
      )

    assign(socket, :source, source)
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="flex flex-col space-y-6">
      <div class="flex">
        <form phx-change="set_options" phx-nosubmit phx-target={@myself}>
          <.switch_field name="include_outputs" label="Include outputs" value={@include_outputs} />
        </form>
      </div>
      <.message_box
        :if={@include_outputs and @any_stale_cell?}
        kind="warning"
        message="There are stale cells, some outputs may be inaccurate. You may want to reevaluate the notebook to make sure the outputs are up to date."
      />
      <div class="flex flex-col space-y-1">
        <div class="flex justify-between items-center">
          <span class="text-sm text-gray-700 font-semibold">
            {Session.file_name_for_download(@session) <> ".livemd"}
          </span>
          <div class="flex justify-end space-x-2">
            <span class="tooltip left" data-tooltip="Copy source">
              <.icon_button
                aria-label="copy source"
                phx-click={JS.dispatch("lb:clipcopy", to: "#export-notebook-source")}
              >
                <.remix_icon icon="clipboard-line" />
              </.icon_button>
            </span>
            <span class="tooltip left" data-tooltip="Download source">
              <.icon_button
                aria-label="download source"
                href={
                  ~p"/sessions/#{@session.id}/download/export/livemd?include_outputs=#{@include_outputs}"
                }
                download
              >
                <.remix_icon icon="download-2-line" />
              </.icon_button>
            </span>
          </div>
        </div>
        <div class="markdown">
          <.code_preview source_id="export-notebook-source" language="markdown" source={@source} />
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
