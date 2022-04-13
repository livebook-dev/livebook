defmodule LivebookWeb.SessionLive.CellEditorComponent do
  use LivebookWeb, :live_component

  @impl true
  def mount(socket) do
    {:ok, assign(socket, initialized: false)}
  end

  @impl true
  def update(assigns, socket) do
    socket =
      socket
      |> assign(assigns)
      |> assign_new(:intellisense, fn -> false end)
      |> assign_new(:read_only, fn -> false end)
      |> assign_new(:rounded, fn -> :both end)

    socket =
      if not connected?(socket) or socket.assigns.initialized do
        socket
      else
        socket
        |> push_event(
          "cell_editor_init:#{socket.assigns.cell_id}:#{socket.assigns.tag}",
          %{
            source_view: socket.assigns.source_view,
            language: socket.assigns.language,
            intellisense: socket.assigns.intellisense,
            read_only: socket.assigns.read_only
          }
        )
        |> assign(initialized: true)
      end

    {:ok, socket}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div id={"cell-editor-#{@id}"}
      phx-update="ignore"
      phx-hook="CellEditor"
      data-cell-id={@cell_id}
      data-tag={@tag}>
      <div class={"py-3 #{rounded_class(@rounded)} bg-editor"} data-el-editor-container>
        <div class="px-8" data-el-skeleton>
          <.content_skeleton bg_class="bg-gray-500" empty={empty?(@source_view)} />
        </div>
      </div>
    </div>
    """
  end

  defp empty?(%{source: ""} = _source_view), do: true
  defp empty?(_source_view), do: false

  defp rounded_class(:both), do: "rounded-lg"
  defp rounded_class(:top), do: "rounded-t-lg"
  defp rounded_class(:bottom), do: "rounded-b-lg"
end
