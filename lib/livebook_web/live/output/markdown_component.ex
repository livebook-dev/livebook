defmodule LivebookWeb.Output.MarkdownComponent do
  use LivebookWeb, :live_component

  @impl true
  def mount(socket) do
    {:ok,
     socket
     |> assign(allowed_uri_schemes: Livebook.Config.allowed_uri_schemes(), initialized: false)
     |> stream(:chunks, [])}
  end

  @impl true
  def update(%{event: {:append, output}}, socket) do
    {:ok, append_text(socket, output.text)}
  end

  def update(assigns, socket) do
    {output, assigns} = Map.pop(assigns, :output)
    socket = assign(socket, assigns)

    if socket.assigns.initialized do
      # After initialization, output text may be pruned and updates
      # are sent as events instead
      {:ok, socket}
    else
      {:ok,
       socket
       |> append_text(output.text)
       |> assign(:initialized, true)}
    end
  end

  defp append_text(socket, text) do
    chunk = %{id: Livebook.Utils.random_long_id(), text: text}
    stream_insert(socket, :chunks, chunk)
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div
      id={@id}
      phx-hook="MarkdownRenderer"
      data-p-base-path={hook_prop(~p"/sessions/#{@session_id}")}
      data-p-allowed-uri-schemes={hook_prop(@allowed_uri_schemes)}
    >
      <div
        data-template
        id={"#{@id}-template"}
        class="text-gray-700 whitespace-pre-wrap hidden"
        phx-update="stream"
        phx-no-format
      ><span :for={{dom_id, chunk}<- @streams.chunks} id={dom_id}>{chunk.text}</span></div>
      <div data-content class="markdown" id={"#{@id}-content"} phx-update="ignore"></div>
    </div>
    """
  end
end
