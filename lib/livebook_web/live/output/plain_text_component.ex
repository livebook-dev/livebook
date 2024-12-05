defmodule LivebookWeb.Output.PlainTextComponent do
  use LivebookWeb, :live_component

  @impl true
  def mount(socket) do
    {:ok,
     socket
     |> assign(initialized: false)
     |> stream(:chunks, [])}
  end

  @impl true
  def update(%{event: {:append, text}}, socket) do
    {:ok, append_text(socket, text)}
  end

  def update(assigns, socket) do
    {text, assigns} = Map.pop(assigns, :text)
    socket = assign(socket, assigns)

    if socket.assigns.initialized do
      {:ok, socket}
    else
      {:ok,
       socket
       |> append_text(text)
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
      class="text-gray-700 whitespace-pre-wrap break-words"
      phx-update="stream"
      phx-no-format
    ><span
      :for={{dom_id, chunk}<- @streams.chunks} id={dom_id}>{chunk.text}</span></div>
    """
  end
end
