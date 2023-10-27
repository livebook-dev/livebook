defmodule LivebookWeb.Output.PlainTextComponent do
  use LivebookWeb, :live_component

  @impl true
  def mount(socket) do
    {:ok, stream(socket, :chunks, [])}
  end

  @impl true
  def update(assigns, socket) do
    {text, assigns} = Map.pop(assigns, :text)

    socket = assign(socket, assigns)

    if text do
      chunk = %{id: Livebook.Utils.random_id(), text: text}
      {:ok, stream_insert(socket, :chunks, chunk)}
    else
      {:ok, socket}
    end
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
      :for={{dom_id, chunk}<- @streams.chunks} id={dom_id}><%= chunk.text %></span></div>
    """
  end
end
