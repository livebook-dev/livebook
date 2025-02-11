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
  def update(%{event: {:append, output}}, socket) do
    {:ok, append_output(socket, output)}
  end

  def update(assigns, socket) do
    {output, assigns} = Map.pop(assigns, :output)
    socket = assign(socket, assigns)

    if socket.assigns.initialized do
      # After initialization, .text may be pruned
      {:ok, socket}
    else
      {:ok,
       socket
       |> append_output(output)
       |> assign(:initialized, true)}
    end
  end

  defp append_output(socket, output) do
    chunk = %{
      id: Livebook.Utils.random_long_id(),
      text: output.text,
      style: output.style
    }

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
      :for={{dom_id, chunk}<- @streams.chunks} id={dom_id} style={format_style(chunk.style)}>{chunk.text}</span></div>
    """
  end

  defp format_style(enum) do
    Enum.map_join(enum, "; ", fn {key, value} when key in [:color, :font_weight, :font_size] ->
      value = to_string(value)

      if String.contains?(value, ";") do
        raise ArgumentError, "invalid CSS property value"
      end

      "#{key |> Atom.to_string() |> String.replace("_", "-")}: #{value}"
    end)
  end
end
