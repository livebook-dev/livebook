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
      # After initialization, output text may be pruned and updates
      # are sent as events instead
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

  defp format_style([]), do: nil

  defp format_style(enum) do
    for {key, value} <- enum,
        key in [:color, :font_weight, :font_size],
        value = to_string(value),
        not String.contains?(value, ";") do
      "#{key |> Atom.to_string() |> String.replace("_", "-")}: #{value}"
    end
    |> Enum.join("; ")
  end
end
