defmodule LivebookWeb.Output.GridComponent do
  use LivebookWeb, :live_component

  @impl true
  def mount(socket) do
    {:ok, stream(socket, :outputs, [])}
  end

  @impl true
  def update(assigns, socket) do
    {outputs, assigns} = Map.pop!(assigns, :outputs)

    socket = assign(socket, assigns)

    stream_items =
      for {idx, output} <- outputs do
        id = "#{idx}-grid-item"
        %{id: id, idx: idx, output: output}
      end

    socket = stream(socket, :outputs, stream_items)

    {:ok, socket}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div
      id={@id}
      phx-hook="ScrollOnUpdate"
      class="overflow-auto tiny-scrollbar pr-1.5 -mr-1.5"
      style={max_height_style(@max_height)}
    >
      <div
        id={"#{@id}-grid"}
        class="grid grid-cols-2 w-full"
        style={join_styles([columns_style(@columns), gap_style(@gap)])}
        phx-update="stream"
      >
        <div :for={{dom_id, output} <- @streams.outputs} id={dom_id}>
          <LivebookWeb.Output.output
            id={"outputs-#{output.idx}"}
            output={output.output}
            session_id={@session_id}
            session_pid={@session_pid}
            input_views={@input_views}
            client_id={@client_id}
            cell_id={@cell_id}
          />
        </div>
      </div>
    </div>
    """
  end

  defp columns_style(columns) when is_tuple(columns) do
    columns = Tuple.to_list(columns)

    if Enum.all?(columns, &is_integer/1) do
      template = Enum.map_join(columns, " ", fn n -> "minmax(0, #{n}fr)" end)
      "grid-template-columns: #{template}"
    end
  end

  defp columns_style(columns) when is_integer(columns) do
    "grid-template-columns: repeat(#{columns}, minmax(0, 1fr))"
  end

  defp columns_style(_columns), do: nil

  defp gap_style(gap) when is_integer(gap) do
    "gap: #{gap}px"
  end

  defp gap_style(_other), do: nil

  defp max_height_style(max_height) when is_integer(max_height) do
    "max-height: #{max_height}px"
  end

  defp max_height_style(_other), do: nil

  defp join_styles(styles) do
    styles
    |> Enum.reject(&is_nil/1)
    |> Enum.join("; ")
  end
end
