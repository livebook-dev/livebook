defmodule LivebookWeb.Output.TabsComponent do
  use LivebookWeb, :live_component

  @impl true
  def mount(socket) do
    {:ok, stream(socket, :outputs, [])}
  end

  @impl true
  def update(assigns, socket) do
    {labels, assigns} = Map.pop!(assigns, :labels)
    {outputs, assigns} = Map.pop!(assigns, :outputs)

    # We compute these only on initial render, when we have all outputs
    socket =
      socket
      |> assign_new(:labels, fn ->
        Enum.zip_with(labels, outputs, fn label, {output_idx, _} -> {output_idx, label} end)
      end)
      |> assign_new(:active_idx, fn ->
        get_in(outputs, [Access.at(0), Access.elem(0)])
      end)

    socket = assign(socket, assigns)

    stream_items =
      for {idx, output} <- outputs do
        id = "#{idx}-tabs-item"
        %{id: id, idx: idx, output: output}
      end

    socket = stream(socket, :outputs, stream_items)

    {:ok, socket}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div id={@id}>
      <div class="tabs mb-2" id={"#{@id}-tabs"}>
        <%!-- Note that we use > in the phx-click selectors, because there may be nested tabs --%>
        <button
          :for={{output_idx, label} <- @labels}
          id={"#{@id}-tabs-#{output_idx}"}
          class={["tab", output_idx == @active_idx && "active"]}
          phx-click={
            JS.remove_class("active", to: "##{@id}-tabs > .tab.active")
            |> JS.add_class("active")
            |> JS.add_class("hidden", to: "##{@id}-tab-contents > [data-tab-content]:not(.hidden)")
            |> JS.remove_class("hidden",
              to: ~s/##{@id}-tab-contents > [data-tab-content="#{output_idx}"]/
            )
          }
        >
          {label}
        </button>
      </div>
      <div id={"#{@id}-tab-contents"} phx-update="stream">
        <div
          :for={{dom_id, output} <- @streams.outputs}
          id={dom_id}
          data-tab-content={output.idx}
          class={[output.idx != @active_idx && "hidden"]}
        >
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
end
