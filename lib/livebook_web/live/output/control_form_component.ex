defmodule LivebookWeb.Output.ControlFormComponent do
  use LivebookWeb, :live_component

  @impl true
  def mount(socket) do
    {:ok, assign(socket, data: nil)}
  end

  @impl true
  def update(assigns, socket) do
    prev_data = socket.assigns.data

    socket = assign(socket, assigns)

    data =
      Map.new(assigns.control.attrs.fields, fn
        {field, nil} -> {field, nil}
        {field, input} -> {field, assigns.input_views[input.id].value}
      end)

    if prev_data != nil and data != prev_data do
      change_data =
        for {field, value} <- data,
            assigns.control.attrs.report_changes[field],
            into: %{},
            do: {field, value}

      if change_data != %{} do
        report_event(socket, %{type: :change, data: change_data})
      end
    end

    {:ok, assign(socket, data: data)}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="flex flex-col space-y-3">
      <.live_component
        :for={{_field, input} <- @control.attrs.fields}
        :if={input}
        module={LivebookWeb.Output.InputComponent}
        id={"#{@id}-#{input.id}"}
        input={input}
        input_views={@input_views}
        session_pid={@session_pid}
        client_id={@client_id}
        local={true}
      />
      <div :if={@control.attrs.submit}>
        <.button type="button" phx-click="submit" phx-target={@myself}>
          {@control.attrs.submit}
        </.button>
      </div>
    </div>
    """
  end

  @impl true
  def handle_event("submit", %{}, socket) do
    report_event(socket, %{type: :submit, data: socket.assigns.data})

    if socket.assigns.control.attrs.reset_on_submit do
      reset_inputs(socket)
    end

    {:noreply, socket}
  end

  defp report_event(socket, attrs) do
    topic = socket.assigns.control.ref
    event = Map.merge(%{origin: socket.assigns.client_id}, attrs)
    send(socket.assigns.control.destination, {:event, topic, event})
  end

  defp reset_inputs(socket) do
    values =
      for {field, input} <- socket.assigns.control.attrs.fields,
          input != nil and field in socket.assigns.control.attrs.reset_on_submit,
          do: {input.id, input.attrs.default}

    send(self(), {:set_input_values, values, true})
  end
end
