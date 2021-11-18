defmodule LivebookWeb.Output.ControlsDynamicLive do
  use LivebookWeb, :live_view

  @impl true
  def mount(_params, %{"pid" => pid, "id" => id}, socket) do
    if connected?(socket) do
      send(pid, {:connect, self()})
    end

    {:ok, assign(socket, id: id, pid: pid, keyboard_events: [], controls: [])}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="flex space-x-2"
      id={"#{@id}-root"}
      phx-hook="Controls"
      data-keydown-enabled={to_string(:keydown in @keyboard_events)}
      data-keyup-enabled={to_string(:keyup in @keyboard_events)}>
      <%= for {control, idx} <- Enum.with_index(@controls) do %>
        <.control control={control} idx={idx} keyboard_enabled={@keyboard_events != []} />
      <% end %>
    </div>
    """
  end

  defp control(%{control: %{type: :keyboard}} = assigns) do
    ~H"""
    <span class="tooltip right" data-tooltip="Toggle keyboard control">
      <button class={"button #{if @keyboard_enabled, do: "button-blue", else: "button-gray"} button-square-icon"}
        type="button"
        aria-label="toggle keyboard control"
        phx-click={JS.push("toggle_keyboard", value: %{idx: @idx})}>
        <.remix_icon icon="keyboard-line" />
      </button>
    </span>
    """
  end

  defp control(%{control: %{type: :button}} = assigns) do
    ~H"""
    <button class="button button-gray"
      type="button"
      phx-click={JS.push("button_click", value: %{idx: @idx})}>
      <%= @control.label %>
    </button>
    """
  end

  defp control(assigns), do: ~H""

  @impl true
  def handle_event("toggle_keyboard", %{"idx" => idx}, socket) do
    keyboard = Enum.at(socket.assigns.controls, idx)

    keyboard_events =
      case socket.assigns.keyboard_events do
        [] -> keyboard.events
        _ -> []
      end

    {:noreply, assign(socket, keyboard_events: keyboard_events)}
  end

  def handle_event("keydown", %{"key" => key}, socket) do
    report_event(socket, :keydown, %{key: key})
    {:noreply, socket}
  end

  def handle_event("keyup", %{"key" => key}, socket) do
    report_event(socket, :keyup, %{key: key})
    {:noreply, socket}
  end

  def handle_event("button_click", %{"idx" => idx}, socket) do
    button = Enum.at(socket.assigns.controls, idx)
    report_event(socket, button.event)
    {:noreply, socket}
  end

  @impl true
  def handle_info({:connect_reply, %{controls: controls}}, socket) do
    {:noreply, assign(socket, controls: controls)}
  end

  defp report_event(socket, type, attrs \\ %{}) do
    event = Map.merge(%{type: type, origin: self()}, attrs)
    send(socket.assigns.pid, {:event, event})
  end
end
