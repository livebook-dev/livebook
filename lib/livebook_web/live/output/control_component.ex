defmodule LivebookWeb.Output.ControlComponent do
  use LivebookWeb, :live_component

  @impl true
  def mount(socket) do
    {:ok, assign(socket, keyboard_enabled: false)}
  end

  @impl true
  def render(%{attrs: %{type: :keyboard}} = assigns) do
    ~H"""
    <div
      class="flex"
      id={"#{@id}-root"}
      phx-hook="KeyboardControl"
      data-keydown-enabled={to_string(@keyboard_enabled and :keydown in @attrs.events)}
      data-keyup-enabled={to_string(@keyboard_enabled and :keyup in @attrs.events)}
      data-target={@myself}
    >
      <span class="tooltip right" data-tooltip="Toggle keyboard control">
        <button
          class={
            "button-base #{if @keyboard_enabled, do: "button-blue", else: "button-gray"} button-square-icon"
          }
          type="button"
          aria-label="toggle keyboard control"
          phx-click={JS.push("toggle_keyboard", target: @myself)}
        >
          <.remix_icon icon="keyboard-line" />
        </button>
      </span>
    </div>
    """
  end

  def render(%{attrs: %{type: :button}} = assigns) do
    ~H"""
    <div class="flex">
      <button
        class="button-base button-gray"
        type="button"
        phx-click={JS.push("button_click", target: @myself)}
      >
        <%= @attrs.label %>
      </button>
    </div>
    """
  end

  def render(%{attrs: %{type: :form}} = assigns) do
    ~H"""
    <div>
      <.live_component
        module={LivebookWeb.Output.ControlFormComponent}
        id={@id}
        attrs={@attrs}
        input_values={@input_values}
      />
    </div>
    """
  end

  def render(assigns) do
    ~H"""
    <div class="text-red-600">
      Unknown control type <%= @attrs.type %>
    </div>
    """
  end

  @impl true
  def handle_event("toggle_keyboard", %{}, socket) do
    socket = update(socket, :keyboard_enabled, &not/1)
    maybe_report_status(socket)
    {:noreply, socket}
  end

  def handle_event("disable_keyboard", %{}, socket) do
    socket =
      if socket.assigns.keyboard_enabled do
        socket = assign(socket, keyboard_enabled: false)
        maybe_report_status(socket)
        socket
      else
        socket
      end

    {:noreply, socket}
  end

  def handle_event("button_click", %{}, socket) do
    report_event(socket, %{type: :click})
    {:noreply, socket}
  end

  def handle_event("keydown", %{"key" => key}, socket) do
    report_event(socket, %{type: :keydown, key: key})
    {:noreply, socket}
  end

  def handle_event("keyup", %{"key" => key}, socket) do
    report_event(socket, %{type: :keyup, key: key})
    {:noreply, socket}
  end

  defp maybe_report_status(socket) do
    if :status in socket.assigns.attrs.events do
      report_event(socket, %{type: :status, enabled: socket.assigns.keyboard_enabled})
    end
  end

  defp report_event(socket, attrs) do
    topic = socket.assigns.attrs.ref
    event = Map.merge(%{origin: self()}, attrs)
    send(socket.assigns.attrs.destination, {:event, topic, event})
  end
end
