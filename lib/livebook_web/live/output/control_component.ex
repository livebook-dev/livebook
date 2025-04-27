defmodule LivebookWeb.Output.ControlComponent do
  use LivebookWeb, :live_component

  @impl true
  def mount(socket) do
    {:ok, assign(socket, keyboard_enabled: false)}
  end

  @impl true
  def render(assigns) when assigns.control.attrs.type == :keyboard do
    ~H"""
    <div
      class="flex"
      id={"#{@id}-root"}
      phx-hook="KeyboardControl"
      data-p-cell-id={hook_prop(@cell_id)}
      data-p-default-handlers={hook_prop(@control.attrs.default_handlers)}
      data-p-keydown-enabled={hook_prop(@keyboard_enabled and :keydown in @control.attrs.events)}
      data-p-keyup-enabled={hook_prop(@keyboard_enabled and :keyup in @control.attrs.events)}
      data-p-target={hook_prop(@myself)}
    >
      <span class="tooltip right" data-tooltip="Toggle keyboard control">
        <.button
          color={if(@keyboard_enabled, do: "blue", else: "gray")}
          small
          type="button"
          aria-label="toggle keyboard control"
          phx-click={JS.push("toggle_keyboard", target: @myself)}
        >
          <.remix_icon icon="keyboard-line" class="text-xl leading-none py-1" />
        </.button>
      </span>
    </div>
    """
  end

  def render(assigns) when assigns.control.attrs.type == :button do
    ~H"""
    <div class="flex">
      <.button color="gray" type="button" phx-click={JS.push("button_click", target: @myself)}>
        {@control.attrs.label}
      </.button>
    </div>
    """
  end

  def render(assigns) when assigns.control.attrs.type == :form do
    ~H"""
    <div>
      <.live_component
        module={LivebookWeb.Output.ControlFormComponent}
        id={@id}
        control={@control}
        input_views={@input_views}
        session_pid={@session_pid}
        client_id={@client_id}
      />
    </div>
    """
  end

  def render(assigns) do
    ~H"""
    <div class="text-red-600">
      Unknown control type {@control.attrs.type}
    </div>
    """
  end

  @impl true
  def handle_event("toggle_keyboard", %{}, socket) do
    enabled = !socket.assigns.keyboard_enabled
    maybe_report_status(socket, enabled)
    {:noreply, assign(socket, keyboard_enabled: enabled)}
  end

  def handle_event("enable_keyboard", %{}, socket) do
    maybe_report_status(socket, true)
    {:noreply, assign(socket, keyboard_enabled: true)}
  end

  def handle_event("disable_keyboard", %{}, socket) do
    maybe_report_status(socket, false)
    {:noreply, assign(socket, keyboard_enabled: false)}
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

  defp maybe_report_status(socket, enabled) do
    %{assigns: %{control: %{attrs: attrs}, keyboard_enabled: current}} = socket

    if :status in attrs.events and enabled != current do
      report_event(socket, %{type: :status, enabled: enabled})
    end
  end

  defp report_event(socket, attrs) do
    topic = socket.assigns.control.ref
    event = Map.merge(%{origin: socket.assigns.client_id}, attrs)
    send(socket.assigns.control.destination, {:event, topic, event})
  end
end
