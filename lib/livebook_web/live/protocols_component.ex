defmodule LivebookWeb.ProtocolsComponent do
  alias Livebook.Settings
  use LivebookWeb, :live_component

  @impl true
  def mount(socket) do
    {:ok,
     socket
     |> assign(submit_event: nil)}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div>
      <form
        class="grow"
        phx-change="set_protocol"
        phx-submit={if @submit_event, do: "submit"}
        onsubmit={unless @submit_event, do: "return false"}
        phx-target={@myself}
      >
        <input
          class="input"
          id="input-protocol"
          aria-label="custom protocols"
          type="text"
          name="protocol"
          placeholder="e.g rtsp, svn, tel, fax, xmpp"
          value={@protocol}
          spellcheck="false"
          autocomplete="off"
        />
      </form>
    </div>
    """
  end

  @impl true
  def handle_event("set_protocol", %{"protocol" => protocol}, socket) do
    Settings.save_custom_protocol(protocol)
    {:noreply, socket}
  end
end
