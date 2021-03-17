defmodule LivebookWeb.SessionLive.RuntimeComponent do
  use LivebookWeb, :live_component

  alias Livebook.{Session, Runtime}

  @impl true
  def mount(socket) do
    {:ok, assign(socket, type: "elixir_standalone")}
  end

  @impl true
  def render(assigns) do
    ~L"""
    <div class="w-full flex-col space-y-3">
      <p class="text-gray-700">
        The code is evaluated in a separate Elixir runtime (node),
        which you can configure yourself here.
      </p>
      <div class="flex items-center justify-between border border-gray-200 rounded-lg p-4">
        <%= if @runtime do %>
          <div class="flex flex-col space-y-1">
            <span class="text-xs text-gray-500">
              Type
            </span>
            <span class="text-gray-800 text-sm font-semibold">
              <%= runtime_type_label(@runtime) %>
            </span>
          </div>
          <div class="flex flex-col space-y-1">
            <span class="text-xs text-gray-500">
              Node name
            </span>
            <span class="text-gray-800 text-sm font-semibold">
              <%= @runtime.node %>
            </span>
          </div>
          <button class="button-base text-sm button-danger"
            type="button"
            phx-click="disconnect"
            phx-target="<%= @myself %>">
            Disconnect
          </button>
        <% else %>
          <p class="text-sm text-gray-700">
            No connected node
          </p>
        <% end %>
      </div>
      <form phx-change="set_runtime_type" phx-target="<%= @myself %>">
        <div class="radio-button-group">
          <label class="radio-button">
            <%= tag :input, class: "radio-button__input", type: "radio", name: "type", value: "elixir_standalone", checked: @type == "elixir_standalone" %>
            <span class="radio-button__label">Elixir standalone</span>
          </label>
          <label class="radio-button">
            <%= tag :input, class: "radio-button__input", type: "radio", name: "type", value: "mix_standalone", checked: @type == "mix_standalone" %>
            <span class="radio-button__label">Mix standalone</span>
          </label>
          <label class="radio-button">
            <%= tag :input, class: "radio-button__input", type: "radio", name: "type", value: "attached", checked: @type == "attached" %>
            <span class="radio-button__label">Attached node</span>
          </label>
        </div>
      </form>
      <div>
        <%= if @type == "elixir_standalone" do %>
          <%= live_render @socket, LivebookWeb.SessionLive.ElixirStandaloneLive,
            id: :elixir_standalone_runtime,
            session: %{"session_id" => @session_id} %>
        <% end %>
        <%= if @type == "mix_standalone" do %>
          <%= live_render @socket, LivebookWeb.SessionLive.MixStandaloneLive,
            id: :mix_standalone_runtime,
            session: %{"session_id" => @session_id} %>
        <% end %>
        <%= if @type == "attached" do %>
          <%= live_render @socket, LivebookWeb.SessionLive.AttachedLive,
            id: :attached_runtime,
            session: %{"session_id" => @session_id} %>
        <% end %>
      </div>
    </div>
    """
  end

  defp runtime_type_label(%Runtime.ElixirStandalone{}), do: "Elixir standalone"
  defp runtime_type_label(%Runtime.MixStandalone{}), do: "Mix standalone"
  defp runtime_type_label(%Runtime.Attached{}), do: "Attached"

  @impl true
  def handle_event("set_runtime_type", %{"type" => type}, socket) do
    {:noreply, assign(socket, type: type)}
  end

  def handle_event("disconnect", _params, socket) do
    Session.disconnect_runtime(socket.assigns.session_id)

    {:noreply, socket}
  end
end
