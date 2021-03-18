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
      <div class="flex space-x-4">
        <%= content_tag :button, "Elixir standalone",
          class: "choice-button #{if(@type == "elixir_standalone", do: "active")}",
          phx_click: "set_runtime_type",
          phx_value_type: "elixir_standalone",
          phx_target: @myself %>
        <%= content_tag :button, "Mix standalone",
          class: "choice-button #{if(@type == "mix_standalone", do: "active")}",
          phx_click: "set_runtime_type",
          phx_value_type: "mix_standalone",
          phx_target: @myself %>
        <%= content_tag :button, "Attached node",
          class: "choice-button #{if(@type == "attached", do: "active")}",
          phx_click: "set_runtime_type",
          phx_value_type: "attached",
          phx_target: @myself %>
      </div>
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
