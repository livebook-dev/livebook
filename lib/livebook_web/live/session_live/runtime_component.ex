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
    <div class="p-6 pb-4 max-w-4xl w-screen flex flex-col space-y-3">
      <h3 class="text-lg font-medium text-gray-900">
        Runtime
      </h3>
      <div class="w-full flex-col space-y-3">
        <p class="text-gray-500">
          The code is evaluated in a separate Elixir runtime (node),
          which you can configure yourself here.
        </p>
        <div class="shadow rounded-md p-2">
          <%= if @runtime do %>
            <table class="w-full text-center text-sm">
              <thead>
                <tr>
                  <th class="w-1/3">Type</th>
                  <th class="w-1/3">Node name</th>
                  <th class="w-1/3"></th>
                </tr>
              </thead>
              <tbody>
                <tr>
                  <td><%= runtime_type_label(@runtime) %></td>
                  <td><%= @runtime.node %></td>
                  <td>
                    <button class="button-base text-sm button-sm button-danger"
                      type="button"
                      phx-click="disconnect"
                      phx-target="<%= @myself %>">
                      Disconnect
                    </button>
                  </td>
                </tr>
              </tbody>
            </table>
          <% else %>
            <p class="p-2 text-sm text-gray-500">
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
