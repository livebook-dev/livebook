defmodule LivebookWeb.SessionLive.RuntimeComponent do
  use LivebookWeb, :live_component

  alias Livebook.{Session, Runtime}

  @impl true
  def mount(socket) do
    {:ok, assign(socket, type: nil)}
  end

  @impl true
  def update(assigns, socket) do
    assigns =
      if socket.assigns.type == nil do
        type =
          if assigns.runtime do
            runtime_type(assigns.runtime)
          else
            "elixir_standalone"
          end

        Map.put(assigns, :type, type)
      else
        assigns
      end

    {:ok, assign(socket, assigns)}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="p-6 pb-4 max-w-4xl flex flex-col space-y-3">
      <h3 class="text-2xl font-semibold text-gray-800">
        Runtime
      </h3>
      <div class="w-full flex-col space-y-5">
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
            <button class="button button-outlined-red"
              type="button"
              phx-click="disconnect"
              phx-target={@myself}>
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
          <%= content_tag :button, "Embedded",
            class: "choice-button #{if(@type == "embedded", do: "active")}",
            phx_click: "set_runtime_type",
            phx_value_type: "embedded",
            phx_target: @myself %>
        </div>
        <div>
          <%= live_render @socket, live_view_for_type(@type),
                id: "runtime-config-#{@type}",
                session: %{"session_id" => @session_id, "current_runtime" => @runtime} %>
        </div>
      </div>
    </div>
    """
  end

  defp runtime_type_label(%Runtime.ElixirStandalone{}), do: "Elixir standalone"
  defp runtime_type_label(%Runtime.MixStandalone{}), do: "Mix standalone"
  defp runtime_type_label(%Runtime.Attached{}), do: "Attached"
  defp runtime_type_label(%Runtime.Embedded{}), do: "Embedded"

  defp runtime_type(%Runtime.ElixirStandalone{}), do: "elixir_standalone"
  defp runtime_type(%Runtime.MixStandalone{}), do: "mix_standalone"
  defp runtime_type(%Runtime.Attached{}), do: "attached"
  defp runtime_type(%Runtime.Embedded{}), do: "embedded"

  defp live_view_for_type("elixir_standalone"), do: LivebookWeb.SessionLive.ElixirStandaloneLive
  defp live_view_for_type("mix_standalone"), do: LivebookWeb.SessionLive.MixStandaloneLive
  defp live_view_for_type("attached"), do: LivebookWeb.SessionLive.AttachedLive
  defp live_view_for_type("embedded"), do: LivebookWeb.SessionLive.EmbeddedLive

  @impl true
  def handle_event("set_runtime_type", %{"type" => type}, socket) do
    {:noreply, assign(socket, type: type)}
  end

  def handle_event("disconnect", _params, socket) do
    Session.disconnect_runtime(socket.assigns.session_id)

    {:noreply, socket}
  end
end
