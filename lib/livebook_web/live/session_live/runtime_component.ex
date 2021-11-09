defmodule LivebookWeb.SessionLive.RuntimeComponent do
  use LivebookWeb, :live_component

  alias Livebook.Runtime

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
            {runtime_module, _args} = Livebook.Config.default_runtime()
            runtime_module |> struct() |> runtime_type()
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
    <div class="p-6 pb-4 max-w-4xl flex flex-col space-y-5">
      <h3 class="text-2xl font-semibold text-gray-800">
        Runtime
      </h3>
      <div class="w-full flex-col space-y-5">
        <div class="flex space-x-4">
          <.choice_button
            active={@type == "elixir_standalone"}
            phx-click="set_runtime_type"
            phx-value-type="elixir_standalone"
            phx-target={@myself}>
            Elixir standalone
          </.choice_button>
          <.choice_button
            active={@type == "mix_standalone"}
            phx-click="set_runtime_type"
            phx-value-type="mix_standalone"
            phx-target={@myself}>
            Mix standalone
          </.choice_button>
          <.choice_button
            active={@type == "attached"}
            phx-click="set_runtime_type"
            phx-value-type="attached"
            phx-target={@myself}>
            Attached node
          </.choice_button>
          <.choice_button
            active={@type == "embedded"}
            phx-click="set_runtime_type"
            phx-value-type="embedded"
            phx-target={@myself}>
            Embedded
          </.choice_button>
        </div>
        <div>
          <%= live_render @socket, live_view_for_type(@type),
                id: "runtime-config-#{@type}",
                session: %{"session" => @session, "current_runtime" => @runtime} %>
        </div>
      </div>
    </div>
    """
  end

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
end
