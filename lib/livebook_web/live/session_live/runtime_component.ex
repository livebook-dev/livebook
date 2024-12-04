defmodule LivebookWeb.SessionLive.RuntimeComponent do
  use LivebookWeb, :live_component

  alias Livebook.Runtime

  @impl true
  def mount(socket) do
    {:ok, assign(socket, error_message: nil)}
  end

  @impl true
  def update(%{event: {:error, message}}, socket) do
    {:ok, assign(socket, error_message: message)}
  end

  def update(assigns, socket) do
    with %{runtime_status: :connecting} <- socket.assigns,
         %{runtime_status: :connected} <- assigns,
         true <- socket.assigns.type == runtime_type(assigns.runtime) do
      send(self(), {:push_patch, socket.assigns.return_to})
    end

    socket =
      socket
      |> assign(assigns)
      |> assign_new(:type, fn -> runtime_type(assigns.runtime) end)

    {:ok, socket}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="flex flex-col space-y-5">
      <h3 class="text-2xl font-semibold text-gray-800">
        Runtime settings
      </h3>
      <div class="w-full flex-col space-y-5">
        <div class="flex space-x-4">
          <.choice_button
            :if={Livebook.Config.runtime_enabled?(Livebook.Runtime.Standalone)}
            active={@type == "standalone"}
            phx-click="set_runtime_type"
            phx-value-type="standalone"
            phx-target={@myself}
          >
            Standalone
          </.choice_button>
          <.choice_button
            :if={Livebook.Config.runtime_enabled?(Livebook.Runtime.Attached)}
            active={@type == "attached"}
            phx-click="set_runtime_type"
            phx-value-type="attached"
            phx-target={@myself}
          >
            Attached node
          </.choice_button>
          <.choice_button
            :if={Livebook.Config.runtime_enabled?(Livebook.Runtime.Embedded)}
            active={@type == "embedded"}
            phx-click="set_runtime_type"
            phx-value-type="embedded"
            phx-target={@myself}
          >
            Embedded
          </.choice_button>
          <.choice_button
            :if={Livebook.Config.runtime_enabled?(Livebook.Runtime.Fly)}
            active={@type == "fly"}
            phx-click="set_runtime_type"
            phx-value-type="fly"
            phx-target={@myself}
          >
            Fly.io machine
          </.choice_button>
          <.choice_button
            :if={Livebook.Config.runtime_enabled?(Livebook.Runtime.K8s)}
            active={@type == "k8s"}
            phx-click="set_runtime_type"
            phx-value-type="k8s"
            phx-target={@myself}
          >
            Kubernetes Pod
          </.choice_button>
        </div>
        <div
          :if={@error_message && @type == runtime_type(@runtime) && @runtime_status == :disconnected}
          id="runtime-connection-error"
          class="error-box scroll-mt-4"
          phx-mounted={JS.dispatch("lb:scroll_into_view", detail: %{behavior: "instant"})}
        >
          {@error_message}
        </div>
        <div>
          <.live_component
            id={"runtime-config-#{@type}"}
            module={component_for_type(@type)}
            session={@session}
            runtime={@runtime}
            runtime_status={@runtime_status}
            runtime_connect_info={@runtime_connect_info}
            hub={@hub}
            hub_secrets={@hub_secrets}
          />
        </div>
      </div>
    </div>
    """
  end

  defp runtime_type(%Runtime.Standalone{}), do: "standalone"
  defp runtime_type(%Runtime.Attached{}), do: "attached"
  defp runtime_type(%Runtime.Embedded{}), do: "embedded"
  defp runtime_type(%Runtime.Fly{}), do: "fly"
  defp runtime_type(%Runtime.K8s{}), do: "k8s"

  defp component_for_type("standalone"), do: LivebookWeb.SessionLive.StandaloneRuntimeComponent
  defp component_for_type("attached"), do: LivebookWeb.SessionLive.AttachedRuntimeComponent
  defp component_for_type("embedded"), do: LivebookWeb.SessionLive.EmbeddedRuntimeComponent
  defp component_for_type("fly"), do: LivebookWeb.SessionLive.FlyRuntimeComponent
  defp component_for_type("k8s"), do: LivebookWeb.SessionLive.K8sRuntimeComponent

  @impl true
  def handle_event("set_runtime_type", %{"type" => type}, socket) do
    {:noreply, assign(socket, type: type)}
  end
end
