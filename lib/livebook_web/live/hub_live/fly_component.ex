defmodule LivebookWeb.HubLive.FlyComponent do
  use LivebookWeb, :live_component

  alias Livebook.Hubs
  alias Livebook.Hubs.Fly
  alias Livebook.Users.User

  @impl true
  def update(assigns, socket) do
    {:ok,
     socket
     |> assign(assigns)
     |> load_data()}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div>
      <.form
        id={@id}
        class="flex flex-col space-y-4"
        let={f}
        for={:fly}
        phx-submit="save_hub"
        phx-change="update_data"
        phx-target={@myself}
        phx-debounce="blur"
      >
        <div class="flex flex-col space-y-1">
          <h3 class="text-gray-800 font-semibold">
            Access Token
          </h3>
          <%= password_input(f, :token,
            phx_change: "fetch_data",
            phx_debounce: "blur",
            phx_target: @myself,
            value: @data["token"],
            class: "input w-full",
            autofocus: true,
            spellcheck: "false",
            required: true,
            autocomplete: "off"
          ) %>
        </div>

        <%= if length(@organizations) > 0 do %>
          <div class="flex flex-col space-y-1">
            <h3 class="text-gray-800 font-semibold">
              Organization
            </h3>
            <%= select(f, :id, @organizations, class: "input", required: true) %>
          </div>

          <div class="grid grid-cols-1 md:grid-cols-2 gap-3">
            <div class="flex flex-col space-y-1">
              <h3 class="text-gray-800 font-semibold">
                Name
              </h3>
              <%= text_input(f, :name, value: @data["name"], class: "input", required: true) %>
            </div>

            <div class="flex flex-col space-y-1">
              <h3 class="text-gray-800 font-semibold">
                Color
              </h3>

              <div class="flex space-x-4 items-center">
                <div
                  class="border-[3px] rounded-lg p-1 flex justify-center items-center"
                  style={"border-color: #{@data["color"]}"}
                >
                  <div class="rounded h-5 w-5" style={"background-color: #{@data["color"]}"}></div>
                </div>
                <div class="relative grow">
                  <%= text_input(f, :color,
                    value: @data["color"],
                    class: "input",
                    spellcheck: "false",
                    required: true,
                    maxlength: 7
                  ) %>
                  <button
                    class="icon-button absolute right-2 top-1"
                    type="button"
                    phx-click="randomize_color"
                    phx-target={@myself}
                  >
                    <.remix_icon icon="refresh-line" class="text-xl" />
                  </button>
                </div>
              </div>
            </div>
          </div>

          <%= submit("Save", class: "button-base button-blue", phx_disable_with: "Saving...") %>
        <% end %>
      </.form>
    </div>
    """
  end

  defp load_data(%{assigns: %{operation: :new}} = socket) do
    assign(socket, data: %{}, organizations: [], orgs_data: [])
  end

  defp load_data(%{assigns: %{operation: :edit, provider_id: id}} = socket) do
    fly = Hubs.fetch_fly!(id)
    organizations = [fly.organization]

    data =
      fly
      |> Map.from_struct()
      |> Map.delete(:organization)
      |> Jason.encode!()
      |> Jason.decode!()

    opts = select_options(organizations, fly.id)

    assign(socket, data: data, organizations: opts, orgs_data: organizations)
  end

  @impl true
  def handle_event("fetch_data", %{"fly" => %{"token" => token}}, socket) do
    case Fly.fetch_organizations(token) do
      {:ok, organizations} ->
        data = %{"token" => token, "color" => User.random_hex_color()}
        opts = select_options(organizations)

        {:noreply, assign(socket, data: data, organizations: opts, orgs_data: organizations)}

      {:error, _} ->
        send(self(), {:flash_error, "Invalid Access Token"})
        {:noreply, assign(socket, data: %{}, organizations: [], orgs_data: [])}
    end
  end

  def handle_event("randomize_color", _, socket) do
    data = put_in(socket.assigns.data, ["color"], User.random_hex_color())

    {:noreply, assign(socket, data: data)}
  end

  def handle_event("save_hub", %{"fly" => params}, socket) do
    case socket.assigns.operation do
      :new -> create_fly(socket, params)
      :edit -> update_fly(socket, params)
    end
  end

  def handle_event("update_data", %{"fly" => data}, socket) do
    opts = select_options(socket.assigns.orgs_data, data["id"])

    {:noreply, assign(socket, data: data, organizations: opts)}
  end

  defp select_options(orgs, org_id \\ nil) do
    for org <- orgs do
      opts = if org.id == org_id, do: [selected: true], else: []

      [key: org.name, value: org.id] ++ opts
    end
  end

  defp create_fly(socket, params) do
    case Enum.find(socket.assigns.orgs_data, &(&1.id == params["id"])) do
      nil ->
        send(self(), {:flash_error, "Internal Server Error"})
        {:noreply, assign(socket, data: params)}

      organization ->
        if Hubs.fly_exists?(organization) do
          send(self(), {:flash_error, "Organization already exists"})
          {:noreply, assign(socket, data: params)}
        else
          opts = select_options(socket.assigns.orgs_data, params["id"])

          Hubs.save_fly(%Fly{
            id: params["id"],
            name: params["name"],
            color: params["color"],
            token: params["token"],
            organization: organization
          })

          {:noreply, assign(socket, data: params, organizations: opts)}
        end
    end
  end

  defp update_fly(socket, params) do
    fly = Hubs.fetch_fly!("fly-" <> params["id"])
    Hubs.save_fly(%{fly | name: params["name"], color: params["color"]})

    {:noreply, socket}
  end
end
