defmodule LivebookWeb.HubLive.FlyComponent do
  use LivebookWeb, :live_component

  alias Livebook.Hubs
  alias Livebook.Hubs.{Fly, FlyClient}
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
          <%= password_input(f, :access_token,
            phx_change: "fetch_data",
            phx_debounce: "blur",
            phx_target: @myself,
            disabled: @operation == :edit,
            value: @data["access_token"],
            class: "input w-full",
            autofocus: true,
            spellcheck: "false",
            required: true,
            autocomplete: "off"
          ) %>
        </div>

        <%= if length(@apps) > 0 do %>
          <div class="flex flex-col space-y-1">
            <h3 class="text-gray-800 font-semibold">
              Application
            </h3>
            <%= select(f, :application_id, @select_options,
              class: "input",
              required: true,
              phx_target: @myself,
              phx_change: "select_app",
              disabled: @operation == :edit
            ) %>
          </div>

          <div class="grid grid-cols-1 md:grid-cols-2 gap-3">
            <div class="flex flex-col space-y-1">
              <h3 class="text-gray-800 font-semibold">
                Name
              </h3>
              <%= text_input(f, :hub_name, value: @data["hub_name"], class: "input", required: true) %>
            </div>

            <div class="flex flex-col space-y-1">
              <h3 class="text-gray-800 font-semibold">
                Color
              </h3>

              <div class="flex space-x-4 items-center">
                <div
                  class="border-[3px] rounded-lg p-1 flex justify-center items-center"
                  style={"border-color: #{@data["hub_color"]}"}
                >
                  <div class="rounded h-5 w-5" style={"background-color: #{@data["hub_color"]}"}>
                  </div>
                </div>
                <div class="relative grow">
                  <%= text_input(f, :hub_color,
                    value: @data["hub_color"],
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
    assign(socket, data: %{}, select_options: [], apps: [])
  end

  defp load_data(%{assigns: %{operation: :edit, hub: fly}} = socket) do
    data = %{
      "access_token" => fly.access_token,
      "application_id" => fly.application_id,
      "hub_name" => fly.hub_name,
      "hub_color" => fly.hub_color
    }

    {:ok, apps} = FlyClient.fetch_apps(fly.access_token)
    opts = select_options(apps, fly.application_id)

    assign(socket, data: data, selected_app: fly, select_options: opts, apps: apps)
  end

  @impl true
  def handle_event("fetch_data", %{"fly" => %{"access_token" => token}}, socket) do
    case FlyClient.fetch_apps(token) do
      {:ok, apps} ->
        data = %{"access_token" => token, "hub_color" => User.random_hex_color()}
        opts = select_options(apps)

        {:noreply, assign(socket, data: data, select_options: opts, apps: apps)}

      {:error, _} ->
        send(self(), {:flash_error, "Invalid Access Token"})
        {:noreply, assign(socket, data: %{}, select_options: [], apps: [])}
    end
  end

  def handle_event("randomize_color", _, socket) do
    data = Map.replace!(socket.assigns.data, "hub_color", User.random_hex_color())
    {:noreply, assign(socket, data: data)}
  end

  def handle_event("save_hub", %{"fly" => params}, socket) do
    params =
      if socket.assigns.data do
        Map.merge(socket.assigns.data, params)
      else
        params
      end

    case socket.assigns.operation do
      :new -> create_fly(socket, params)
      :edit -> save_fly(socket, params)
    end
  end

  def handle_event("select_app", %{"fly" => %{"application_id" => app_id}}, socket) do
    selected_app = Enum.find(socket.assigns.apps, &(&1.application_id == app_id))
    opts = select_options(socket.assigns.apps, app_id)

    {:noreply, assign(socket, selected_app: selected_app, select_options: opts)}
  end

  def handle_event("update_data", %{"fly" => data}, socket) do
    data =
      if socket.assigns.data do
        Map.merge(socket.assigns.data, data)
      else
        data
      end

    opts = select_options(socket.assigns.apps, data["application_id"])

    {:noreply, assign(socket, data: data, select_options: opts)}
  end

  defp select_options(hubs, app_id \\ nil) do
    disabled_option = [key: "Select one application", value: "", selected: true, disabled: true]

    options =
      for fly <- hubs do
        [
          key: "#{fly.organization_name} - #{fly.application_id}",
          value: fly.application_id,
          selected: fly.application_id == app_id
        ]
      end

    Enum.reverse(options ++ [disabled_option])
  end

  defp create_fly(socket, params) do
    if Hubs.hub_exists?(socket.assigns.selected_app.id) do
      send(self(), {:flash_error, "Application already exists"})
      {:noreply, assign(socket, data: params)}
    else
      save_fly(socket, params)
    end
  end

  defp save_fly(socket, params) do
    Fly.save_fly(socket.assigns.selected_app, params)
    opts = select_options(socket.assigns.apps, params["application_id"])

    {:noreply, assign(socket, data: params, select_options: opts)}
  end
end
