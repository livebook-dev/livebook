defmodule LivebookWeb.Hub.New.FlyComponent do
  use LivebookWeb, :live_component

  import Ecto.Changeset, only: [get_field: 2, add_error: 3]

  alias Livebook.EctoTypes.HexColor
  alias Livebook.Hubs.{Fly, FlyClient}

  @impl true
  def update(assigns, socket) do
    {:ok,
     socket
     |> assign(assigns)
     |> assign(
       changeset: Fly.change_hub(%Fly{}),
       selected_app: nil,
       select_options: [],
       apps: [],
       valid?: false
     )}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div>
      <.form
        id={@id}
        class="flex flex-col space-y-4"
        let={f}
        for={@changeset}
        phx-submit="save"
        phx-change="validate"
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
            value: access_token(@changeset),
            class: "input w-full",
            autofocus: true,
            spellcheck: "false",
            autocomplete: "off"
          ) %>
          <%= error_tag(f, :access_token) %>
        </div>

        <%= if length(@apps) > 0 do %>
          <div class="flex flex-col space-y-1">
            <h3 class="text-gray-800 font-semibold">
              Application
            </h3>
            <%= select(f, :application_id, @select_options, class: "input") %>
            <%= error_tag(f, :application_id) %>
          </div>

          <div class="grid grid-cols-1 md:grid-cols-2 gap-3">
            <div class="flex flex-col space-y-1">
              <h3 class="text-gray-800 font-semibold">
                Name
              </h3>
              <%= text_input(f, :hub_name, class: "input") %>
              <%= error_tag(f, :hub_name) %>
            </div>

            <div class="flex flex-col space-y-1">
              <h3 class="text-gray-800 font-semibold">
                Color
              </h3>

              <div class="flex space-x-4 items-center">
                <div
                  class="border-[3px] rounded-lg p-1 flex justify-center items-center"
                  style={"border-color: #{hub_color(@changeset)}"}
                >
                  <div class="rounded h-5 w-5" style={"background-color: #{hub_color(@changeset)}"} />
                </div>
                <div class="relative grow">
                  <%= text_input(f, :hub_color,
                    class: "input",
                    spellcheck: "false",
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
                  <%= error_tag(f, :hub_color) %>
                </div>
              </div>
            </div>
          </div>

          <%= submit("Create Hub",
            class: "button-base button-blue",
            phx_disable_with: "Creating...",
            disabled: not @valid?
          ) %>
        <% end %>
      </.form>
    </div>
    """
  end

  @impl true
  def handle_event("fetch_data", %{"fly" => %{"access_token" => token}}, socket) do
    case FlyClient.fetch_apps(token) do
      {:ok, apps} ->
        opts = select_options(apps)
        changeset = Fly.change_hub(%Fly{}, %{access_token: token, hub_color: HexColor.random()})

        {:noreply,
         assign(socket,
           changeset: changeset,
           valid?: changeset.valid?,
           select_options: opts,
           apps: apps
         )}

      {:error, _} ->
        changeset =
          %Fly{}
          |> Fly.change_hub(%{access_token: token})
          |> add_error(:access_token, "is invalid")

        {:noreply,
         assign(socket,
           changeset: changeset,
           valid?: changeset.valid?,
           select_options: [],
           apps: []
         )}
    end
  end

  def handle_event("randomize_color", _, socket) do
    handle_event("validate", %{"fly" => %{"hub_color" => HexColor.random()}}, socket)
  end

  def handle_event("save", %{"fly" => params}, socket) do
    if socket.assigns.valid? do
      case Fly.create_hub(socket.assigns.selected_app, params) do
        {:ok, hub} ->
          changeset = Fly.change_hub(hub, params)

          {:noreply,
           socket
           |> assign(changeset: changeset, selected_app: hub, valid?: changeset.valid?)
           |> put_flash(:success, "Hub created successfully")
           |> push_redirect(to: Routes.hub_path(socket, :edit, hub.id))}

        {:error, changeset} ->
          {:noreply,
           assign(socket, changeset: %{changeset | action: :validate}, valid?: changeset.valid?)}
      end
    else
      {:noreply, socket}
    end
  end

  def handle_event("validate", %{"fly" => attrs}, socket) do
    params = Map.merge(socket.assigns.changeset.params, attrs)

    application_id = params["application_id"]
    selected_app = Enum.find(socket.assigns.apps, &(&1.application_id == application_id))
    opts = select_options(socket.assigns.apps, application_id)

    changeset =
      if selected_app do
        Fly.change_hub(selected_app, params)
      else
        socket.assigns.changeset
        |> Fly.changeset(params)
        |> Map.replace!(:action, :validate)
      end

    {:noreply,
     assign(socket,
       changeset: changeset,
       valid?: changeset.valid?,
       selected_app: selected_app,
       select_options: opts
     )}
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

    [disabled_option] ++ options
  end

  defp hub_color(changeset), do: get_field(changeset, :hub_color)
  defp access_token(changeset), do: get_field(changeset, :access_token)
end
