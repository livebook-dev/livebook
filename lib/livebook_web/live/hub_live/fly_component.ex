defmodule LivebookWeb.HubLive.FlyComponent do
  use LivebookWeb, :live_component

  import Ecto.Changeset, only: [get_field: 2, add_error: 3]

  alias Livebook.EctoTypes.HexColor
  alias Livebook.Hubs.{Fly, FlyClient}

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
            disabled: @operation == :edit,
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
            <%= select(f, :application_id, @select_options,
              class: "input",
              disabled: @operation == :edit
            ) %>
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

          <%= submit("Save",
            class: "button-base button-blue",
            phx_disable_with: "Saving...",
            disabled: not @valid?
          ) %>
        <% end %>
      </.form>
    </div>
    """
  end

  defp load_data(%{assigns: %{operation: :new}} = socket) do
    assign(socket,
      changeset: Fly.change_hub(%Fly{}),
      selected_app: nil,
      select_options: [],
      apps: [],
      valid?: false
    )
  end

  defp load_data(%{assigns: %{operation: :edit, hub: fly}} = socket) do
    {:ok, apps} = FlyClient.fetch_apps(fly.access_token)
    params = Map.from_struct(fly)

    assign(socket,
      changeset: Fly.change_hub(fly, params),
      selected_app: fly,
      select_options: select_options(apps),
      apps: apps,
      valid?: true
    )
  end

  @impl true
  def handle_event("fetch_data", %{"fly" => %{"access_token" => token}}, socket) do
    case FlyClient.fetch_apps(token) do
      {:ok, apps} ->
        opts = select_options(apps)

        changeset =
          socket.assigns.changeset
          |> Fly.changeset(%{access_token: token, hub_color: HexColor.random()})
          |> clean_errors()

        {:noreply,
         assign(socket,
           changeset: changeset,
           valid?: changeset.valid?,
           select_options: opts,
           apps: apps
         )}

      {:error, _} ->
        changeset =
          socket.assigns.changeset
          |> Fly.changeset()
          |> clean_errors()
          |> put_action()
          |> add_error(:access_token, "is invalid")

        send(self(), {:flash, :error, "Failed to fetch Applications"})

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
    changeset =
      socket.assigns.changeset
      |> clean_errors()
      |> Fly.change_hub(%{hub_color: HexColor.random()})
      |> put_action()

    {:noreply, assign(socket, changeset: changeset, valid?: changeset.valid?)}
  end

  def handle_event("save", %{"fly" => params}, socket) do
    {:noreply, save_fly(socket, socket.assigns.operation, params)}
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
        Fly.changeset(socket.assigns.changeset, params)
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

  defp save_fly(socket, :new, params) do
    case Fly.create_hub(socket.assigns.selected_app, params) do
      {:ok, fly} ->
        changeset =
          fly
          |> Fly.change_hub(params)
          |> put_action()

        send(self(), {:flash, :info, "Hub created successfully"})
        assign(socket, changeset: changeset, selected_app: fly, valid?: false)

      {:error, changeset} ->
        send(self(), {:flash, :error, "Failed to create Hub"})
        assign(socket, changeset: put_action(changeset), valid?: changeset.valid?)
    end
  end

  defp save_fly(socket, :edit, params) do
    case Fly.update_hub(socket.assigns.selected_app, params) do
      {:ok, fly} ->
        changeset =
          fly
          |> Fly.change_hub(params)
          |> put_action()

        send(self(), {:flash, :info, "Hub updated successfully"})
        assign(socket, changeset: changeset, selected_app: fly, valid?: changeset.valid?)

      {:error, changeset} ->
        send(self(), {:flash, :error, "Failed to update Hub"})
        assign(socket, changeset: put_action(changeset), valid?: changeset.valid?)
    end
  end

  defp clean_errors(changeset), do: %{changeset | errors: []}
  defp put_action(changeset, action \\ :validate), do: %{changeset | action: action}

  defp hub_color(changeset), do: get_field(changeset, :hub_color)
  defp access_token(changeset), do: get_field(changeset, :access_token)
end
