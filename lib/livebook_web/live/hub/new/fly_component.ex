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
       base: %Fly{},
       changeset: Fly.change_hub(%Fly{}),
       selected_app: nil,
       select_options: [],
       apps: []
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
        <.input_wrapper form={f} field={:access_token} class="flex flex-col space-y-1">
          <div class="input-label">Access Token</div>
          <%= password_input(f, :access_token,
            phx_change: "fetch_data",
            phx_debounce: "blur",
            phx_target: @myself,
            value: access_token(@changeset),
            class: "input w-full phx-form-error:border-red-300",
            autofocus: true,
            spellcheck: "false",
            autocomplete: "off"
          ) %>
        </.input_wrapper>

        <%= if length(@apps) > 0 do %>
          <.input_wrapper form={f} field={:application_id} class="flex flex-col space-y-1">
            <div class="input-label">Application</div>
            <%= select(f, :application_id, @select_options, class: "input") %>
          </.input_wrapper>

          <div class="grid grid-cols-1 md:grid-cols-2 gap-3">
            <.input_wrapper form={f} field={:hub_name} class="flex flex-col space-y-1">
              <div class="input-label">Name</div>
              <%= text_input(f, :hub_name, class: "input") %>
            </.input_wrapper>

            <.input_wrapper form={f} field={:hub_color} class="flex flex-col space-y-1">
              <div class="input-label">Color</div>
              <.hex_color_input
                form={f}
                field={:hub_color}
                randomize={JS.push("randomize_color", target: @myself)}
              />
            </.input_wrapper>
          </div>

          <%= submit("Add Hub",
            class: "button-base button-blue",
            phx_disable_with: "Add...",
            disabled: not @changeset.valid?
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
        base = %Fly{access_token: token, hub_color: HexColor.random()}
        changeset = Fly.change_hub(base)

        {:noreply,
         assign(socket, changeset: changeset, base: base, select_options: opts, apps: apps)}

      {:error, _} ->
        changeset =
          %Fly{}
          |> Fly.change_hub(%{access_token: token})
          |> add_error(:access_token, "is invalid")

        {:noreply,
         assign(socket, changeset: changeset, base: %Fly{}, select_options: [], apps: [])}
    end
  end

  def handle_event("randomize_color", _, socket) do
    handle_event("validate", %{"fly" => %{"hub_color" => HexColor.random()}}, socket)
  end

  def handle_event("save", %{"fly" => params}, socket) do
    if socket.assigns.changeset.valid? do
      case Fly.create_hub(socket.assigns.selected_app, params) do
        {:ok, hub} ->
          {:noreply,
           socket
           |> put_flash(:success, "Hub added successfully")
           |> push_redirect(to: Routes.hub_path(socket, :edit, hub.id))}

        {:error, changeset} ->
          {:noreply, assign(socket, changeset: changeset)}
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
    changeset = Fly.change_hub(selected_app || socket.assigns.base, params)

    {:noreply,
     assign(socket, changeset: changeset, selected_app: selected_app, select_options: opts)}
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

  defp access_token(changeset), do: get_field(changeset, :access_token)
end
