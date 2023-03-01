defmodule LivebookWeb.Hub.New.FlyComponent do
  use LivebookWeb, :live_component

  import Ecto.Changeset, only: [add_error: 3]

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
        :let={f}
        id={@id}
        class="flex flex-col space-y-4"
        for={@changeset}
        phx-submit="save"
        phx-change="validate"
        phx-target={@myself}
      >
        <.password_field
          type="password"
          field={f[:access_token]}
          label="Access Token"
          phx-change="fetch_data"
          phx-debounce="blur"
          phx-target={@myself}
          autofocus
          spellcheck="false"
          autocomplete="off"
        />

        <%= if length(@apps) > 0 do %>
          <.select_field
            field={f[:application_id]}
            label="Application"
            options={@select_options}
            prompt="Select one application"
          />

          <div class="grid grid-cols-1 md:grid-cols-2 gap-3">
            <.text_field field={f[:hub_name]} label="Name" />
            <.emoji_field field={f[:hub_emoji]} label="Emoji" />
          </div>

          <div>
            <button
              class="button-base button-blue"
              phx-disable-with="Add..."
              disabled={not @changeset.valid?}
            >
              Add Hub
            </button>
          </div>
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
        base = %Fly{access_token: token, hub_emoji: "🚀"}
        changeset = Fly.validate_hub(base)

        {:noreply,
         assign(socket, changeset: changeset, base: base, select_options: opts, apps: apps)}

      {:error, _} ->
        changeset =
          %Fly{}
          |> Fly.validate_hub(%{access_token: token})
          |> add_error(:access_token, "is invalid")

        {:noreply,
         assign(socket, changeset: changeset, base: %Fly{}, select_options: [], apps: [])}
    end
  end

  def handle_event("save", %{"fly" => params}, socket) do
    if socket.assigns.changeset.valid? do
      case Fly.create_hub(socket.assigns.selected_app, params) do
        {:ok, hub} ->
          {:noreply,
           socket
           |> put_flash(:success, "Hub added successfully")
           |> push_navigate(to: ~p"/hub/#{hub.id}")}

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
    opts = select_options(socket.assigns.apps)
    changeset = Fly.validate_hub(selected_app || socket.assigns.base, params)

    {:noreply,
     assign(socket, changeset: changeset, selected_app: selected_app, select_options: opts)}
  end

  defp select_options(hubs) do
    for fly <- hubs do
      [key: "#{fly.organization_name} - #{fly.application_id}", value: fly.application_id]
    end
  end
end
