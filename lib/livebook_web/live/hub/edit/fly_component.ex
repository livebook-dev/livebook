defmodule LivebookWeb.Hub.Edit.FlyComponent do
  use LivebookWeb, :live_component

  import Ecto.Changeset, only: [get_field: 2]

  alias Livebook.EctoTypes.HexColor
  alias Livebook.Hubs.{Fly, FlyClient}

  @impl true
  def update(assigns, socket) do
    changeset = Fly.change_hub(assigns.hub)
    {:ok, app} = FlyClient.fetch_app(assigns.hub)

    {:ok,
     socket
     |> assign(assigns)
     |> assign(app_url: "https://#{app["hostname"]}", changeset: changeset)}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div>
      <!-- System details -->
      <div class="flex flex-col space-y-2 pb-5">
        <div class="flex items-center justify-between border border-gray-200 rounded-lg p-4">
          <div class="flex items-center space-x-12">
            <.labeled_text label="Application ID">
              <%= @hub.application_id %>
            </.labeled_text>
            <.labeled_text label="Type">
              Fly
            </.labeled_text>
          </div>

          <a href={@app_url} class="button-base button-outlined-gray" target="_blank">
            <.remix_icon icon="dashboard-2-line" class="align-middle mr-1" />
            <span>See app on Fly</span>
          </a>
        </div>
      </div>

      <div class="flex flex-col space-y-4">
        <h2 class="text-xl text-gray-800 font-semibold pb-2 border-b border-gray-200">
          General
        </h2>

        <.form
          id={@id}
          class="flex flex-col mt-4 space-y-4"
          let={f}
          for={@changeset}
          phx-submit="save"
          phx-change="validate"
          phx-target={@myself}
          phx-debounce="blur"
        >
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

          <%= submit("Update Hub",
            class: "button-base button-blue",
            phx_disable_with: "Updating...",
            disabled: not @changeset.valid?
          ) %>
        </.form>
      </div>
    </div>
    """
  end

  @impl true
  def handle_event("randomize_color", _, socket) do
    handle_event("validate", %{"fly" => %{"hub_color" => HexColor.random()}}, socket)
  end

  def handle_event("save", %{"fly" => params}, socket) do
    case Fly.update_hub(socket.assigns.hub, params) do
      {:ok, hub} ->
        {:noreply,
         socket
         |> put_flash(:success, "Hub updated successfully")
         |> push_redirect(to: Routes.hub_path(socket, :edit, hub.id))}

      {:error, changeset} ->
        {:noreply, assign(socket, changeset: changeset)}
    end
  end

  def handle_event("validate", %{"fly" => attrs}, socket) do
    changeset = Fly.change_hub(socket.assigns.hub, attrs)
    {:noreply, assign(socket, changeset: changeset)}
  end

  defp hub_color(changeset), do: get_field(changeset, :hub_color)
end
