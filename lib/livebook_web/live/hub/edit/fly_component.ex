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
     |> assign(
       app_url: "https://#{app["hostname"]}",
       changeset: changeset,
       secrets: app["secrets"],
       secret_data: %{},
       operation: :new,
       valid_secret?: false
     )}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div id={@id <> "-component"}>
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

      <div class="flex flex-col mt-4 space-y-4">
        <div class="grid grid-cols-1 md:grid-cols-2 w-full border-b pb-2 border-gray-200">
          <div class="place-content-start">
            <h2 class="text-xl text-gray-800 font-semibold">
              Secrets
            </h2>
          </div>

          <div class="flex place-content-end">
            <button
              class="button-base border-1 border-gray-200 hover:bg-gray-200"
              type="button"
              phx-click={show_modal("new-secret-modal")}
            >
              Add secret
            </button>
          </div>
        </div>

        <div class="mt-4">
          <div class="border border-gray-200 rounded-lg p-4 mt-0">
            <%= for {secret, index} <- Enum.with_index(@secrets) do %>
              <.secret_card
                myself={@myself}
                secret={secret}
                index={index}
                last_index={length(@secrets) - 1}
              />
            <% end %>
          </div>
        </div>
      </div>

      <.secret_modal
        on_save={hide_modal("secret-modal")}
        data={@secret_data}
        valid?={@valid_secret?}
        myself={@myself}
      />
    </div>
    """
  end

  defp secret_card(assigns) do
    card_class = "flex items-center justify-between"

    assigns =
      if assigns.index < assigns.last_index do
        assign_new(assigns, :class, fn ->
          card_class <> " mb-3 pb-3 border-b border-gray-200"
        end)
      else
        assign_new(assigns, :class, fn -> card_class end)
      end

    ~H"""
    <div id={"secret-" <> @secret["id"]} class={@class}>
      <div class="grid grid-cols-1 md:grid-cols-3 w-full">
        <div class="place-content-start">
          <.labeled_text label="Name">
            <%= @secret["name"] %>
          </.labeled_text>
        </div>

        <div class="flex place-content-end">
          <.labeled_text label="Created at">
            <%= @secret["createdAt"] %>
          </.labeled_text>
        </div>

        <div class="flex place-content-end">
          <button
            id={"secret-" <> @secret["id"] <> "-edit"}
            phx-click={show_modal("secret-modal") |> JS.push("edit", value: %{secret: @secret})}
            phx-target={@myself}
            class="icon-button text-blue-500 hover:text-blue-800"
          >
            <.remix_icon icon="file-edit-line" class="align-middle mr-1" />
            <span>Edit</span>
          </button>
          <button
            id={"secret-" <> @secret["id"] <> "-delete"}
            phx-click={
              with_confirm(
                JS.push("delete", value: %{secret: @secret}),
                title: "Delete #{@secret["name"]}",
                description: "Are you sure you want to delete secret?",
                confirm_text: "Delete",
                confirm_icon: "delete-bin-6-line"
              )
            }
            phx-target={@myself}
            class="icon-button text-red-500 hover:text-red-800"
          >
            <.remix_icon icon="delete-bin-line" class="align-middle mr-1" />
            <span>Delete</span>
          </button>
        </div>
      </div>
    </div>
    """
  end

  defp secret_modal(assigns) do
    ~H"""
    <.modal id="secret-modal" class="w-full max-w-sm">
      <div class="p-6 pb-4 max-w-4xl flex flex-col space-y-5">
        <h3 class="text-2xl font-semibold text-gray-800">
          Add secret
        </h3>
        <div class="flex-col space-y-5">
          <p class="text-gray-700">
            Enter the secret name and its value.
          </p>
          <.form
            id="secret-form"
            let={f}
            for={:secret}
            phx-submit={@on_save |> JS.push("save")}
            phx-change="validate"
            autocomplete="off"
            phx-target={@myself}
          >
            <div class="flex flex-col space-y-4">
              <div>
                <div class="input-label">
                  Key <span class="text-xs text-gray-500">(alphanumeric and underscore)</span>
                </div>
                <%= text_input(f, :key,
                  value: @data["key"],
                  class: "input",
                  placeholder: "secret key",
                  autofocus: true,
                  aria_labelledby: "secret-key",
                  spellcheck: "false"
                ) %>
              </div>
              <div>
                <div class="input-label">Value</div>
                <%= text_input(f, :value,
                  value: @data["value"],
                  class: "input",
                  placeholder: "secret value",
                  aria_labelledby: "secret-value",
                  spellcheck: "false"
                ) %>
              </div>
            </div>
            <%= submit("Add Secret",
              class: "mt-5 button-base button-blue",
              phx_disable_with: "Adding...",
              disabled: not @valid?
            ) %>
          </.form>
        </div>
      </div>
    </.modal>
    """
  end

  @impl true
  def handle_event("randomize_color", _, socket) do
    handle_event("validate", %{"fly" => %{"hub_color" => HexColor.random()}}, socket)
  end

  def handle_event("edit", %{"secret" => %{"name" => name}}, socket) do
    {:noreply, assign(socket, operation: :edit, secret_data: %{"key" => name})}
  end

  def handle_event("delete", %{"secret" => %{"name" => key}}, socket) do
    case FlyClient.delete_secrets(socket.assigns.hub, [key]) do
      {:ok, _} ->
        {:noreply,
         socket
         |> put_flash(:success, "Secret deleted successfully")
         |> push_redirect(to: Routes.hub_path(socket, :edit, socket.assigns.hub.id))}

      {:error, _} ->
        {:noreply,
         socket
         |> put_flash(:error, "Failed to delete secret")
         |> push_redirect(to: Routes.hub_path(socket, :edit, socket.assigns.hub.id))}
    end
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

  def handle_event("save", %{"secret" => params}, socket) do
    if socket.assigns.valid_secret? do
      case FlyClient.put_secrets(socket.assigns.hub, [params]) do
        {:ok, _} ->
          message =
            if socket.assigns.operation == :new do
              "Secret added successfully"
            else
              "Secret updated successfully"
            end

          {:noreply,
           socket
           |> put_flash(:success, message)
           |> push_redirect(to: Routes.hub_path(socket, :edit, socket.assigns.hub.id))}

        {:error, _} ->
          message =
            if socket.assigns.operation == :new do
              "Failed to add secret"
            else
              "Failed to update secret"
            end

          {:noreply,
           socket
           |> put_flash(:error, message)
           |> push_redirect(to: Routes.hub_path(socket, :edit, socket.assigns.hub.id))}
      end
    else
      {:noreply, socket}
    end
  end

  def handle_event("validate", %{"fly" => attrs}, socket) do
    changeset = Fly.change_hub(socket.assigns.hub, attrs)
    {:noreply, assign(socket, changeset: changeset)}
  end

  def handle_event("validate", %{"secret" => attrs}, socket) do
    valid? = String.match?(attrs["key"], ~r/^\w+$/) and attrs["value"] not in ["", nil]
    {:noreply, assign(socket, valid_secret?: valid?, secret_data: attrs)}
  end

  defp hub_color(changeset), do: get_field(changeset, :hub_color)
end
