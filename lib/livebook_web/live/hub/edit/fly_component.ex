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
       env_vars: app["secrets"],
       env_var_data: %{},
       operation: :new,
       valid_env_var?: false
     )}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div id={@id <> "-component"} class="flex flex-col space-y-10">
      <div class="flex flex-col space-y-2">
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

      <div class="flex flex-col space-y-2">
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

      <div class="flex flex-col space-y-4">
        <h2 class="text-xl text-gray-800 font-semibold pb-2 border-b border-gray-200">
          Environment Variables
        </h2>

        <div class="flex flex-col space-y-4">
          <%= for {env_var, index} <- Enum.with_index(@env_vars) do %>
            <.environment_variable_card
              myself={@myself}
              env_var={env_var}
              index={index}
              last_index={length(@env_vars) - 1}
            />
          <% end %>
        </div>

        <button
          class="button-base button-blue"
          type="button"
          phx-click={show_modal("environment-variable-modal")}
        >
          Add environment variable
        </button>
      </div>

      <.environment_variable_modal
        id="environment-variable-modal"
        on_save={hide_modal("environment-variable-modal")}
        data={@env_var_data}
        valid?={@valid_env_var?}
        myself={@myself}
      />
    </div>
    """
  end

  defp environment_variable_card(assigns) do
    ~H"""
    <div
      id={"env-var-" <> @env_var["id"]}
      class="flex items-center justify-between border border-gray-200 rounded-lg p-4"
    >
      <div class="grid grid-cols-1 md:grid-cols-3 w-full">
        <div class="place-content-start">
          <.labeled_text label="Name">
            <%= @env_var["name"] %>
          </.labeled_text>
        </div>

        <div class="flex place-content-end">
          <.labeled_text label="Created at">
            <%= @env_var["createdAt"] %>
          </.labeled_text>
        </div>

        <div class="flex place-content-end">
          <.menu id={"env-var-#{@env_var["id"]}-menu"}>
            <:toggle>
              <button class="icon-button" aria-label="open session menu" type="button">
                <.remix_icon icon="more-2-fill" class="text-xl" />
              </button>
            </:toggle>
            <:content>
              <button
                id={"env-var-" <> @env_var["id"] <> "-edit"}
                type="button"
                phx-click={
                  show_modal("environment-variable-modal")
                  |> JS.push("edit", value: %{env_var: @env_var})
                }
                phx-target={@myself}
                role="menuitem"
                class="menu-item text-gray-600"
              >
                <.remix_icon icon="file-edit-line" />
                <span class="font-medium">Edit</span>
              </button>
              <button
                id={"env-var-" <> @env_var["id"] <> "-delete"}
                type="button"
                phx-click={
                  with_confirm(
                    JS.push("delete", value: %{env_var: @env_var}),
                    title: "Delete #{@env_var["name"]}",
                    description: "Are you sure you want to delete environment variable?",
                    confirm_text: "Delete",
                    confirm_icon: "delete-bin-6-line"
                  )
                }
                phx-target={@myself}
                role="menuitem"
                class="menu-item text-red-600"
              >
                <.remix_icon icon="delete-bin-line" />
                <span class="font-medium">Delete</span>
              </button>
            </:content>
          </.menu>
        </div>
      </div>
    </div>
    """
  end

  defp environment_variable_modal(assigns) do
    ~H"""
    <.modal id={@id} class="w-full max-w-lg">
      <div class="p-6 pb-4 max-w-4xl flex flex-col space-y-5">
        <h3 class="text-2xl font-semibold text-gray-800">
          Add environment variable
        </h3>
        <div class="flex-col space-y-5">
          <p class="text-gray-700">
            Enter the environment variable name and its value.
          </p>
          <.form
            id="env-var-form"
            let={f}
            for={:env_var}
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
                  placeholder: "environment variable key",
                  autofocus: true,
                  aria_labelledby: "env-var-key",
                  spellcheck: "false"
                ) %>
              </div>
              <div>
                <div class="input-label">Value</div>
                <%= text_input(f, :value,
                  value: @data["value"],
                  class: "input",
                  placeholder: "environment variable value",
                  aria_labelledby: "env-var-value",
                  spellcheck: "false"
                ) %>
              </div>
            </div>
            <%= submit("Add environment variable",
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

  def handle_event("edit", %{"env_var" => %{"name" => name}}, socket) do
    {:noreply, assign(socket, operation: :edit, env_var_data: %{"key" => name})}
  end

  def handle_event("delete", %{"env_var" => %{"name" => key}}, socket) do
    case FlyClient.delete_secrets(socket.assigns.hub, [key]) do
      {:ok, _} ->
        {:noreply,
         socket
         |> put_flash(:success, "Environment variable deleted")
         |> push_redirect(to: Routes.hub_path(socket, :edit, socket.assigns.hub.id))}

      {:error, _} ->
        {:noreply,
         socket
         |> put_flash(:error, "Failed to delete environment variable")
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

  def handle_event("save", %{"env_var" => params}, socket) do
    if socket.assigns.valid_env_var? do
      case FlyClient.put_secrets(socket.assigns.hub, [params]) do
        {:ok, _} ->
          message =
            if socket.assigns.operation == :new do
              "Environment variable added"
            else
              "Environment variable updated"
            end

          {:noreply,
           socket
           |> put_flash(:success, message)
           |> push_redirect(to: Routes.hub_path(socket, :edit, socket.assigns.hub.id))}

        {:error, _} ->
          message =
            if socket.assigns.operation == :new do
              "Failed to add environment variable"
            else
              "Failed to update environment variable"
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

  def handle_event("validate", %{"env_var" => attrs}, socket) do
    valid? = String.match?(attrs["key"], ~r/^\w+$/) and attrs["value"] not in ["", nil]
    {:noreply, assign(socket, valid_env_var?: valid?, env_var_data: attrs)}
  end

  defp hub_color(changeset), do: get_field(changeset, :hub_color)
end
