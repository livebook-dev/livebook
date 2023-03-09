defmodule LivebookWeb.Hub.Edit.PersonalComponent do
  use LivebookWeb, :live_component

  alias Livebook.Hubs.Personal
  alias LivebookWeb.LayoutHelpers

  @impl true
  def update(assigns, socket) do
    changeset = Personal.change_hub(assigns.hub)

    secret_value =
      if assigns.live_action == :edit_secret do
        secret = Enum.find(assigns.secrets, &(&1.name == assigns.secret_name))
        secret.value
      end

    {:ok,
     socket
     |> assign(assigns)
     |> assign(changeset: changeset, secret_value: secret_value)}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div id={"#{@id}-component"} class="space-y-8">
      <div class="space-y-4">
        <LayoutHelpers.title text={"#{@hub.hub_emoji} #{@hub.hub_name}"} />

        <p class="text-gray-700">
          Your personal hub. Only you can see and access the data in it.
        </p>
      </div>

      <div class="flex flex-col space-y-10">
        <div class="flex flex-col space-y-2">
          <h2 class="text-xl text-gray-800 font-medium pb-2 border-b border-gray-200">
            General
          </h2>

          <.form
            :let={f}
            id={@id}
            class="flex flex-col mt-4 space-y-4"
            for={@changeset}
            phx-submit="save"
            phx-change="validate"
            phx-target={@myself}
          >
            <div class="grid grid-cols-1 md:grid-cols-2 gap-3">
              <.text_field field={f[:hub_name]} label="Name" />
              <.emoji_field field={f[:hub_emoji]} label="Emoji" />
            </div>
            <div>
              <button
                class="button-base button-blue"
                type="submit"
                phx-disable-with="Updating..."
                disable={not @changeset.valid?}
              >
                Update Hub
              </button>
            </div>
          </.form>
        </div>

        <div class="flex flex-col space-y-4">
          <h2 class="text-xl text-gray-800 font-medium pb-2 border-b border-gray-200">
            Secrets
          </h2>

          <.secrets_list
            id="hub-secrets-list"
            new_secret_path={~p"/hub/#{@hub.id}/secrets/new"}
            secrets={@secrets}
            target={@myself}
          />
        </div>
      </div>

      <.modal
        :if={@live_action in [:new_secret, :edit_secret]}
        id="secrets-modal"
        show
        width={:big}
        patch={~p"/hub/#{@hub.id}"}
      >
        <.live_component
          module={LivebookWeb.Hub.SecretsComponent}
          id="secrets"
          hub={@hub}
          secret_name={@secret_name}
          secret_value={@secret_value}
          return_to={~p"/hub/#{@hub.id}"}
        />
      </.modal>
    </div>
    """
  end

  defp secrets_list(assigns) do
    ~H"""
    <div id={@id} class="flex flex-col space-y-4">
      <div class="flex flex-col space-y-4">
        <div
          :for={secret <- @secrets}
          class="flex items-center justify-between border border-gray-200 rounded-lg p-4"
        >
          <.secret_info secret={secret} target={@target} />
        </div>
      </div>
      <div class="flex">
        <.link patch={@new_secret_path} class="button-base button-blue" id="add-secret">
          Add new secret
        </.link>
      </div>
    </div>
    """
  end

  defp secret_info(assigns) do
    ~H"""
    <div class="grid grid-cols-1 md:grid-cols-2 w-full">
      <div class="place-content-start">
        <.labeled_text label="Name">
          <%= @secret.name %>
        </.labeled_text>
      </div>

      <div class="flex items-center place-content-end">
        <.menu id={"hub-secret-#{@secret.name}-menu"}>
          <:toggle>
            <button class="icon-button" aria-label="open environment variable menu" type="button">
              <.remix_icon icon="more-2-fill" class="text-xl" />
            </button>
          </:toggle>
          <.menu_item>
            <.link
              id={"hub-secret-#{@secret.name}-edit"}
              patch={~p"/hub/#{@secret.hub_id}/secrets/edit/#{@secret.name}"}
              type="button"
              role="menuitem"
            >
              <.remix_icon icon="file-edit-line" />
              <span>Edit</span>
            </.link>
          </.menu_item>
          <.menu_item variant={:danger}>
            <button
              id={"hub-secret-#{@secret.name}-delete"}
              type="button"
              phx-click={
                with_confirm(
                  JS.push("delete_hub_secret",
                    value: %{
                      name: @secret.name,
                      value: @secret.value,
                      hub_id: @secret.hub_id
                    },
                    target: @target
                  ),
                  title: "Delete hub secret - #{@secret.name}",
                  description: "Are you sure you want to delete this hub secret?",
                  confirm_text: "Delete",
                  confirm_icon: "delete-bin-6-line"
                )
              }
              phx-target={@target}
              role="menuitem"
            >
              <.remix_icon icon="delete-bin-line" />
              <span>Delete</span>
            </button>
          </.menu_item>
        </.menu>
      </div>
    </div>
    """
  end

  @impl true
  def handle_event("save", %{"personal" => params}, socket) do
    case Personal.update_hub(socket.assigns.hub, params) do
      {:ok, hub} ->
        {:noreply,
         socket
         |> put_flash(:success, "Hub updated successfully")
         |> push_navigate(to: ~p"/hub/#{hub.id}")}

      {:error, changeset} ->
        {:noreply, assign(socket, changeset: changeset)}
    end
  end

  def handle_event("validate", %{"personal" => attrs}, socket) do
    {:noreply, assign(socket, changeset: Personal.validate_hub(socket.assigns.hub, attrs))}
  end

  def handle_event("delete_hub_secret", attrs, socket) do
    {:ok, secret} = Livebook.Secrets.update_secret(%Livebook.Secrets.Secret{}, attrs)
    :ok = Livebook.Hubs.delete_secret(socket.assigns.hub, secret)

    {:noreply, socket}
  end
end
