defmodule LivebookWeb.Hub.Edit.PersonalComponent do
  use LivebookWeb, :live_component

  alias Livebook.Hubs
  alias Livebook.Hubs.Personal
  alias LivebookWeb.LayoutHelpers
  alias LivebookWeb.NotFoundError

  @impl true
  def update(assigns, socket) do
    socket = assign(socket, assigns)
    changeset = Personal.change_hub(assigns.hub)
    secrets = Hubs.get_secrets(assigns.hub)
    secret_name = assigns.params["secret_name"]

    secret_value =
      if assigns.live_action == :edit_secret do
        Enum.find_value(secrets, &(&1.name == secret_name and &1.value)) ||
          raise(NotFoundError, "could not find secret matching #{inspect(secret_name)}")
      end

    {:ok,
     assign(socket,
       secrets: secrets,
       changeset: changeset,
       stamp_changeset: changeset,
       secret_name: secret_name,
       secret_value: secret_value
     )}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="p-4 md:px-12 md:py-7 max-w-screen-md mx-auto">
      <div id={"#{@id}-component"}>
        <div class="mb-8 flex flex-col space-y-10">
          <div class="flex flex-col space-y-2">
            <LayoutHelpers.title text={"#{@hub.hub_emoji} #{@hub.hub_name}"} />

            <p class="text-gray-700 text-sm">
              Your personal hub. All data is stored on your machine and only you can access it.
            </p>
          </div>

          <div class="flex flex-col space-y-4">
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
                  disabled={not @changeset.valid?}
                >
                  Save
                </button>
              </div>
            </.form>
          </div>

          <div class="flex flex-col space-y-4">
            <h2 class="text-xl text-gray-800 font-medium pb-2 border-b border-gray-200">
              Secrets
            </h2>

            <p class="text-gray-700">
              Secrets are a safe way to share credentials and tokens with notebooks.
              They are often used by Smart cells and can be read as
              environment variables using the <code>LB_</code> prefix.
            </p>

            <.live_component
              module={LivebookWeb.Hub.SecretListComponent}
              id="hub-secrets-list"
              hub={@hub}
              secrets={@secrets}
              target={@myself}
            />
          </div>

          <div class="flex flex-col space-y-4">
            <h2 class="text-xl text-gray-800 font-medium pb-2 border-b border-gray-200">
              Stamping
            </h2>

            <p class="text-gray-700">
              Notebooks may be stamped using your <span class="font-medium text-gray-800">secret key</span>.
              A stamp allows to securely store information such as the names of the secrets that you granted access to.
              You must not share your secret key with others. But you may copy the secret key between
              different machines you own.
            </p>
            <p class="text-gray-700">
              If you change the <span class="font-medium text-gray-800">secret key</span>, you will need
              to grant access to secrets once again in previously stamped notebooks.
            </p>

            <.form
              :let={f}
              id={"#{@id}-stamp"}
              class="flex flex-col mt-4 space-y-4"
              for={@stamp_changeset}
              phx-submit="stamp_save"
              phx-change="stamp_validate"
              phx-target={@myself}
            >
              <div class="flex space-x-2">
                <div class="grow">
                  <.password_field field={f[:secret_key]} label="Secret key" />
                </div>
                <div class="mt-6">
                  <span class="tooltip top" data-tooltip="Generate">
                    <button
                      class="button-base button-outlined-gray button-square-icon"
                      type="button"
                      phx-click="generate_secret_key"
                      phx-target={@myself}
                    >
                      <.remix_icon icon="refresh-line" class="text-xl" />
                    </button>
                  </span>
                </div>
              </div>
              <div>
                <button
                  class="button-base button-blue"
                  type="submit"
                  phx-disable-with="Updating..."
                  disabled={not @stamp_changeset.valid?}
                >
                  Save
                </button>
              </div>
            </.form>
          </div>
        </div>
      </div>

      <.modal
        :if={@live_action in [:new_secret, :edit_secret]}
        id="secrets-modal"
        show
        width={:medium}
        patch={~p"/hub/#{@hub.id}"}
      >
        <.live_component
          module={LivebookWeb.Hub.SecretFormComponent}
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

  @impl true
  def handle_event("save", %{"personal" => params}, socket) do
    {:noreply, save(params, :changeset, socket)}
  end

  def handle_event("validate", %{"personal" => params}, socket) do
    {:noreply, validate(params, :changeset, socket)}
  end

  def handle_event("stamp_save", %{"personal" => params}, socket) do
    {:noreply, save(params, :stamp_changeset, socket)}
  end

  def handle_event("stamp_validate", %{"personal" => params}, socket) do
    {:noreply, validate(params, :stamp_changeset, socket)}
  end

  def handle_event("generate_secret_key", %{}, socket) do
    params = %{"secret_key" => Personal.generate_secret_key()}
    {:noreply, validate(params, :stamp_changeset, socket)}
  end

  def handle_event("delete_hub_secret", attrs, socket) do
    %{hub: hub} = socket.assigns

    on_confirm = fn socket ->
      {:ok, secret} = Livebook.Secrets.update_secret(%Livebook.Secrets.Secret{}, attrs)
      _ = Livebook.Hubs.delete_secret(hub, secret)

      socket
    end

    {:noreply,
     confirm(socket, on_confirm,
       title: "Delete hub secret - #{attrs["name"]}",
       description: "Are you sure you want to delete this hub secret?",
       confirm_text: "Delete",
       confirm_icon: "delete-bin-6-line"
     )}
  end

  defp save(params, changeset_name, socket) do
    case Personal.update_hub(socket.assigns.hub, params) do
      {:ok, hub} ->
        socket
        |> put_flash(:success, "Hub updated successfully")
        |> push_navigate(to: ~p"/hub/#{hub.id}")

      {:error, changeset} ->
        assign(socket, changeset_name, changeset)
    end
  end

  defp validate(params, changeset_name, socket) do
    assign(socket, changeset_name, Personal.validate_hub(socket.assigns.hub, params))
  end
end
