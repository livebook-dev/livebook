defmodule LivebookWeb.Hub.Edit.PersonalComponent do
  use LivebookWeb, :live_component

  alias Livebook.Hubs
  alias Livebook.Hubs.Personal
  alias LivebookWeb.LayoutComponents
  alias LivebookWeb.NotFoundError

  @impl true
  def update(assigns, socket) do
    socket = assign(socket, assigns)
    changeset = Personal.change_hub(assigns.hub)
    secrets = Hubs.get_secrets(assigns.hub)
    file_systems = Hubs.get_file_systems(assigns.hub, hub_only: true)
    secret_name = assigns.params["secret_name"]
    file_system_id = assigns.params["file_system_id"]

    secret_value =
      if assigns.live_action == :edit_secret do
        Enum.find_value(secrets, &(&1.name == secret_name and &1.value)) ||
          raise(NotFoundError, "could not find secret matching #{inspect(secret_name)}")
      end

    file_system =
      if assigns.live_action == :edit_file_system do
        Enum.find_value(file_systems, &(&1.id == file_system_id && &1)) ||
          raise(NotFoundError, "could not find file system matching #{inspect(file_system_id)}")
      end

    {:ok,
     assign(socket,
       secrets: secrets,
       file_system: file_system,
       file_system_id: file_system_id,
       file_systems: file_systems,
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
        <div class="mb-8 flex flex-col space-y-2">
          <LayoutComponents.title text={"#{@hub.hub_emoji} #{@hub.hub_name}"} />

          <p class="text-gray-700 text-sm">
            Your personal workspace. All data is stored on your machine and only you can access it.
          </p>
        </div>

        <div class="mb-8 flex flex-col space-y-10">
          <div class="flex flex-col space-y-4">
            <h2 class="text-xl text-gray-800 font-medium pb-2 border-b border-gray-200">
              General
            </h2>

            <.form
              :let={f}
              id={@id}
              class="flex flex-col md:flex-row mt-4 space-y-4 md:space-x-2 md:space-y-0"
              for={@changeset}
              phx-submit="save"
              phx-change="validate"
              phx-target={@myself}
            >
              <div class="flex-auto">
                <.text_field field={f[:hub_name]} label="Name" />
              </div>
              <div class="min-w-48">
                <.emoji_field field={f[:hub_emoji]} label="Emoji" />
              </div>

              <div class="!mt-6">
                <.button type="submit" phx-disable-with="Updating..." disabled={not @changeset.valid?}>
                  Save
                </.button>
              </div>
            </.form>
          </div>

          <div class="flex flex-col space-y-4">
            <h2 class="text-xl text-gray-800 font-medium pb-2 border-b border-gray-200">
              Secrets
            </h2>

            <p class="text-gray-700">
              Secrets are a safe way to allow notebooks to access
              credentials and tokens.
            </p>

            <.live_component
              module={LivebookWeb.Hub.SecretListComponent}
              id="hub-secrets-list"
              hub={@hub}
              secrets={@secrets}
              edit_path={"hub/#{@hub.id}/secrets/edit"}
              return_to={~p"/hub/#{@hub.id}"}
            />

            <div>
              <.button patch={~p"/hub/#{@hub.id}/secrets/new"} id="add-secret">
                Add secret
              </.button>
            </div>
          </div>

          <div class="flex flex-col space-y-4">
            <h2 class="text-xl text-gray-800 font-medium pb-2 border-b border-gray-200">
              File Storages
            </h2>

            <p class="text-gray-700">
              File storages are used to store notebooks and their files.
            </p>

            <.live_component
              module={LivebookWeb.Hub.FileSystemListComponent}
              id="hub-file-systems-list"
              hub_id={@hub.id}
              file_systems={@file_systems}
              disabled={false}
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
              class="flex mt-4 space-x-2"
              for={@stamp_changeset}
              phx-submit="stamp_save"
              phx-change="stamp_validate"
              phx-target={@myself}
            >
              <div class="grow">
                <.password_field field={f[:secret_key]} label="Secret key" />
              </div>
              <div class="mt-6">
                <span class="tooltip top" data-tooltip="Generate">
                  <.button
                    color="gray"
                    small
                    type="button"
                    phx-click="generate_secret_key"
                    phx-target={@myself}
                  >
                    <.remix_icon icon="refresh-line" class="text-xl leading-none py-1" />
                  </.button>
                </span>
              </div>

              <div class="mt-6">
                <.button
                  type="submit"
                  phx-disable-with="Updating..."
                  disabled={not @stamp_changeset.valid?}
                >
                  Save
                </.button>
              </div>
            </.form>
          </div>
        </div>
      </div>

      <.modal
        :if={@live_action in [:new_secret, :edit_secret]}
        id="secrets-modal"
        show
        width="medium"
        patch={~p"/hub/#{@hub.id}"}
      >
        <.live_component
          module={LivebookWeb.Hub.SecretFormComponent}
          id="secrets"
          hub={@hub}
          secret_name={@secret_name}
          secret_value={@secret_value}
          disabled={false}
          return_to={~p"/hub/#{@hub.id}"}
        />
      </.modal>

      <.modal
        :if={@live_action in [:new_file_system, :edit_file_system]}
        id="file-systems-modal"
        show
        width="medium"
        patch={~p"/hub/#{@hub.id}"}
      >
        <.live_component
          module={LivebookWeb.Hub.FileSystemFormComponent}
          id="file-systems"
          hub={@hub}
          disabled={false}
          file_system={@file_system}
          file_system_id={@file_system_id}
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

  defp save(params, changeset_name, socket) do
    case Personal.update_hub(socket.assigns.hub, params) do
      {:ok, hub} ->
        socket
        |> put_flash(:success, "Workspace updated successfully")
        |> push_navigate(to: ~p"/hub/#{hub.id}")

      {:error, changeset} ->
        assign(socket, changeset_name, changeset)
    end
  end

  defp validate(params, changeset_name, socket) do
    assign(socket, changeset_name, Personal.validate_hub(socket.assigns.hub, params))
  end
end
