defmodule LivebookWeb.Hub.Edit.TeamComponent do
  use LivebookWeb, :live_component

  alias Livebook.Hubs
  alias Livebook.Hubs.{Provider, Team}
  alias Livebook.Teams
  alias LivebookWeb.LayoutHelpers
  alias LivebookWeb.NotFoundError

  @impl true
  def update(assigns, socket) do
    socket = assign(socket, assigns)
    changeset = Team.change_hub(assigns.hub)
    show_key? = assigns.params["show-key"] == "true"
    secrets = Hubs.get_secrets(assigns.hub)
    file_systems = Hubs.get_file_systems(assigns.hub, hub_only: true)
    secret_name = assigns.params["secret_name"]
    file_system_id = assigns.params["file_system_id"]
    default? = default_hub?(assigns.hub)

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
     socket
     |> assign(
       secrets: secrets,
       file_system: file_system,
       file_system_id: file_system_id,
       file_systems: file_systems,
       show_key: show_key?,
       secret_name: secret_name,
       secret_value: secret_value,
       hub_metadata: Provider.to_metadata(assigns.hub),
       default?: default?
     )
     |> assign_new(:config_changeset, fn -> Hubs.Dockerfile.config_changeset() end)
     |> update_dockerfile()
     |> assign_form(changeset)}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div>
      <LayoutHelpers.topbar
        :if={not @hub_metadata.connected? && Provider.connection_error(@hub)}
        variant={:warning}
      >
        <%= Provider.connection_error(@hub) %>
      </LayoutHelpers.topbar>

      <div class="p-4 md:px-12 md:py-7 max-w-screen-md mx-auto">
        <div id={"#{@id}-component"}>
          <div class="mb-8 flex flex-col space-y-10">
            <div class="flex flex-col space-y-2">
              <LayoutHelpers.title>
                <div class="flex gap-2 items-center">
                  <div class="flex justify-center">
                    <span class="relative">
                      <%= @hub.hub_emoji %>

                      <div class={[
                        "absolute w-[10px] h-[10px] border-white border-2 rounded-full right-0 bottom-1",
                        if(@hub_metadata.connected?, do: "bg-green-400", else: "bg-red-400")
                      ]} />
                    </span>
                  </div>
                  <%= @hub.hub_name %>
                  <span class="bg-green-100 text-green-800 text-xs px-2.5 py-0.5 rounded cursor-default">
                    Livebook Teams
                  </span>
                  <%= if @default? do %>
                    <span class="bg-blue-100 text-blue-800 text-xs px-2.5 py-0.5 rounded cursor-default">
                      Default
                    </span>
                  <% end %>
                </div>
              </LayoutHelpers.title>

              <p class="text-sm flex flex-row space-x-6 text-gray-700">
                <a href={org_url(@hub, "/")} class="hover:text-blue-600">
                  <.remix_icon icon="mail-line" /> Invite users
                </a>

                <a href={org_url(@hub, "/")} class="hover:text-blue-600">
                  <.remix_icon icon="settings-line" /> Manage organization
                </a>

                <button
                  phx-click={show_modal("show-key-modal")}
                  phx-target={@myself}
                  class="hover:text-blue-600 cursor-pointer"
                >
                  <.remix_icon icon="key-2-fill" /> Display Teams key
                </button>
                <%= if @default? do %>
                  <button
                    phx-click="remove_as_default"
                    phx-target={@myself}
                    class="hover:text-blue-600 cursor-pointer"
                  >
                    <.remix_icon icon="star-fill" /> Remove as default
                  </button>
                <% else %>
                  <button
                    phx-click="mark_as_default"
                    phx-target={@myself}
                    class="hover:text-blue-600 cursor-pointer"
                  >
                    <.remix_icon icon="star-line" /> Mark as default
                  </button>
                <% end %>
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
                for={@form}
                phx-submit="save"
                phx-change="validate"
                phx-target={@myself}
              >
                <div class="grid grid-cols-1 md:grid-cols-2 gap-3">
                  <.text_field
                    field={f[:hub_name]}
                    label="Name"
                    disabled
                    help="Name cannot be changed"
                    class="bg-gray-200/50 border-200/80 cursor-not-allowed"
                  />
                  <.emoji_field field={f[:hub_emoji]} label="Emoji" />
                </div>

                <div>
                  <button class="button-base button-blue" type="submit" phx-disable-with="Updating...">
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
                File storages
              </h2>

              <p class="text-gray-700">
                File storages are used to store notebooks and their files.
              </p>

              <.live_component
                module={LivebookWeb.Hub.FileSystemListComponent}
                id="hub-file-systems-list"
                hub_id={@hub.id}
                file_systems={@file_systems}
                target={@myself}
              />
            </div>

            <div class="flex flex-col space-y-4">
              <h2 class="text-xl text-gray-800 font-medium pb-2 border-b border-gray-200">
                Airgapped deployment
              </h2>

              <p class="text-gray-700">
                It is possible to deploy notebooks that belong to this Hub in an airgapped
                deployment, without connecting back to Livebook Teams server. Configure the
                deployment below and use the generated Dockerfile in a directory with notebooks
                that belong to your Organization.
              </p>

              <.form
                :let={f}
                for={@config_changeset}
                as={:data}
                phx-change="validate_dockerfile"
                phx-target={@myself}
              >
                <LivebookWeb.AppHelpers.docker_config_form_content
                  hub={@hub}
                  form={f}
                  show_deploy_all={false}
                />
              </.form>

              <LivebookWeb.AppHelpers.docker_instructions
                hub={@hub}
                dockerfile={@dockerfile}
                dockerfile_config={Ecto.Changeset.apply_changes(@config_changeset)}
              />
            </div>

            <div class="flex flex-col space-y-4">
              <h2 class="text-xl text-gray-800 font-medium pb-2 border-b border-gray-200">
                Danger zone
              </h2>

              <div class="flex items-center justify-between gap-4 text-gray-700">
                <div class="flex flex-col">
                  <h3 class="font-semibold">
                    Delete this hub
                  </h3>
                  <p class="text-sm">
                    This only removes the hub from this machine. You must rejoin to access its features once again.
                  </p>
                </div>
                <button
                  id="delete-hub"
                  phx-click={JS.push("delete_hub", value: %{id: @hub.id})}
                  class="button-base button-outlined-red"
                >
                  <span class="hidden sm:block">Delete hub</span>
                  <.remix_icon icon="delete-bin-line" class="text-lg sm:hidden" />
                </button>
              </div>
            </div>
          </div>

          <.modal show={@show_key} id="show-key-modal" width={:medium} patch={~p"/hub/#{@hub.id}"}>
            <.teams_key_modal teams_key={@hub.teams_key} />
          </.modal>

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

          <.modal
            :if={@live_action in [:new_file_system, :edit_file_system]}
            id="file-systems-modal"
            show
            width={:medium}
            patch={~p"/hub/#{@hub.id}"}
          >
            <.live_component
              module={LivebookWeb.Hub.FileSystemFormComponent}
              id="file-systems"
              hub={@hub}
              file_system={@file_system}
              file_system_id={@file_system_id}
              return_to={~p"/hub/#{@hub.id}"}
            />
          </.modal>
        </div>
      </div>
    </div>
    """
  end

  defp org_url(hub, path) do
    Livebook.Config.teams_url() <> "/orgs/#{hub.org_id}" <> path
  end

  defp teams_key_modal(assigns) do
    ~H"""
    <div class="p-6 flex flex-col space-y-5">
      <h3 class="text-2xl font-semibold text-gray-800">
        Teams key
      </h3>
      <div class="justify-center">
        This is your <strong>Teams key</strong>. This key encrypts your
        data before it is sent to Livebook Teams servers. This key is
        required for you and invited users to join this organization.
        We recommend storing it somewhere safe:
      </div>
      <div class=" w-full">
        <div id="teams-key-toggle" class="relative flex">
          <input
            type="password"
            id="teams-key"
            readonly
            value={@teams_key}
            class="input font-mono w-full border-neutral-200 bg-neutral-100 py-2 border-2 pr-8"
          />

          <div class="flex items-center absolute inset-y-0 right-1">
            <button
              class="icon-button"
              data-tooltip="Copied to clipboard"
              type="button"
              aria-label="copy to clipboard"
              phx-click={
                JS.dispatch("lb:clipcopy", to: "#teams-key")
                |> JS.add_class("", transition: {"tooltip top", "", ""}, time: 2000)
              }
            >
              <.remix_icon icon="clipboard-line" class="text-xl" />
            </button>

            <button
              class="icon-button"
              data-show
              type="button"
              aria-label="show password"
              phx-click={
                JS.remove_attribute("type", to: "#teams-key-toggle input")
                |> JS.set_attribute({"type", "text"}, to: "#teams-key-toggle input")
                |> toggle_class("hidden", to: "#teams-key-toggle [data-show]")
                |> toggle_class("hidden", to: "#teams-key-toggle [data-hide]")
              }
            >
              <.remix_icon icon="eye-line" class="text-xl" />
            </button>
            <button
              class="icon-button hidden"
              data-hide
              type="button"
              aria-label="hide password"
              phx-click={
                JS.remove_attribute("type", to: "#teams-key-toggle input")
                |> JS.set_attribute({"type", "password"}, to: "#teams-key-toggle input")
                |> toggle_class("hidden", to: "#teams-key-toggle [data-show]")
                |> toggle_class("hidden", to: "#teams-key-toggle [data-hide]")
              }
            >
              <.remix_icon icon="eye-off-line" class="text-xl" />
            </button>
          </div>
        </div>
      </div>
    </div>
    """
  end

  @impl true
  def handle_event("save", %{"team" => params}, socket) do
    case Teams.update_hub(socket.assigns.hub, params) do
      {:ok, hub} ->
        {:noreply,
         socket
         |> put_flash(:success, "Hub updated successfully")
         |> push_navigate(to: ~p"/hub/#{hub.id}")}

      {:error, changeset} ->
        {:noreply, assign_form(socket, changeset)}
    end
  end

  def handle_event("validate", %{"team" => attrs}, socket) do
    changeset =
      socket.assigns.hub
      |> Team.change_hub(attrs)
      |> Map.replace!(:action, :validate)

    {:noreply, assign_form(socket, changeset)}
  end

  def handle_event("delete_hub_secret", attrs, socket) do
    %{hub: hub} = socket.assigns

    on_confirm = fn socket ->
      {:ok, secret} = Livebook.Secrets.update_secret(%Livebook.Secrets.Secret{}, attrs)

      case Livebook.Hubs.delete_secret(hub, secret) do
        :ok -> socket
        {:transport_error, reason} -> put_flash(socket, :error, reason)
      end
    end

    {:noreply,
     confirm(socket, on_confirm,
       title: "Delete hub secret - #{attrs["name"]}",
       description: "Are you sure you want to delete this hub secret?",
       confirm_text: "Delete",
       confirm_icon: "delete-bin-6-line"
     )}
  end

  def handle_event("validate_dockerfile", %{"data" => data}, socket) do
    changeset =
      data
      |> Hubs.Dockerfile.config_changeset()
      |> Map.replace!(:action, :validate)

    {:noreply,
     socket
     |> assign(config_changeset: changeset)
     |> update_dockerfile()}
  end

  def handle_event("mark_as_default", _, socket) do
    Hubs.set_default_hub(socket.assigns.hub.id)
    {:noreply, assign(socket, default?: true)}
  end

  def handle_event("remove_as_default", _, socket) do
    Hubs.unset_default_hub()
    {:noreply, assign(socket, default?: false)}
  end

  defp default_hub?(hub) do
    Hubs.get_default_hub().id == hub.id
  end

  defp assign_form(socket, %Ecto.Changeset{} = changeset) do
    assign(socket, form: to_form(changeset))
  end

  defp update_dockerfile(socket) do
    config =
      socket.assigns.config_changeset
      |> Ecto.Changeset.apply_changes()
      |> Map.replace!(:deploy_all, true)

    %{hub: hub, secrets: hub_secrets, file_systems: hub_file_systems} = socket.assigns

    dockerfile =
      Hubs.Dockerfile.build_dockerfile(config, hub, hub_secrets, hub_file_systems, nil, [], %{})

    assign(socket, :dockerfile, dockerfile)
  end
end
