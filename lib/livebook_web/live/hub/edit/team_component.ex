defmodule LivebookWeb.Hub.Edit.TeamComponent do
  use LivebookWeb, :live_component

  alias Livebook.Hubs.Team
  alias Livebook.Teams
  alias LivebookWeb.LayoutHelpers
  alias LivebookWeb.NotFoundError

  @impl true
  def update(assigns, socket) do
    socket = assign(socket, assigns)
    changeset = Team.change_hub(assigns.hub)
    show_key? = assigns.params["show-key"] == "true"
    secrets = Livebook.Hubs.get_secrets(assigns.hub)
    secret_name = assigns.params["secret_name"]

    secret_value =
      if assigns.live_action == :edit_secret do
        Enum.find_value(secrets, &(&1.name == secret_name and &1.value)) ||
          raise(NotFoundError, "could not find secret matching #{inspect(secret_name)}")
      end

    {:ok,
     socket
     |> assign(
       secrets: secrets,
       show_key: show_key?,
       secret_name: secret_name,
       secret_value: secret_value
     )
     |> assign_dockerfile()
     |> assign_form(changeset)}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div id={"#{@id}-component"}>
      <div class="mb-8 flex flex-col space-y-8">
        <div class="flex justify-between">
          <LayoutHelpers.title text={"#{@hub.hub_emoji} #{@hub.hub_name}"} />

          <div class="flex justify-end gap-2">
            <button
              phx-click={show_modal("show-key-modal")}
              phx-target={@myself}
              class="button-base button-outlined-gray"
            >
              <span class="hidden sm:block">Teams key</span>
              <.remix_icon icon="key-2-fill" class="text-xl sm:hidden" />
            </button>
          </div>
        </div>
        <div>
          <p class="text-gray-700">
            A shared Teams hub. All resources here are shared with your team. Manage users and billing on livebook.dev.
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
              for={@form}
              phx-submit="save"
              phx-change="validate"
              phx-target={@myself}
            >
              <div class="grid grid-cols-1 md:grid-cols-2 gap-3 mt-2">
                <div class="tooltip top close-gap flex-col" data-tooltip="Name cannot be changed.">
                  <.text_field
                    field={f[:hub_name]}
                    label="Name"
                    disabled
                    class="bg-gray-200/50 border-200/80 cursor-not-allowed"
                  />
                </div>
                <.emoji_field field={f[:hub_emoji]} label="Emoji" />
              </div>

              <div>
                <button class="button-base button-blue" type="submit" phx-disable-with="Updating...">
                  Update Hub
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
              They are often shared with Smart cells and can be read as
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
              Offline Deployment
            </h2>

            <p class="text-gray-700">
              Deploy your stamped notebooks with your Hub
              using an instance of the Hub using
              environment variables.
            </p>

            <div id="env-code">
              <div class="flex justify-between items-center mb-2 px-2">
                <span class="text-sm text-gray-700 font-semibold"> Dockerfile </span>
                <button
                  class="button-base button-gray whitespace-nowrap py-1 px-2"
                  data-copy
                  data-tooltip="Copied to clipboard"
                  type="button"
                  aria-label="copy to clipboard"
                  phx-click={
                    JS.dispatch("lb:clipcopy", to: "#offline-deployment-#{@hub.id}-source")
                    |> JS.add_class(
                      "tooltip top",
                      to: "#env-code [data-copy]",
                      transition: {"ease-out duration-200", "opacity-0", "opacity-100"}
                    )
                    |> JS.remove_class(
                      "tooltip top",
                      to: "#env-code [data-copy]",
                      transition: {"ease-out duration-200", "opacity-0", "opacity-100"},
                      time: 2000
                    )
                  }
                >
                  <.remix_icon icon="clipboard-line" class="align-middle mr-1 text-" /> Copy source
                </button>
              </div>

              <.code_preview
                source_id={"offline-deployment-#{@hub.id}-source"}
                source={@dockerfile}
                language="dockerfile"
                wrap
              />
            </div>
          </div>

          <div class="flex flex-col space-y-4">
            <h2 class="text-xl text-gray-800 font-medium pb-2 border-b border-gray-200">
              Danger Zone
            </h2>

            <div class="flex items-center justify-between gap-4 text-gray-700">
              <div class="flex flex-col">
                <h3 class="font-semibold">
                  Delete this hub
                </h3>
                <p>Once deleted, you won’t be able to access its features unless you rejoin.</p>
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
    </div>
    """
  end

  defp teams_key_modal(assigns) do
    ~H"""
    <div class="p-6 flex flex-col space-y-5">
      <h3 class="text-2xl font-semibold text-gray-800">
        Teams Key
      </h3>
      <div class="justify-center">
        This is your <strong>Teams Key</strong>. If you want to join or invite others
        to your organization, you will need to share your Teams Key with them. We
        recommend storing it somewhere safe:
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
              data-copy
              data-tooltip="Copied to clipboard"
              type="button"
              aria-label="copy to clipboard"
              phx-click={
                JS.dispatch("lb:clipcopy", to: "#teams-key")
                |> JS.add_class(
                  "tooltip top",
                  to: "#teams-key-toggle [data-copy]",
                  transition: {"ease-out duration-200", "opacity-0", "opacity-100"}
                )
                |> JS.remove_class(
                  "tooltip top",
                  to: "#teams-key-toggle [data-copy]",
                  transition: {"ease-out duration-200", "opacity-0", "opacity-100"},
                  time: 2000
                )
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
        :ok ->
          socket
          |> put_flash(:success, "Secret deleted successfully")
          |> push_navigate(to: ~p"/hub/#{hub.id}")

        {:transport_error, reason} ->
          put_flash(socket, :error, reason)
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

  defp assign_form(socket, %Ecto.Changeset{} = changeset) do
    assign(socket, form: to_form(changeset))
  end

  defp assign_dockerfile(socket) do
    version = to_string(Application.spec(:livebook, :vsn))
    version = if version =~ "dev", do: "edge", else: version

    assign(socket, :dockerfile, """
    FROM livebook/livebook:#{version}

    COPY /path/to/my/notebooks /data
    ENV LIVEBOOK_APPS_PATH "/data"

    ENV LIVEBOOK_APPS_PATH_HUB_ID "#{socket.assigns.hub.id}"
    ENV LIVEBOOK_TEAMS_NAME "#{socket.assigns.hub.hub_name}"
    ENV LIVEBOOK_TEAMS_OFFLINE_KEY "#{socket.assigns.hub.org_public_key}"

    CMD [ "/app/bin/livebook", "start" ]\
    """)
  end
end
