defmodule LivebookWeb.Hub.Edit.TeamComponent do
  use LivebookWeb, :live_component

  alias Livebook.Hubs.{Provider, Team}
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
    zta = %{"provider" => "", "key" => ""}

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
       secret_value: secret_value,
       hub_metadata: Provider.to_metadata(assigns.hub),
       zta: zta
     )
     |> assign_dockerfile()
     |> assign_form(changeset)}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div>
      <LayoutHelpers.topbar :if={not @hub_metadata.connected?} variant={:warning}>
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
                </div>
              </LayoutHelpers.title>

              <p class="text-sm flex flex-row space-x-6 text-gray-700">
                <a href={org_url(@hub, "/")} class="hover:text-blue-600">
                  <.remix_icon icon="mail-line" /> Invite users
                </a>

                <a href={org_url(@hub, "/")} class="hover:text-blue-600">
                  <.remix_icon icon="settings-line" /> Manage organization
                </a>

                <a
                  phx-click={show_modal("show-key-modal")}
                  phx-target={@myself}
                  class="hover:text-blue-600 cursor-pointer"
                >
                  <.remix_icon icon="key-2-fill" /> Display Teams key
                </a>
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
                Zero Trust Authentication
              </h2>

              <p class="text-gray-700">
                Enables Zero Trust Authentication to be used as the identity provider.
              </p>

              <.form :let={f} for={@zta} phx-change="change_zta" phx-target={@myself}>
                <div class="grid grid-cols-1 md:grid-cols-2 gap-3">
                  <.select_field
                    name="provider"
                    label="Provider"
                    value={@zta["provider"]}
                    options={[
                      {"None", ""},
                      {"Cloudflare", "cloudflare"},
                      {"GoogleIAP", "google_iap"}
                    ]}
                  />
                  <span class={[@zta["provider"] == "" && "hidden"]}>
                    <.text_field
                      field={f[:key]}
                      label={"#{if @zta["provider"] == "cloudflare", do: "Team name (domain)", else: "Audience (aud)"}"}
                      phx-debounce
                    />
                  </span>
                </div>
                <%= if @zta["provider"] == "cloudflare" do %>
                  <span>
                    See the
                    <a
                      class="text-blue-800 hover:text-blue-600"
                      href="https://developers.cloudflare.com/cloudflare-one/"
                    >
                      CloudFlare docs
                    </a>
                    for more information about Cloudflare Zero Trust.
                  </span>
                <% end %>
                <%= if @zta["provider"] == "google_iap" do %>
                  <span>
                    See the
                    <a
                      class="text-blue-800 hover:text-blue-600"
                      href="https://cloud.google.com/iap/docs/concepts-overview"
                    >
                      Google docs
                    </a>
                    for more information about Google Identity-Aware Proxy (IAP).
                  </span>
                <% end %>
              </.form>
            </div>

            <div class="flex flex-col space-y-4">
              <h2 class="text-xl text-gray-800 font-medium pb-2 border-b border-gray-200">
                Airgapped Deployment
              </h2>

              <p class="text-gray-700">
                It is possible to deploy notebooks that belong to this Hub in an airgapped
                deployment, without connecting back to Livebook Teams server. This is done
                using the Docker image template below, which encrypts all of your Hub metadata,
                and taking some additional steps.
              </p>

              <div id="env-code">
                <div class="flex justify-between items-end">
                  <span class="text-sm text-gray-700 font-semibold">Dockerfile</span>
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
                    <.remix_icon icon="clipboard-line" class="align-middle mr-1 text-xs" />
                    <span class="font-normal text-xs">Copy source</span>
                  </button>
                </div>

                <.code_preview
                  source_id={"offline-deployment-#{@hub.id}-source"}
                  source={@dockerfile}
                  language="dockerfile"
                />
              </div>

              <p class="text-gray-700 py-2">
                Before deployment, you must perform the following manual steps:
              </p>

              <ol class="text-gray-700 space-y-3 list-disc list-inside">
                <li>
                  You must change <code>/path/to/my/notebooks</code> in the template above
                  to point to a directory with the <code>.livemd</code> files you want to deploy
                </li>
                <li>
                  You must set the <code>LIVEBOOK_TEAMS_KEY</code> environment variable
                  directly on your deployment platform, with the value you can find at the
                  top of this page
                </li>
                <li>
                  You may set the <code>LIVEBOOK_PASSWORD</code> environment variable to any
                  value of your choice, if you want to access and debug your deployed notebooks
                  in production
                </li>
              </ol>
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
        This is your <strong>Teams key</strong>. This key is
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

  def handle_event("change_zta", %{"provider" => ""}, socket) do
    zta = %{"provider" => "", "key" => ""}
    {:noreply, assign(socket, zta: zta) |> assign_dockerfile()}
  end

  def handle_event("change_zta", %{"provider" => provider, "key" => key}, socket) do
    zta = %{"provider" => provider, "key" => key}
    {:noreply, assign(socket, zta: zta) |> assign_dockerfile()}
  end

  defp assign_form(socket, %Ecto.Changeset{} = changeset) do
    assign(socket, form: to_form(changeset))
  end

  defp assign_dockerfile(socket) do
    version = to_string(Application.spec(:livebook, :vsn))
    version = if version =~ "dev", do: "edge", else: version

    base =
      """
      FROM ghcr.io/livebook-dev/livebook:#{version}

      ENV LIVEBOOK_APPS_PATH_HUB_ID "#{socket.assigns.hub.id}"
      ENV LIVEBOOK_TEAMS_NAME "#{socket.assigns.hub.hub_name}"
      ENV LIVEBOOK_TEAMS_OFFLINE_KEY "#{socket.assigns.hub.org_public_key}"
      ENV LIVEBOOK_TEAMS_SECRETS "#{encrypt_secrets_to_dockerfile(socket)}"

      """

    apps =
      """
      COPY /path/to/my/notebooks /apps
      ENV LIVEBOOK_APPS_PATH "/apps"
      ENV LIVEBOOK_APPS_PATH_WARMUP "manual"
      RUN /app/bin/warmup_apps.sh\
      """

    zta = zta_env(socket.assigns.zta)
    dockerfile = if zta, do: base <> zta <> apps, else: base <> apps

    assign(socket, :dockerfile, dockerfile)
  end

  defp encrypt_secrets_to_dockerfile(socket) do
    {secret_key, sign_secret} = Livebook.Teams.derive_keys(socket.assigns.hub.teams_key)

    secrets_map =
      for %{name: name, value: value} <- socket.assigns.secrets,
          into: %{},
          do: {name, value}

    stringified_secrets = Jason.encode!(secrets_map)

    Livebook.Teams.encrypt(stringified_secrets, secret_key, sign_secret)
  end

  defp zta_env(%{"provider" => ""}), do: nil
  defp zta_env(%{"key" => ""}), do: nil

  defp zta_env(%{"provider" => provider, "key" => key}) do
    """
    ENV LIVEBOOK_IDENTITY_PROVIDER "#{provider}:#{key}"

    """
  end
end
