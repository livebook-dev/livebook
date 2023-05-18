defmodule LivebookWeb.SessionLive.AppInfoComponent do
  use LivebookWeb, :live_component

  import LivebookWeb.AppHelpers

  alias Livebook.Notebook.AppSettings

  @impl true
  def mount(socket) do
    {:ok, assign(socket, deploy_confirmation: false)}
  end

  @impl true
  def update(assigns, socket) do
    changeset =
      case socket.assigns do
        %{changeset: changeset} when changeset.data == assigns.settings -> changeset
        _ -> AppSettings.change(assigns.settings)
      end

    {:ok,
     socket
     |> assign(assigns)
     |> assign(changeset: changeset)}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="flex flex-col">
      <div class="flex items-center justify-between">
        <h3 class="uppercase text-sm font-semibold text-gray-500">
          App settings
        </h3>
        <.app_info_icon />
      </div>
      <div class="mt-2">
        <.app_form
          changeset={@changeset}
          deploy_confirmation={@deploy_confirmation}
          session={@session}
          myself={@myself}
        />
      </div>
      <%= if @app do %>
        <h3 class="mt-16 uppercase text-sm font-semibold text-gray-500">
          Deployment
        </h3>
        <div class="mt-2 border border-gray-200 rounded-lg">
          <div class="p-4 flex flex-col space-y-3">
            <.labeled_text label="URL" one_line>
              <a href={~p"/apps/#{@app.slug}"}>
                <%= ~p"/apps/#{@app.slug}" %>
              </a>
            </.labeled_text>
            <.labeled_text label="Version" one_line>
              v<%= @app.version %>
            </.labeled_text>
          </div>
          <div class="border-t border-gray-200 px-3 py-2 flex space-x-2">
            <div class="grow" />
            <span class="tooltip top" data-tooltip="Terminate">
              <button
                class="icon-button"
                aria-label="terminate app"
                phx-click={JS.push("terminate_app", target: @myself)}
              >
                <.remix_icon icon="delete-bin-6-line" class="text-lg" />
              </button>
            </span>
          </div>
        </div>
        <div class="mt-2 text-gray-600 font-medium text-sm">
          App sessions
        </div>
        <div class="mt-2 flex flex-col space-y-4">
          <div :for={app_session <- @app.sessions} class="border border-gray-200 rounded-lg">
            <div class="p-4 flex flex-col space-y-3">
              <.labeled_text label="Status">
                <a
                  class="inline-block"
                  aria-label="debug app"
                  href={app_session.app_status == :error && ~p"/sessions/#{app_session.id}"}
                  target="_blank"
                >
                  <.app_status status={app_session.app_status} />
                </a>
              </.labeled_text>
              <.labeled_text label="Version">
                v<%= app_session.version %>
              </.labeled_text>
            </div>
            <div class="border-t border-gray-200 px-3 py-2 flex space-x-2">
              <span class="tooltip top" data-tooltip="Open">
                <a
                  class={[
                    "icon-button",
                    not Livebook.Session.Data.app_active?(app_session.app_status) && "disabled"
                  ]}
                  aria-label="open app"
                  href={~p"/apps/#{@app.slug}/#{app_session.id}"}
                >
                  <.remix_icon icon="link" class="text-lg" />
                </a>
              </span>
              <div class="grow" />
              <span class="tooltip top" data-tooltip="Debug">
                <a class="icon-button" aria-label="debug app" href={~p"/sessions/#{app_session.id}"}>
                  <.remix_icon icon="terminal-line" class="text-lg" />
                </a>
              </span>
              <%= if app_session.app_status in [:deactivated, :shutting_down] do %>
                <span class="tooltip top" data-tooltip="Terminate">
                  <button
                    class="icon-button"
                    aria-label="terminate app session"
                    phx-click={
                      JS.push("terminate_app_session",
                        value: %{session_id: app_session.id},
                        target: @myself
                      )
                    }
                  >
                    <.remix_icon icon="delete-bin-6-line" class="text-lg" />
                  </button>
                </span>
              <% else %>
                <span class="tooltip top" data-tooltip="Deactivate">
                  <button
                    class="icon-button"
                    aria-label="deactivate app session"
                    phx-click={
                      JS.push("deactivate_app_session",
                        value: %{session_id: app_session.id},
                        target: @myself
                      )
                    }
                  >
                    <.remix_icon icon="stop-circle-line" class="text-lg" />
                  </button>
                </span>
              <% end %>
            </div>
          </div>
        </div>
      <% end %>
    </div>
    """
  end

  defp app_info_icon(assigns) do
    ~H"""
    <span
      class="icon-button cursor-pointer tooltip bottom-left"
      data-tooltip={
        ~S'''
        App deployment is a way to share your
        notebook for people to interact with. Use
        inputs and controls to build interactive
        UIs, perfect for demos and tasks.
        '''
      }
    >
      <.remix_icon icon="question-line" class="text-xl leading-none" />
    </span>
    """
  end

  defp app_form(%{session: %{mode: :app}} = assigns) do
    ~H"""
    <div class="mt-5 flex flex-col space-y-6">
      <span class="text-gray-700 text-sm">
        This session is a running app. To deploy a modified version, you can fork it.
      </span>
      <div>
        <button class="button-base button-blue" phx-click="fork_session">
          <.remix_icon icon="git-branch-line" />
          <span>Fork</span>
        </button>
      </div>
    </div>
    """
  end

  defp app_form(%{deploy_confirmation: true} = assigns) do
    ~H"""
    <div class="mt-5">
      <span class="text-gray-700 text-sm">
        An app with this slug already exists, do you want to deploy a new version?
      </span>
      <div class="mt-5 flex space-x-2">
        <button
          class="button-base button-red"
          phx-click="deploy_confirmation_confirm"
          phx-target={@myself}
        >
          Replace
        </button>
        <button
          class="button-base button-outlined-gray bg-transparent"
          phx-click="deploy_confirmation_cancel"
          phx-target={@myself}
        >
          Cancel
        </button>
      </div>
    </div>
    """
  end

  defp app_form(assigns) do
    ~H"""
    <.form
      :let={f}
      for={@changeset}
      phx-submit="deploy"
      phx-change="validate"
      phx-target={@myself}
      autocomplete="off"
    >
      <div class="flex flex-col space-y-4">
        <.text_field
          field={f[:slug]}
          label="Slug"
          spellcheck="false"
          phx-debounce
          class="bg-gray-100"
        />
        <.radio_button_group_field
          field={f[:multi_session]}
          options={[{"false", "Single-session"}, {"true", "Multi-session"}]}
          full_width
        />
        <.select_field
          field={f[:auto_shutdown_type]}
          label="Auto shutdown"
          options={[
            {"Never", "never"},
            {"No users", "inactive_5s"},
            {"No users for 1 minute", "inactive_1m"},
            {"No users for 1 hour", "inactive_1h"},
            {"New version", "new_version"}
          ]}
        />
        <%= unless Ecto.Changeset.get_field(@changeset, :multi_session) do %>
          <.checkbox_field field={f[:zero_downtime]} label="Zero-downtime deployment" />
        <% end %>
        <%= if Ecto.Changeset.get_field(@changeset, :multi_session) do %>
          <.checkbox_field field={f[:auto_session_startup]} label="Start sessions automatically" />
        <% end %>
        <div class="flex flex-col space-y-1">
          <.checkbox_field
            field={f[:access_type]}
            label="Password-protected"
            checked_value="protected"
            unchecked_value="public"
          />
          <%= if Ecto.Changeset.get_field(@changeset, :access_type) == :protected do %>
            <.password_field field={f[:password]} spellcheck="false" phx-debounce class="bg-gray-100" />
          <% end %>
        </div>
        <.checkbox_field field={f[:show_source]} label="Show source" />
        <.radio_field
          field={f[:output_type]}
          label="Output type"
          options={[{"all", "All"}, {"rich", "Rich only"}]}
        />
      </div>
      <div class="mt-6 flex space-x-2">
        <button class="button-base button-blue" type="submit" disabled={not @changeset.valid?}>
          Deploy
        </button>
        <button class="button-base button-outlined-gray bg-transparent" type="reset" name="reset">
          Reset
        </button>
      </div>
    </.form>
    """
  end

  @impl true
  def handle_event("validate", %{"_target" => ["reset"]}, socket) do
    settings = AppSettings.new()
    Livebook.Session.set_app_settings(socket.assigns.session.pid, settings)
    {:noreply, assign(socket, changeset: AppSettings.change(settings))}
  end

  def handle_event("validate", %{"app_settings" => params}, socket) do
    changeset =
      socket.assigns.settings
      |> AppSettings.change(params)
      |> Map.put(:action, :validate)

    with {:ok, settings} <- AppSettings.update(socket.assigns.settings, params) do
      Livebook.Session.set_app_settings(socket.assigns.session.pid, settings)
    end

    {:noreply, assign(socket, changeset: changeset)}
  end

  def handle_event("deploy", %{"app_settings" => params}, socket) do
    case AppSettings.update(socket.assigns.settings, params) do
      {:ok, settings} ->
        Livebook.Session.set_app_settings(socket.assigns.session.pid, settings)

        if slug_taken?(settings.slug, socket.assigns.deployed_app_slug) do
          {:noreply, assign(socket, deploy_confirmation: true)}
        else
          Livebook.Session.deploy_app(socket.assigns.session.pid)
          {:noreply, socket}
        end

      {:error, _changeset} ->
        {:noreply, socket}
    end
  end

  def handle_event("deploy_confirmation_confirm", %{}, socket) do
    Livebook.Session.deploy_app(socket.assigns.session.pid)
    {:noreply, assign(socket, deploy_confirmation: false)}
  end

  def handle_event("deploy_confirmation_cancel", %{}, socket) do
    {:noreply, assign(socket, deploy_confirmation: false)}
  end

  def handle_event("terminate_app", %{}, socket) do
    {:noreply, confirm_app_termination(socket, socket.assigns.app.pid)}
  end

  def handle_event("terminate_app_session", %{"session_id" => session_id}, socket) do
    app_session = Enum.find(socket.assigns.app.sessions, &(&1.id == session_id))
    Livebook.Session.close(app_session.pid)
    {:noreply, socket}
  end

  def handle_event("deactivate_app_session", %{"session_id" => session_id}, socket) do
    app_session = Enum.find(socket.assigns.app.sessions, &(&1.id == session_id))
    Livebook.Session.app_deactivate(app_session.pid)
    {:noreply, socket}
  end

  defp slug_taken?(slug, deployed_app_slug) do
    slug != deployed_app_slug and Livebook.Apps.exists?(slug)
  end
end
