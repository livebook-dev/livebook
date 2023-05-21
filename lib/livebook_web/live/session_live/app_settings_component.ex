defmodule LivebookWeb.SessionLive.AppSettingsComponent do
  use LivebookWeb, :live_component

  alias Livebook.Notebook.AppSettings

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
    <div class="p-6 max-w-4xl flex flex-col space-y-8">
      <h3 class="text-2xl font-semibold text-gray-800">
        App settings
      </h3>
      <.form :let={f} for={@changeset} phx-change="validate" phx-target={@myself} autocomplete="off">
        <div class="flex flex-col space-y-4">
          <.text_field field={f[:slug]} label="Slug" spellcheck="false" phx-debounce />
          <div class="flex flex-col space-y-1">
            <.checkbox_field
              field={f[:access_type]}
              label="Password-protected"
              checked_value="protected"
              unchecked_value="public"
            />
            <%= if Ecto.Changeset.get_field(@changeset, :access_type) == :protected do %>
              <.password_field field={f[:password]} spellcheck="false" phx-debounce />
            <% end %>
          </div>
          <.radio_button_group_field
            field={f[:multi_session]}
            options={[{"false", "Single-session"}, {"true", "Multi-session"}]}
            label="Session type"
            help={
              ~S'''
              Whether all users should share the
              same session or be able to manually
              start multiple sessions.
              '''
            }
            full_width
          />
          <.select_field
            field={f[:auto_shutdown_ms]}
            label="Shutdown after inactivity"
            options={[
              {"Never", ""},
              {"5 seconds", "5000"},
              {"1 minute", "60000"},
              {"1 hour", "3600000"}
            ]}
            help={
              ~S'''
              Shuts down the session if it has no
              users for the specified amount of time.
              '''
            }
          />
          <.checkbox_field
            field={f[:show_source]}
            label="Show source"
            help={
              ~S'''
              When enabled, it makes notebook source
              accessible in the app menu.
              '''
            }
          />
          <.checkbox_field
            field={f[:output_type]}
            label="Only render rich outputs"
            checked_value="rich"
            unchecked_value="all"
            help={
              ~S'''
              When enabled, hides simple outputs
              and only shows rich elements, such
              as inputs, frames, tables, etc.
              '''
            }
          />
          <%= if Ecto.Changeset.get_field(@changeset, :multi_session) do %>
            <.checkbox_field
              field={f[:show_existing_sessions]}
              label="List existing sessions"
              help={
                ~S'''
                When enabled, the user can see all
                currently running sessions and join
                them before creating a new session.
                '''
              }
            />
          <% else %>
            <.checkbox_field
              field={f[:zero_downtime]}
              label="Zero-downtime deployment"
              help={
                ~S'''
                When enabled, a new version only becomes
                available after it executes all of its
                cells, making sure it is ready to use.
                If an error happens, deploy is aborted.
                '''
              }
            />
          <% end %>
        </div>
        <div class="mt-8 flex space-x-2">
          <button
            class="button-base button-blue"
            type="button"
            phx-click={JS.patch(~p"/sessions/#{@session.id}") |> JS.push("deploy_app")}
            disabled={not @changeset.valid?}
          >
            <.remix_icon icon="rocket-line" class="align-middle mr-1" />
            <span>Deploy</span>
          </button>
          <button class="button-base button-outlined-gray" type="reset" name="reset">
            Reset
          </button>
        </div>
      </.form>
    </div>
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
end
