defmodule LivebookWeb.SessionLive.AppSettingsComponent do
  use LivebookWeb, :live_component

  alias Livebook.Notebook.AppSettings
  import LivebookWeb.Confirm

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
    <div class="flex flex-col space-y-8">
      <h3 class="text-2xl font-semibold text-gray-800">
        App settings
      </h3>
      <.message_box :if={@context == "preview"} kind="info">
        You must configure your app before previewing it.
      </.message_box>
      <.message_box :if={@context && @context != "preview"} kind="info">
        You must configure your app before deploying it.
      </.message_box>

      <.form
        :let={f}
        for={@changeset}
        phx-change="validate"
        phx-submit="save"
        phx-target={@myself}
        autocomplete="off"
      >
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
          <.button disabled={not @changeset.valid?}>
            {if @context == "preview", do: "Launch", else: "Save"}
          </.button>

          <.button color="gray" outlined type="reset" name="reset">
            Reset
          </.button>
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

    {:noreply, assign(socket, changeset: changeset)}
  end

  def handle_event("save", %{"app_settings" => params}, socket) do
    app_settings = socket.assigns.settings
    deployed_app_slug = socket.assigns.deployed_app_slug

    app_settings =
      case AppSettings.update(app_settings, params) do
        {:ok, app_settings} ->
          Livebook.Session.set_app_settings(socket.assigns.session.pid, app_settings)
          app_settings

        {:error, _changeset} ->
          app_settings
      end

    case socket.assigns.context do
      "preview" ->
        {:noreply, preview_app(socket, app_settings, deployed_app_slug)}

      redirect when is_binary(redirect) ->
        {:noreply, push_patch(socket, to: ~p"/sessions/#{socket.assigns.session.id}/#{redirect}")}

      nil ->
        {:noreply, push_patch(socket, to: ~p"/sessions/#{socket.assigns.session.id}")}
    end
  end

  def preview_app(socket, app_settings, deployed_app_slug) do
    on_confirm = fn socket ->
      Livebook.Session.deploy_app(socket.assigns.session.pid)
      push_patch(socket, to: ~p"/sessions/#{socket.assigns.session.id}")
    end

    slug = app_settings.slug

    app =
      case Livebook.Apps.fetch_app(slug) do
        {:ok, app} -> app
        :error -> nil
      end

    permanent? = app && app.permanent
    slug_taken? = slug != deployed_app_slug and app != nil

    cond do
      permanent? ->
        socket
        |> put_flash(
          :error,
          "A permanent app with this slug already exists and cannot be replaced."
        )
        |> push_patch(to: ~p"/sessions/#{socket.assigns.session.id}")

      slug_taken? ->
        confirm(socket, on_confirm,
          title: "Deploy app",
          description:
            "An app with this slug already exists, do you want to deploy a new version?",
          confirm_text: "Replace"
        )

      true ->
        on_confirm.(socket)
    end
  end
end
