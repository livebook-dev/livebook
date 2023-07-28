defmodule LivebookWeb.SettingsLive do
  use LivebookWeb, :live_view

  alias LivebookWeb.LayoutHelpers

  on_mount LivebookWeb.SidebarHook

  @impl true
  def mount(_params, _session, socket) do
    if connected?(socket) do
      Livebook.Settings.subscribe()
    end

    {:ok,
     assign(socket,
       file_systems: Livebook.Settings.file_systems(),
       env_vars: Livebook.Settings.fetch_env_vars() |> Enum.sort(),
       env_var: nil,
       autosave_path_state: %{
         file: autosave_dir(),
         dialog_opened?: false
       },
       update_check_enabled: Livebook.UpdateCheck.enabled?(),
       page_title: "Settings - Livebook"
     )}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <LayoutHelpers.layout
      current_page={~p"/settings"}
      current_user={@current_user}
      saved_hubs={@saved_hubs}
    >
      <div id="settings-page" class="p-4 md:px-12 md:py-7 max-w-screen-md mx-auto space-y-16">
        <!-- System settings section -->
        <div class="flex flex-col space-y-10">
          <div>
            <LayoutHelpers.title text="System settings" />
            <p class="mt-4 text-gray-700">
              Here you can change global Livebook configuration. Keep in mind
              that this configuration gets persisted and will be restored on application
              launch.
            </p>
          </div>
          <!-- System details -->
          <div class="flex flex-col space-y-2">
            <h2 class="text-xl text-gray-800 font-medium">
              About
            </h2>
            <div class="flex flex-col sm:flex-row gap-4 sm:gap-12 items-center justify-center sm:justify-between border border-gray-200 rounded-lg p-4">
              <div class="flex justify-center sm:items-center space-x-12">
                <.labeled_text :if={app_name = Livebook.Config.app_service_name()} label="Application">
                  <%= if app_url = Livebook.Config.app_service_url() do %>
                    <a href={app_url} class="underline hover:no-underline" target="_blank">
                      <%= app_name %>
                    </a>
                  <% else %>
                    <%= app_name %>
                  <% end %>
                </.labeled_text>
                <.labeled_text label="Livebook">
                  v<%= Application.spec(:livebook, :vsn) %>
                </.labeled_text>
                <.labeled_text label="Elixir">
                  v<%= System.version() %>
                </.labeled_text>
              </div>

              <div class="self-center">
                <.link navigate={~p"/dashboard"} class="button-base button-outlined-gray">
                  <.remix_icon icon="dashboard-2-line" class="align-middle mr-1" />
                  <span>Open dashboard</span>
                </.link>
              </div>
            </div>
          </div>
          <!-- Updates -->
          <div class="flex flex-col space-y-4">
            <h2 class="text-xl text-gray-800 font-medium pb-2 border-b border-gray-200">
              Updates
            </h2>
            <form class="mt-4" phx-change="save" phx-nosubmit>
              <.switch_field
                name="update_check_enabled"
                label="Show banner when a new Livebook version is available"
                value={@update_check_enabled}
              />
            </form>
          </div>
          <!-- Autosave path configuration -->
          <div class="flex flex-col space-y-4">
            <h2 class="text-xl text-gray-800 font-medium pb-2 border-b border-gray-200">
              Autosave
            </h2>
            <p class="text-gray-700">
              The directory to keep unsaved notebooks.
            </p>
            <.autosave_path_select state={@autosave_path_state} />
          </div>
          <!-- File systems configuration -->
          <div class="flex flex-col space-y-4">
            <h2 class="text-xl text-gray-800 font-medium pb-2 border-b border-gray-200">
              File systems
            </h2>
            <p class="mt-4 text-gray-700">
              File systems are used to store notebooks. The local disk file system
              is visible only to the current machine, but alternative file systems
              are available, such as S3-based storages.
            </p>
            <LivebookWeb.SettingsLive.FileSystemsComponent.render file_systems={@file_systems} />
          </div>
          <!-- Environment variables configuration -->
          <div class="flex flex-col space-y-4">
            <h2 class="text-xl text-gray-800 font-medium pb-2 border-b border-gray-200">
              Environment variables
            </h2>
            <p class="mt-4 text-gray-700">
              Environment variables store global values, specific to this
              Livebook instance, which are available inside your notebooks.
              You can also configure the <code>PATH</code> environment to
              make system dependencies available to notebooks.
            </p>
            <.live_component
              module={LivebookWeb.SettingsLive.EnvVarsComponent}
              id="env-vars"
              env_vars={@env_vars}
              return_to={~p"/settings"}
              add_env_var_path={~p"/settings/env-var/new"}
            />
          </div>
        </div>
        <!-- User settings section -->
        <div class="flex flex-col space-y-10 pb-8">
          <div>
            <LayoutHelpers.title text="User settings" />
            <p class="mt-4 text-gray-700">
              The configuration in this section changes only your Livebook
              experience and is saved in your browser.
            </p>
          </div>
          <!-- Editor configuration -->
          <div class="flex flex-col space-y-4">
            <h2 class="text-xl text-gray-800 font-medium pb-2 border-b border-gray-200">
              Code editor
            </h2>
            <div
              class="flex flex-col space-y-3"
              id="editor-settings"
              phx-hook="EditorSettings"
              phx-update="ignore"
            >
              <.switch_field
                name="editor_auto_completion"
                label="Show completion list while typing"
                value={false}
              />
              <.switch_field
                name="editor_auto_signature"
                label="Show function signature while typing"
                value={false}
              />
              <.switch_field name="editor_font_size" label="Increase font size" value={false} />
              <.switch_field name="editor_light_theme" label="Use light theme" value={false} />
              <.switch_field
                name="editor_markdown_word_wrap"
                label="Wrap words in Markdown"
                value={false}
              />
            </div>
          </div>
        </div>
      </div>
    </LayoutHelpers.layout>

    <.modal
      :if={@live_action == :add_file_system}
      id="add-file-system-modal"
      show
      width={:medium}
      patch={~p"/settings"}
    >
      <.live_component
        module={LivebookWeb.SettingsLive.AddFileSystemComponent}
        id="add-file-system"
        return_to={~p"/settings"}
      />
    </.modal>

    <.modal
      :if={@live_action in [:add_env_var, :edit_env_var]}
      id="env-var-modal"
      show
      width={:medium}
      patch={~p"/settings"}
    >
      <.live_component
        module={LivebookWeb.SettingsLive.EnvVarComponent}
        id="env-var"
        env_var={@env_var}
        headline="Configure your application global environment variables."
        return_to={~p"/settings"}
      />
    </.modal>
    """
  end

  defp autosave_path_select(%{state: %{file: nil}} = assigns), do: ~H""

  defp autosave_path_select(%{state: %{dialog_opened?: true}} = assigns) do
    ~H"""
    <div class="w-full h-52">
      <.live_component
        module={LivebookWeb.FileSelectComponent}
        id="autosave-path-component"
        file={@state.file}
        extnames={[]}
        running_files={[]}
        submit_event={:set_autosave_path}
        file_system_select_disabled={true}
        target={self()}
      >
        <button class="button-base button-gray" phx-click="cancel_autosave_path" tabindex="-1">
          Cancel
        </button>
        <button class="button-base button-gray" phx-click="reset_autosave_path" tabindex="-1">
          Reset
        </button>
        <button
          class="button-base button-blue"
          phx-click="set_autosave_path"
          disabled={not Livebook.FileSystem.File.dir?(@state.file)}
          tabindex="-1"
        >
          Save
        </button>
      </.live_component>
    </div>
    """
  end

  defp autosave_path_select(assigns) do
    ~H"""
    <div class="flex">
      <input class="input mr-2" readonly value={@state.file.path} />
      <button class="button-base button-gray" phx-click="open_autosave_path_select">
        Change
      </button>
    </div>
    """
  end

  @impl true
  def handle_params(%{"env_var_id" => key}, _url, socket) do
    env_var = Livebook.Settings.fetch_env_var!(key)
    {:noreply, assign(socket, env_var: env_var)}
  end

  def handle_params(%{"file_system_id" => file_system_id}, _url, socket) do
    {:noreply, assign(socket, file_system_id: file_system_id)}
  end

  def handle_params(_params, _url, socket), do: {:noreply, assign(socket, env_var: nil)}

  @impl true
  def handle_event("cancel_autosave_path", %{}, socket) do
    {:noreply,
     update(
       socket,
       :autosave_path_state,
       &%{&1 | dialog_opened?: false, file: autosave_dir()}
     )}
  end

  def handle_event("set_autosave_path", %{}, socket) do
    path = socket.assigns.autosave_path_state.file.path

    Livebook.Settings.set_autosave_path(path)

    {:noreply,
     update(
       socket,
       :autosave_path_state,
       &%{&1 | dialog_opened?: false, file: autosave_dir()}
     )}
  end

  @impl true
  def handle_event("reset_autosave_path", %{}, socket) do
    {:noreply,
     update(
       socket,
       :autosave_path_state,
       &%{&1 | file: default_autosave_dir()}
     )}
  end

  def handle_event("open_autosave_path_select", %{}, socket) do
    {:noreply, update(socket, :autosave_path_state, &%{&1 | dialog_opened?: true})}
  end

  def handle_event("detach_file_system", %{"id" => file_system_id}, socket) do
    on_confirm = fn socket ->
      Livebook.Settings.remove_file_system(file_system_id)
      file_systems = Livebook.Settings.file_systems()
      assign(socket, file_systems: file_systems)
    end

    {:noreply,
     confirm(socket, on_confirm,
       title: "Detach file system",
       description:
         "Are you sure you want to detach this file system? Any sessions using it will keep the access until they get closed.",
       confirm_text: "Detach",
       confirm_icon: "close-circle-line"
     )}
  end

  def handle_event("save", %{"update_check_enabled" => enabled}, socket) do
    enabled = enabled == "true"
    Livebook.UpdateCheck.set_enabled(enabled)
    {:noreply, assign(socket, :update_check_enabled, enabled)}
  end

  def handle_event("save", %{"env_var" => attrs}, socket) do
    env_var = socket.assigns.env_var || %Livebook.Settings.EnvVar{}

    case Livebook.Settings.set_env_var(env_var, attrs) do
      {:ok, _} ->
        {:noreply, push_patch(socket, to: ~p"/settings")}

      {:error, _changeset} ->
        {:noreply, socket}
    end
  end

  def handle_event("edit_env_var", %{"env_var" => key}, socket) do
    {:noreply, push_patch(socket, to: "/settings/env-var/edit/#{key}")}
  end

  def handle_event("delete_env_var", %{"env_var" => key}, socket) do
    on_confirm = fn socket ->
      Livebook.Settings.unset_env_var(key)
      socket
    end

    {:noreply,
     confirm(socket, on_confirm,
       title: "Delete #{key}",
       description: "Are you sure you want to delete environment variable?",
       confirm_text: "Delete",
       confirm_icon: "delete-bin-6-line"
     )}
  end

  @impl true
  def handle_info({:file_systems_updated, file_systems}, socket) do
    {:noreply, assign(socket, file_systems: file_systems)}
  end

  def handle_info({:set_file, file, _info}, socket) do
    {:noreply, update(socket, :autosave_path_state, &%{&1 | file: file})}
  end

  def handle_info(:set_autosave_path, socket) do
    handle_event("set_autosave_path", %{}, socket)
  end

  def handle_info({:env_var_set, env_var}, socket) do
    idx = Enum.find_index(socket.assigns.env_vars, &(&1.name == env_var.name))

    env_vars =
      if idx,
        do: List.replace_at(socket.assigns.env_vars, idx, env_var),
        else: [env_var | socket.assigns.env_vars]

    {:noreply, assign(socket, env_vars: Enum.sort(env_vars), env_var: nil)}
  end

  def handle_info({:env_var_unset, env_var}, socket) do
    env_vars = Enum.reject(socket.assigns.env_vars, &(&1.name == env_var.name))

    {:noreply, assign(socket, env_vars: env_vars, env_var: nil)}
  end

  def handle_info(_message, socket), do: {:noreply, socket}

  defp autosave_dir() do
    if path = Livebook.Settings.autosave_path() do
      path
      |> Livebook.FileSystem.Utils.ensure_dir_path()
      |> Livebook.FileSystem.File.local()
    end
  end

  defp default_autosave_dir() do
    Livebook.Settings.default_autosave_path()
    |> Livebook.FileSystem.Utils.ensure_dir_path()
    |> Livebook.FileSystem.File.local()
  end
end
