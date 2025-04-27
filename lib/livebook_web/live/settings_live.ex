defmodule LivebookWeb.SettingsLive do
  use LivebookWeb, :live_view

  alias LivebookWeb.LayoutComponents

  on_mount LivebookWeb.SidebarHook

  @impl true
  def mount(_params, _session, socket) do
    if connected?(socket) do
      Livebook.Settings.subscribe()
    end

    {:ok,
     assign(socket,
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
    <LayoutComponents.layout
      current_page={~p"/settings"}
      current_user={@current_user}
      saved_hubs={@saved_hubs}
    >
      <div id="settings-page" class="p-4 md:px-12 md:py-7 max-w-screen-md mx-auto space-y-16">
        <!-- System settings section -->
        <div class="flex flex-col space-y-10">
          <div>
            <LayoutComponents.title text="System settings" />
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
                      {app_name}
                    </a>
                  <% else %>
                    {app_name}
                  <% end %>
                </.labeled_text>
                <.labeled_text label="Livebook">
                  v{Livebook.Config.app_version()}
                </.labeled_text>
                <.labeled_text label="Elixir">
                  v{System.version()}
                </.labeled_text>
              </div>

              <div class="self-center">
                <.button navigate={~p"/dashboard"} color="gray" outlined>
                  <.remix_icon icon="dashboard-2-line" />
                  <span>Open dashboard</span>
                </.button>
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
            />

            <div>
              <.button patch={~p"/settings/env-var/new"} id="add-env-var">
                Add environment variable
              </.button>
            </div>
          </div>
        </div>
        <!-- User settings section -->
        <div class="flex flex-col space-y-10 pb-8">
          <div>
            <LayoutComponents.title text="User settings" />
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
              <.switch_field
                name="editor_auto_close_brackets"
                label="Automatically close brackets"
                value={false}
              />
              <.switch_field name="editor_font_size" label="Increase font size" value={false} />
              <.switch_field name="editor_ligatures" label="Render ligatures" value={false} />
              <.switch_field name="editor_light_theme" label="Use light theme" value={false} />
              <.switch_field
                name="editor_markdown_word_wrap"
                label="Wrap words in Markdown"
                value={false}
              />
              <div class="flex items-center gap-1 sm:gap-3 justify-between">
                <span class="text-gray-700 flex gap-1 items-center">
                  Key bindings
                </span>

                <.select_field
                  name="editor_mode"
                  value={false}
                  class="pt-1 pb-1"
                  options={[
                    {"Default", "default"},
                    {"Emacs", "emacs"},
                    {"Vim", "vim"}
                  ]}
                />
              </div>
            </div>
          </div>
        </div>
      </div>
    </LayoutComponents.layout>

    <.modal
      :if={@live_action in [:add_env_var, :edit_env_var]}
      id="env-var-modal"
      show
      width="medium"
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

  defp autosave_path_select(%{state: %{file: nil}} = assigns) do
    ~H"""
    <div>
      <.button color="gray" id="enable-autosave" phx-click="open_autosave_path_select">
        Enable
      </.button>
    </div>
    """
  end

  defp autosave_path_select(%{state: %{dialog_opened?: true}} = assigns) do
    ~H"""
    <div class="w-full h-52">
      <.live_component
        module={LivebookWeb.FileSelectComponent}
        id="autosave-path-component"
        file={@state.file}
        extnames={[]}
        running_files={[]}
        on_submit={JS.push("set_autosave_path")}
        file_system_select_disabled={true}
        target={self()}
      >
        <.button id="cancel-autosave" color="gray" phx-click="cancel_autosave_path" tabindex="-1">
          Cancel
        </.button>
        <.button id="reset-autosave" color="gray" phx-click="reset_autosave_path" tabindex="-1">
          Reset
        </.button>
        <.button
          id="save-autosave"
          phx-click="set_autosave_path"
          disabled={not Livebook.FileSystem.File.dir?(@state.file)}
          tabindex="-1"
        >
          Save
        </.button>
      </.live_component>
    </div>
    """
  end

  defp autosave_path_select(assigns) do
    ~H"""
    <div class="flex gap-2">
      <div class="grow">
        <.text_field name={nil} readonly value={@state.file.path} />
      </div>
      <.button color="gray" id="change-autosave" phx-click="open_autosave_path_select">
        Change
      </.button>
    </div>
    """
  end

  @impl true
  def handle_params(%{"env_var_id" => key}, _url, socket) do
    env_var = Livebook.Settings.fetch_env_var!(key)
    {:noreply, assign(socket, env_var: env_var)}
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
    {:noreply,
     update(
       socket,
       :autosave_path_state,
       &%{&1 | dialog_opened?: true, file: &1.file || default_autosave_dir()}
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
  def handle_info({:set_file, file, _info}, socket) do
    {:noreply, update(socket, :autosave_path_state, &%{&1 | file: file})}
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
