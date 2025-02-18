defmodule LivebookWeb.SessionLive.Render do
  use LivebookWeb, :html

  import LivebookWeb.UserComponents
  import LivebookWeb.SessionHelpers
  import Livebook.Utils, only: [format_bytes: 1]

  alias Livebook.Notebook.Cell

  def render(assigns) do
    ~H"""
    <div
      class="flex grow h-full"
      id={"session-#{@session.id}"}
      data-el-session
      phx-hook="Session"
      data-p-global-status={hook_prop(elem(@data_view.global_status, 0))}
      data-p-autofocus-cell-id={hook_prop(@autofocus_cell_id)}
    >
      <.sidebar
        session={@session}
        live_action={@live_action}
        current_user={@current_user}
        runtime_connected_nodes={@data_view.runtime_connected_nodes}
      />
      <.side_panel app={@app} session={@session} data_view={@data_view} client_id={@client_id} />
      <div class="grow overflow-y-auto relative" data-el-notebook>
        <div data-el-js-view-iframes phx-update="ignore" id="js-view-iframes"></div>
        <.indicators
          session_id={@session.id}
          file={@data_view.file}
          dirty={@data_view.dirty}
          persistence_warnings={@data_view.persistence_warnings}
          autosave_interval_s={@data_view.autosave_interval_s}
          runtime_status={@data_view.runtime_status}
          global_status={@data_view.global_status}
        />
        <.notebook_content
          data_view={@data_view}
          session={@session}
          client_id={@client_id}
          allowed_uri_schemes={@allowed_uri_schemes}
          saved_hubs={@saved_hubs}
          starred_files={@starred_files}
        />
      </div>
    </div>

    <.current_user_modal current_user={@current_user} />

    <.modal
      :if={@live_action == :runtime_settings}
      id="runtime-settings-modal"
      show
      width="big"
      patch={@self_path}
    >
      <.live_component
        module={LivebookWeb.SessionLive.RuntimeComponent}
        id="runtime-settings"
        session={@session}
        return_to={@self_path}
        runtime={@data_view.runtime}
        runtime_status={@data_view.runtime_status}
        runtime_connect_info={@data_view.runtime_connect_info}
        hub={@data_view.hub}
        hub_secrets={@data_view.hub_secrets}
      />
    </.modal>

    <.modal
      :if={@live_action == :file_settings}
      id="persistence-modal"
      show
      width="big"
      patch={@self_path}
    >
      <.live_component
        module={LivebookWeb.SessionLive.PersistenceComponent}
        id="persistence"
        session={@session}
        file={@data_view.file}
        hub={@data_view.hub}
        context={@action_assigns.context}
        persist_outputs={@data_view.persist_outputs}
        autosave_interval_s={@data_view.autosave_interval_s}
      />
    </.modal>

    <.modal
      :if={@live_action == :app_settings}
      id="app-settings-modal"
      show
      width="medium"
      patch={@self_path}
    >
      <.live_component
        module={LivebookWeb.SessionLive.AppSettingsComponent}
        id="app-settings"
        session={@session}
        settings={@data_view.app_settings}
        context={@action_assigns.context}
        deployed_app_slug={@data_view.deployed_app_slug}
      />
    </.modal>

    <.modal
      :if={@live_action == :app_docker}
      id="app-docker-modal"
      show
      width="large"
      patch={@self_path}
    >
      <.live_component
        module={LivebookWeb.SessionLive.AppDockerComponent}
        id="app-docker"
        session={@session}
        hub={@data_view.hub}
        file={@data_view.file}
        app_settings={@data_view.app_settings}
        secrets={@data_view.secrets}
        file_entries={@data_view.file_entries}
        settings={@data_view.app_settings}
        deployment_group_id={@data_view.deployment_group_id}
      />
    </.modal>

    <.modal :if={@live_action == :app_teams} id="app-teams-modal" show width="big" patch={@self_path}>
      {live_render(@socket, LivebookWeb.SessionLive.AppTeamsLive,
        id: "app-teams",
        session: %{
          "session_pid" => @session.pid
        }
      )}
    </.modal>

    <.modal
      :if={@live_action == :app_teams_hub_info}
      id="app-teams-hub-info-modal"
      show
      width="big"
      patch={@self_path}
    >
      <.app_teams_hub_info_content
        any_team_hub?={Enum.any?(@saved_hubs, &(Livebook.Hubs.Provider.type(&1.provider) == "team"))}
        session={@session}
      />
    </.modal>

    <.modal
      :if={@live_action == :add_file_entry}
      id="add-file-entry-modal"
      show
      width="big"
      patch={@self_path}
    >
      <.add_file_entry_content
        session={@session}
        hub={@data_view.hub}
        file_entries={@data_view.file_entries}
        tab={@action_assigns.tab}
      />
    </.modal>

    <.modal
      :if={@live_action == :rename_file_entry}
      id="rename-file-entry-modal"
      show
      width="big"
      patch={@self_path}
    >
      <.live_component
        module={LivebookWeb.SessionLive.RenameFileEntryComponent}
        id="rename-file-entry"
        session={@session}
        file_entry={@action_assigns.renaming_file_entry}
      />
    </.modal>

    <.modal
      :if={@live_action == :shortcuts}
      id="shortcuts-modal"
      show
      width="large"
      patch={@self_path}
    >
      <.live_component
        module={LivebookWeb.SessionLive.ShortcutsComponent}
        id="shortcuts"
        platform={@platform}
      />
    </.modal>

    <.modal
      :if={@live_action == :cell_settings}
      id="cell-settings-modal"
      show
      width="medium"
      patch={@self_path}
    >
      <.live_component
        module={settings_component_for(@action_assigns.cell)}
        id="cell-settings"
        session={@session}
        return_to={@self_path}
        cell={@action_assigns.cell}
      />
    </.modal>

    <.modal
      :if={@live_action == :insert_image}
      id="insert-image-modal"
      show
      width="medium"
      patch={@self_path}
    >
      <.live_component
        module={LivebookWeb.SessionLive.InsertImageComponent}
        id="insert-image"
        session={@session}
        return_to={@self_path}
        insert_image_metadata={@action_assigns.insert_image_metadata}
      />
    </.modal>

    <.modal
      :if={@live_action == :insert_file}
      id="insert-file-modal"
      show
      width="medium"
      patch={@self_path}
    >
      <.live_component
        module={LivebookWeb.SessionLive.InsertFileComponent}
        id="insert-file"
        session={@session}
        return_to={@self_path}
        insert_file_metadata={@action_assigns.insert_file_metadata}
      />
    </.modal>

    <.modal :if={@live_action == :bin} id="bin-modal" show width="big" patch={@self_path}>
      <.live_component
        module={LivebookWeb.SessionLive.BinComponent}
        id="bin"
        session={@session}
        return_to={@self_path}
        bin_entries={@data_view.bin_entries}
      />
    </.modal>

    <.modal :if={@live_action == :export} id="export-modal" show width="big" patch={@self_path}>
      <.live_component
        module={LivebookWeb.SessionLive.ExportComponent}
        id="export"
        session={@session}
        tab={@action_assigns.tab}
        any_stale_cell?={@action_assigns.any_stale_cell?}
      />
    </.modal>

    <.modal
      :if={@live_action == :package_search}
      id="package-search-modal"
      show
      width="medium"
      patch={@self_path}
    >
      {live_render(@socket, LivebookWeb.SessionLive.PackageSearchLive,
        id: "package-search",
        session: %{
          "session_pid" => @session.pid,
          "runtime" => @data_view.runtime
        }
      )}
    </.modal>

    <.modal
      :if={@live_action == :secrets}
      id="secrets-modal"
      show
      width={if(@action_assigns.select_secret_metadata, do: "large", else: "medium")}
      patch={@self_path}
    >
      <.live_component
        module={LivebookWeb.SessionLive.SecretsComponent}
        id="secrets"
        session={@session}
        secrets={@data_view.secrets}
        hub_secrets={@data_view.hub_secrets}
        hub={@data_view.hub}
        select_secret_metadata={@action_assigns.select_secret_metadata}
        prefill_secret_name={@action_assigns.prefill_secret_name}
        return_to={@self_path}
      />
    </.modal>

    <.modal
      :if={@live_action == :custom_view_settings}
      id="custom-view-modal"
      show
      width="medium"
      patch={@self_path}
    >
      <.live_component
        module={LivebookWeb.SessionLive.CustomViewComponent}
        id="custom"
        return_to={@self_path}
        session={@session}
      />
    </.modal>
    """
  end

  defp settings_component_for(%Cell.Code{}),
    do: LivebookWeb.SessionLive.CodeCellSettingsComponent

  def sidebar(assigns) do
    ~H"""
    <nav
      class="w-16 flex flex-col items-center px-3 py-1 space-y-2 sm:space-y-3 sm:py-5 bg-gray-900"
      aria-label="sidebar"
      data-el-sidebar
    >
      <span>
        <.link navigate={~p"/"} aria-label="go to homepage">
          <img src={~p"/images/logo.png"} height="40" width="40" alt="" />
        </.link>
      </span>

      <%!-- Local functionality --%>

      <.button_item
        icon="node-tree"
        label="Outline (so)"
        button_attrs={["data-el-outline-toggle": true]}
      />

      <.button_item
        icon="group-fill"
        label="Connected users (su)"
        button_attrs={["data-el-clients-list-toggle": true]}
      />

      <div class="relative">
        <.button_item
          icon="cpu-line"
          label="Runtime settings (sr)"
          button_attrs={["data-el-runtime-info-toggle": true]}
        />
        <div
          :if={@runtime_connected_nodes != []}
          data-el-runtime-indicator
          class={[
            "absolute w-[12px] h-[12px] border-gray-900 border-2 rounded-full right-1.5 top-1.5 pointer-events-none",
            "bg-blue-500"
          ]}
        />
      </div>

      <%!-- Hub functionality --%>

      <.button_item
        icon="lock-password-line"
        label="Secrets (ss)"
        button_attrs={["data-el-secrets-list-toggle": true]}
      />

      <.button_item
        icon="folder-open-fill"
        label="Files (sf)"
        button_attrs={["data-el-files-list-toggle": true]}
      />

      <.button_item
        icon="rocket-line"
        label="App settings (sa)"
        button_attrs={["data-el-app-info-toggle": true]}
      />

      <div class="grow"></div>

      <.link_item
        icon="delete-bin-6-fill"
        label="Bin (sb)"
        path={~p"/sessions/#{@session.id}/bin"}
        active={@live_action == :bin}
        link_attrs={["data-btn-show-bin": true]}
      />

      <.link_item
        icon="keyboard-box-fill"
        label="Keyboard shortcuts (?)"
        path={~p"/sessions/#{@session.id}/shortcuts"}
        active={@live_action == :shortcuts}
        link_attrs={["data-btn-show-shortcuts": true]}
      />

      <span class="tooltip right distant" data-tooltip="User profile">
        <button
          class="text-gray-400 rounded-xl h-8 w-8 flex items-center justify-center mt-2 group"
          aria_label="user profile"
          phx-click={show_current_user_modal()}
        >
          <.user_avatar
            user={@current_user}
            class="w-8 h-8 group-hover:ring-white group-hover:ring-2"
            text_class="text-xs"
          />
        </button>
      </span>
    </nav>
    """
  end

  def side_panel(assigns) do
    ~H"""
    <div
      class="flex flex-col h-full w-full max-w-xs absolute z-30 top-0 left-[64px] overflow-y-auto shadow-xl md:static md:shadow-none bg-gray-50 border-r border-gray-100 px-6 pt-16 md:py-8"
      data-el-side-panel
    >
      <.outline_list data_view={@data_view} />
      <.clients_list data_view={@data_view} client_id={@client_id} />
      <.live_component
        module={LivebookWeb.SessionLive.FilesListComponent}
        id="files-list"
        session={@session}
        file_entries={@data_view.file_entries}
        quarantine_file_entry_names={@data_view.quarantine_file_entry_names}
      />
      <.live_component
        module={LivebookWeb.SessionLive.SecretsListComponent}
        id="secrets-list"
        session={@session}
        secrets={@data_view.secrets}
        hub_secrets={@data_view.hub_secrets}
        hub={@data_view.hub}
      />
      <.live_component
        module={LivebookWeb.SessionLive.AppInfoComponent}
        id="app-info"
        session={@session}
        settings={@data_view.app_settings}
        app={@app}
        deployed_app_slug={@data_view.deployed_app_slug}
        any_session_secrets?={@data_view.any_session_secrets?}
        hub={@data_view.hub}
      />
      <.runtime_info data_view={@data_view} session={@session} />
    </div>
    """
  end

  defp button_item(assigns) do
    ~H"""
    <span class="tooltip right distant" data-tooltip={@label}>
      <button
        class="text-2xl text-gray-400 hover:text-gray-50 focus:text-gray-50 rounded-xl h-10 w-10 flex items-center justify-center"
        aria-label={@label}
        {@button_attrs}
      >
        <.remix_icon icon={@icon} />
      </button>
    </span>
    """
  end

  defp link_item(assigns) do
    assigns = assign_new(assigns, :link_attrs, fn -> [] end)

    ~H"""
    <span class="tooltip right distant" data-tooltip={@label}>
      <.link
        patch={@path}
        class={[
          "text-gray-400 hover:text-gray-50 focus:text-gray-50 rounded-xl h-10 w-10 flex items-center justify-center",
          @active && "text-gray-50 bg-gray-700"
        ]}
        aria-label={@label}
        {@link_attrs}
      >
        <.remix_icon icon={@icon} class="text-2xl" />
      </.link>
    </span>
    """
  end

  defp outline_list(assigns) do
    ~H"""
    <div class="flex flex-col grow" data-el-outline>
      <h3 class="uppercase text-sm font-semibold text-gray-500">
        Outline
      </h3>
      <div class="flex flex-col mt-4 space-y-4">
        <div :for={section_item <- @data_view.sections_items} class="flex flex-col">
          <div class="flex justify-between items-center">
            <button
              class="grow flex items-baseline gap-1 text-gray-600 hover:text-gray-900 text-left"
              data-el-outline-item
              data-section-id={section_item.id}
            >
              <.remix_icon icon="h-2" class="text-lg font-normal leading-none opacity-50" />
              <span>{section_item.name}</span>
              <%!--
                Note: the container has overflow-y auto, so we cannot set overflow-x visible,
                consequently we show the tooltip wrapped to a fixed number of characters
                --%>
              <span
                :if={section_item.parent}
                {branching_tooltip_attrs(section_item.name, section_item.parent.name)}
              >
                <.remix_icon
                  icon="git-branch-line"
                  class="text-lg font-normal leading-none flip-horizontally"
                />
              </span>
            </button>

            <.section_status
              status={elem(section_item.status, 0)}
              cell_id={elem(section_item.status, 1)}
            />
          </div>

          <ul :if={section_item.identifier_definitions != []} class="mt-2 ml-5 list-none items-center">
            <li :for={definition <- section_item.identifier_definitions}>
              <button
                class="flex items-baseline max-w-full text-gray-600 hover:text-gray-900 text-sm gap-1"
                data-el-outline-definition-item
                data-file={definition.file}
                data-line={definition.line}
              >
                <.remix_icon icon="braces-line" class="font-normal opacity-50" />
                <span class="font-mono break-all text-left">
                  {definition.label}
                </span>
              </button>
            </li>
          </ul>
        </div>
      </div>
      <button
        class="inline-flex items-center justify-center p-8 py-1 mt-8 space-x-2 text-sm font-medium text-gray-500 border border-gray-400 border-dashed rounded-xl hover:bg-gray-100"
        phx-click="append_section"
      >
        <.remix_icon icon="add-line" class="text-lg align-center" />
        <span>New section</span>
      </button>
      <div class="grow"></div>
      <button
        class="inline-flex items-center justify-center p-8 py-1 mt-8 space-x-2 text-sm font-medium text-gray-500 border border-gray-400 border-dashed rounded-xl hover:bg-gray-100"
        data-el-section-toggle-collapse-all-button
      >
        <.remix_icon icon="split-cells-vertical" class="text-lg align-center" />
        <span>Expand/collapse all</span>
      </button>
    </div>
    """
  end

  defp branching_tooltip_attrs(name, parent_name) do
    direction = if String.length(name) >= 16, do: "left", else: "right"

    wrapped_name = Livebook.Utils.wrap_line("”" <> parent_name <> "”", 16)
    label = "Branches from\n#{wrapped_name}"

    [class: "tooltip #{direction}", "data-tooltip": label]
  end

  defp clients_list(assigns) do
    ~H"""
    <div class="flex flex-col grow" data-el-clients-list>
      <div class="flex items-center justify-between space-x-4 -mt-1">
        <h3 class="uppercase text-sm font-semibold text-gray-500">
          Users
        </h3>
        <span class="flex items-center px-2 py-1 space-x-2 text-sm bg-gray-200 rounded-lg">
          <span class="inline-flex w-3 h-3 bg-green-600 rounded-full"></span>
          <span>{length(@data_view.clients)} connected</span>
        </span>
      </div>
      <div class="flex flex-col mt-5 space-y-4">
        <div
          :for={{client_id, user} <- @data_view.clients}
          class="flex items-center justify-between space-x-2"
          id={"clients-list-item-#{client_id}"}
          data-el-clients-list-item
          data-client-id={client_id}
        >
          <button
            class="flex items-center space-x-2 text-gray-500 hover:text-gray-900 disabled:pointer-events-none"
            disabled={client_id == @client_id}
            data-el-client-link
          >
            <.user_avatar user={user} class="shrink-0 h-7 w-7" text_class="text-xs" />
            <span class="text-left">
              {user.name || "Anonymous"}
              {if(client_id == @client_id, do: "(you)")}
            </span>
          </button>
          <%= if client_id == @client_id do %>
            <.icon_button aria-label="edit profile" phx-click={show_current_user_modal()}>
              <.remix_icon icon="user-settings-line" />
            </.icon_button>
          <% else %>
            <span
              class="tooltip left"
              data-tooltip="Follow this user"
              data-el-client-follow-toggle
              data-meta="follow"
            >
              <.icon_button aria-label="follow this user">
                <.remix_icon icon="pushpin-line" />
              </.icon_button>
            </span>
            <span
              class="tooltip left"
              data-tooltip="Unfollow this user"
              data-el-client-follow-toggle
              data-meta="unfollow"
            >
              <.icon_button aria-label="unfollow this user">
                <.remix_icon icon="pushpin-fill" />
              </.icon_button>
            </span>
          <% end %>
        </div>
      </div>
    </div>
    """
  end

  defp runtime_info(assigns) do
    ~H"""
    <div class="flex flex-col grow" data-el-runtime-info>
      <div class="flex items-center justify-between">
        <h3 class="uppercase text-sm font-semibold text-gray-500">
          Runtime
        </h3>
        <span
          class="tooltip bottom-left"
          data-tooltip={
            ~S'''
            The runtime configures which Erlang VM
            instance the notebook code runs on.
            '''
          }
        >
          <.icon_button>
            <.remix_icon icon="question-line" />
          </.icon_button>
        </span>
      </div>
      <div class="flex flex-col mt-2">
        <div class="flex flex-col space-y-3">
          <.labeled_text :for={{label, value} <- @data_view.runtime_metadata} label={label} one_line>
            {value}
          </.labeled_text>
        </div>
        <div class="mt-4 grid grid-cols-2 gap-2">
          <.button :if={@data_view.runtime_status == :disconnected} phx-click="connect_runtime">
            <.remix_icon icon="wireless-charging-line" />
            <span>Connect</span>
          </.button>
          <.button :if={@data_view.runtime_status == :connecting} disabled>
            <.remix_icon icon="wireless-charging-line" />
            <span>Connecting...</span>
          </.button>
          <.button :if={@data_view.runtime_status == :connected} phx-click="reconnect_runtime">
            <.remix_icon icon="wireless-charging-line" />
            <span>Reconnect</span>
          </.button>
          <.button color="gray" outlined patch={~p"/sessions/#{@session.id}/settings/runtime"}>
            Configure
          </.button>

          <.button
            :if={@data_view.runtime_status in [:connected, :connecting]}
            color="red"
            outlined
            type="button"
            phx-click="disconnect_runtime"
            class="col-span-2"
          >
            Disconnect
          </.button>
        </div>

        <div :if={@data_view.runtime_connect_info} class="mt-4">
          <.message_box kind="info">
            <div class="flex items-center gap-2">
              <.spinner />
              <span>Step: {@data_view.runtime_connect_info}</span>
            </div>
          </.message_box>
        </div>

        <.memory_usage_info
          memory_usage={@session.memory_usage}
          runtime_metadata={@data_view.runtime_metadata}
        />

        <.runtime_connected_nodes_info runtime_connected_nodes={@data_view.runtime_connected_nodes} />
      </div>
    </div>
    """
  end

  defp memory_usage_info(assigns) do
    ~H"""
    <div class="mt-8 flex flex-col gap-2">
      <div class="flex items-center justify-between">
        <div class="text-sm text-gray-500 font-semibold uppercase">
          Memory
        </div>
        <span class="tooltip left" data-tooltip="See on dashboard">
          <.icon_button
            :if={node = runtime_node(@runtime_metadata)}
            href={LivebookWeb.HTMLHelpers.live_dashboard_node_path(node)}
            target="_blank"
            aria-label="see on dashboard"
          >
            <.remix_icon icon="dashboard-2-line" />
          </.icon_button>
        </span>
      </div>
      <%= if uses_memory?(@memory_usage) do %>
        <.runtime_memory_info memory_usage={@memory_usage} />
      <% else %>
        <p class="text-sm text-gray-800">
          {format_bytes(@memory_usage.system.free)} available out of {format_bytes(
            @memory_usage.system.total
          )}
        </p>
      <% end %>
    </div>
    """
  end

  defp runtime_node(runtime_metadata) do
    Enum.find_value(runtime_metadata, fn {key, value} -> key == "Node name" && value end)
  end

  defp runtime_memory_info(assigns) do
    assigns = assign(assigns, :runtime_memory, runtime_memory(assigns.memory_usage))

    ~H"""
    <div class="flex flex-col gap-2">
      <div class="w-full flex flex-row gap-0.5">
        <div
          :for={{type, memory} <- @runtime_memory}
          class={["h-6", memory_color(type)]}
          style={"width: #{memory.percentage}%"}
        >
        </div>
      </div>
      <div class="flex flex-col">
        <div :for={{type, memory} <- @runtime_memory} class="flex flex-row items-center">
          <span class={["w-4 h-4 mr-2 rounded", memory_color(type)]}></span>
          <span class="capitalize text-gray-700">{type}</span>
          <span class="text-gray-500 ml-auto">{memory.unit}</span>
        </div>
      </div>
      <div class="flex rounded justify-center py-0.5 text-sm text-gray-800 bg-gray-200">
        Total: {format_bytes(@memory_usage.runtime.total)}
      </div>
    </div>
    """
  end

  defp memory_color(:atom), do: "bg-blue-500"
  defp memory_color(:code), do: "bg-yellow-600"
  defp memory_color(:processes), do: "bg-blue-700"
  defp memory_color(:binary), do: "bg-green-500"
  defp memory_color(:ets), do: "bg-red-500"
  defp memory_color(:other), do: "bg-gray-400"

  defp runtime_memory(%{runtime: memory}) do
    memory
    |> Map.drop([:total, :system])
    |> Enum.map(fn {type, bytes} ->
      {type,
       %{
         unit: format_bytes(bytes),
         percentage: Float.round(bytes / memory.total * 100, 2),
         value: bytes
       }}
    end)
  end

  defp runtime_connected_nodes_info(assigns) do
    ~H"""
    <div class="mt-8 flex flex-col gap-2">
      <span class="text-sm text-gray-500 font-semibold uppercase">
        Connected nodes
        <%= if @runtime_connected_nodes != [] do %>
          ({length(@runtime_connected_nodes)})
        <% end %>
      </span>
      <%= if @runtime_connected_nodes == [] do %>
        <div class="text-sm text-gray-800 flex flex-col">
          No connected nodes
        </div>
      <% else %>
        <div class="flex flex-col">
          <div
            :for={node <- @runtime_connected_nodes}
            class="flex flex-nowrap items-baseline py-1 pl-2 -ml-2 pr-1 hover:bg-gray-100 group rounded-lg"
          >
            <.remix_icon icon="circle-fill" class="mr-2 text-xs text-blue-500" />
            <div class={[
              "flex-grow text-sm text-gray-700 text-medium whitespace-nowrap text-ellipsis overflow-hidden",
              "group-hover:overflow-visible group-hover:whitespace-normal group-hover:break-all"
            ]}>
              {node}
            </div>
            <span class="tooltip left" data-tooltip="Disconnect">
              <.icon_button phx-click="runtime_disconnect_node" phx-value-node={node} small>
                <.remix_icon icon="close-line" />
              </.icon_button>
            </span>
          </div>
        </div>
      <% end %>
    </div>
    """
  end

  defp section_status(%{status: :evaluating} = assigns) do
    ~H"""
    <button data-el-focus-cell-button data-target={@cell_id}>
      <.status_indicator variant="progressing" />
    </button>
    """
  end

  defp section_status(%{status: :stale} = assigns) do
    ~H"""
    <button data-el-focus-cell-button data-target={@cell_id}>
      <.status_indicator variant="warning" />
    </button>
    """
  end

  defp section_status(assigns), do: ~H""

  def session_menu(assigns) do
    ~H"""
    <.menu id="session-menu">
      <:toggle>
        <.icon_button aria-label="open notebook menu">
          <.remix_icon icon="more-2-fill" />
        </.icon_button>
      </:toggle>
      <.menu_item>
        <.link patch={~p"/sessions/#{@session.id}/export/livemd"} role="menuitem">
          <.remix_icon icon="download-2-line" />
          <span>Export</span>
        </.link>
      </.menu_item>
      <.menu_item>
        <button role="menuitem" phx-click="erase_outputs">
          <.remix_icon icon="eraser-fill" />
          <span>Erase outputs</span>
        </button>
      </.menu_item>
      <.menu_item>
        <button role="menuitem" phx-click="fork_session">
          <.remix_icon icon="git-branch-line" />
          <span>Fork</span>
        </button>
      </.menu_item>
      <.menu_item>
        <a
          role="menuitem"
          href={LivebookWeb.HTMLHelpers.live_dashboard_process_path(@session.pid)}
          target="_blank"
        >
          <.remix_icon icon="dashboard-2-line" />
          <span>See on Dashboard</span>
        </a>
      </.menu_item>
      <.menu_item variant="danger">
        <button role="menuitem" phx-click="close_session">
          <.remix_icon icon="close-circle-line" />
          <span>Close</span>
        </button>
      </.menu_item>
    </.menu>
    """
  end

  defp app_teams_hub_info_content(assigns) do
    ~H"""
    <div class="flex flex-col space-y-4">
      <h3 class="text-2xl font-semibold text-gray-800">
        App deployment with Livebook Teams
      </h3>

      <%= if @any_team_hub? do %>
        <.message_box kind="info">
          In order to deploy your app using Livebook Teams, you need to select a Livebook Teams
          workspace. To change the workspace, use the dropdown right below the notebook title.
          <.link
            class="text-blue-600 font-medium"
            patch={~p"/sessions/#{@session.id}"}
            phx-click={show_menu(%JS{}, "notebook-hub-menu", animate: true)}
          >
            <span>Change workspace</span>
            <.remix_icon icon="arrow-right-line" />
          </.link>
        </.message_box>
      <% else %>
        <.message_box kind="info">
          In order to deploy your app using Livebook Teams, you need to create an organization.
          <.link class="text-blue-600 font-medium" patch={~p"/hub"}>
            <span>Add organization</span>
            <.remix_icon icon="arrow-right-line" />
          </.link>
        </.message_box>
      <% end %>
    </div>
    """
  end

  def add_file_entry_content(assigns) do
    ~H"""
    <div class="flex flex-col space-y-4">
      <h3 class="text-2xl font-semibold text-gray-800">
        Add file
      </h3>
      <div class="flex flex-col space-y-4">
        <div class="tabs">
          <.link
            patch={~p"/sessions/#{@session.id}/add-file/storage"}
            class={["tab", @tab == "storage" && "active"]}
          >
            <.remix_icon icon="file-3-line" class="align-middle" />
            <span class="font-medium">From storage</span>
          </.link>
          <.link
            patch={~p"/sessions/#{@session.id}/add-file/url"}
            class={["tab", @tab == "url" && "active"]}
          >
            <.remix_icon icon="download-cloud-2-line" class="align-middle" />
            <span class="font-medium">From URL</span>
          </.link>
          <.link
            patch={~p"/sessions/#{@session.id}/add-file/upload"}
            class={["tab", @tab == "upload" && "active"]}
          >
            <.remix_icon icon="file-upload-line" class="align-middle" />
            <span class="font-medium">From upload</span>
          </.link>
          <.link
            patch={~p"/sessions/#{@session.id}/add-file/unlisted"}
            class={["tab", @tab == "unlisted" && "active"]}
          >
            <.remix_icon icon="folder-shared-line" class="align-middle" />
            <span class="font-medium">From unlisted</span>
          </.link>
          <div class="grow tab"></div>
        </div>
        <.live_component
          :if={@tab == "storage"}
          module={LivebookWeb.SessionLive.AddFileEntryFileComponent}
          id="add-file-entry-from-file"
          hub={@hub}
          session={@session}
        />
        <.live_component
          :if={@tab == "url"}
          module={LivebookWeb.SessionLive.AddFileEntryUrlComponent}
          id="add-file-entry-from-url"
          hub={@hub}
          session={@session}
        />
        <.live_component
          :if={@tab == "upload"}
          module={LivebookWeb.SessionLive.AddFileEntryUploadComponent}
          id="add-file-entry-from-upload"
          hub={@hub}
          session={@session}
        />
        <.live_component
          :if={@tab == "unlisted"}
          module={LivebookWeb.SessionLive.AddFileEntryUnlistedComponent}
          id="add-file-entry-from-unlisted"
          hub={@hub}
          session={@session}
          file_entries={@file_entries}
        />
      </div>
    </div>
    """
  end

  def indicators(assigns) do
    ~H"""
    <div class="flex items-center justify-between sticky px-2 top-0 left-0 right-0 z-[500] bg-white border-b border-gray-200">
      <div class="sm:hidden text-2xl text-gray-400 hover:text-gray-600 focus:text-gray-600 rounded-xl h-10 w-10 flex items-center justify-center">
        <button
          aria-label="hide sidebar"
          data-el-toggle-sidebar
          phx-click={
            JS.add_class("hidden sm:flex", to: "[data-el-sidebar]")
            |> JS.toggle(to: "[data-el-toggle-sidebar]", display: "flex")
          }
        >
          <.remix_icon icon="menu-fold-line" />
        </button>

        <button
          class="hidden"
          aria-label="show sidebar"
          data-el-toggle-sidebar
          phx-click={
            JS.remove_class("hidden sm:flex", to: "[data-el-sidebar]")
            |> JS.toggle(to: "[data-el-toggle-sidebar]", display: "flex")
          }
        >
          <.remix_icon icon="menu-unfold-line" />
        </button>
      </div>
      <div class="sm:fixed bottom-[0.4rem] right-[1.5rem]">
        <div
          class="flex flex-row-reverse sm:flex-col items-center justify-end p-2 sm:p-0 space-x-2 space-x-reverse sm:space-x-0 sm:space-y-2"
          data-el-notebook-indicators
        >
          <.view_indicator />
          <.persistence_indicator
            file={@file}
            dirty={@dirty}
            persistence_warnings={@persistence_warnings}
            autosave_interval_s={@autosave_interval_s}
            session_id={@session_id}
          />
          <.runtime_indicator
            runtime_status={@runtime_status}
            global_status={@global_status}
            session_id={@session_id}
          />
          <.insert_mode_indicator />
        </div>
      </div>
    </div>
    """
  end

  defp view_indicator(assigns) do
    ~H"""
    <div class="tooltip left" data-tooltip="Choose views to activate" data-el-views>
      <.menu id="views-menu" position="bottom-right" sm_position="top-right">
        <:toggle>
          <button
            class={status_button_classes(:gray)}
            aria-label="choose views to activate"
            data-el-views-disabled
          >
            <.remix_icon icon="layout-5-line" />
          </button>
          <button
            class={status_button_classes(:green)}
            aria-label="choose views to activate"
            data-el-views-enabled
          >
            <.remix_icon icon="layout-5-line" />
          </button>
        </:toggle>
        <.menu_item>
          <button role="menuitem" data-el-view-toggle="code-zen">
            <.remix_icon icon="code-line" />
            <span>Code zen</span>
          </button>
        </.menu_item>
        <.menu_item>
          <button role="menuitem" data-el-view-toggle="presentation">
            <.remix_icon icon="slideshow-2-line" />
            <span>Presentation</span>
          </button>
        </.menu_item>
        <.menu_item>
          <button role="menuitem" data-el-view-toggle="custom">
            <.remix_icon icon="settings-5-line" />
            <span>Custom</span>
          </button>
        </.menu_item>
      </.menu>
    </div>
    """
  end

  defp persistence_indicator(%{file: nil} = assigns) do
    ~H"""
    <span class="tooltip left" data-tooltip="Choose a file to save the notebook">
      <.link
        patch={~p"/sessions/#{@session_id}/settings/file"}
        class={status_button_classes(:gray)}
        aria-label="choose a file to save the notebook"
      >
        <.remix_icon icon="save-line" />
      </.link>
    </span>
    """
  end

  defp persistence_indicator(%{dirty: false} = assigns) do
    ~H"""
    <span
      class="tooltip left"
      data-tooltip={
        case @persistence_warnings do
          [] ->
            "Notebook saved"

          warnings ->
            "Notebook saved with warnings:\n" <> Enum.map_join(warnings, "\n", &("- " <> &1))
        end
      }
    >
      <.link
        patch={~p"/sessions/#{@session_id}/settings/file"}
        class={status_button_classes(:green)}
        aria-label="notebook saved, click to open file settings"
      >
        <div class="relative">
          <.remix_icon icon="save-line" />
          <.remix_icon
            :if={@persistence_warnings != []}
            icon="error-warning-fill"
            class="text-lg text-red-400 absolute -top-1.5 -right-2"
          />
        </div>
      </.link>
    </span>
    """
  end

  defp persistence_indicator(%{autosave_interval_s: nil} = assigns) do
    ~H"""
    <span class="tooltip left" data-tooltip="No autosave configured, make sure to save manually">
      <.link
        patch={~p"/sessions/#{@session_id}/settings/file"}
        class={status_button_classes(:yellow)}
        aria-label="no autosave configured, click to open file settings"
      >
        <.remix_icon icon="save-line" />
      </.link>
    </span>
    """
  end

  defp persistence_indicator(assigns) do
    ~H"""
    <span class="tooltip left" data-tooltip="Autosave pending">
      <.link
        patch={~p"/sessions/#{@session_id}/settings/file"}
        class={status_button_classes(:blue)}
        aria-label="autosave pending, click to open file settings"
      >
        <.remix_icon icon="save-line" />
      </.link>
    </span>
    """
  end

  defp runtime_indicator(assigns) do
    ~H"""
    <%= if @runtime_status == :disconnected do %>
      <span class="tooltip left" data-tooltip="Choose a runtime to run the notebook in">
        <.link
          patch={~p"/sessions/#{@session_id}/settings/runtime"}
          class={status_button_classes(:gray)}
          aria-label="choose a runtime to run the notebook in"
        >
          <.remix_icon icon="loader-3-line" />
        </.link>
      </span>
    <% else %>
      <.global_status status={elem(@global_status, 0)} cell_id={elem(@global_status, 1)} />
    <% end %>
    """
  end

  defp global_status(%{status: :evaluating} = assigns) do
    ~H"""
    <span class="tooltip left" data-tooltip="Go to evaluating cell">
      <button
        class={status_button_classes(:blue)}
        aria-label="go to evaluating cell"
        data-el-focus-cell-button
        data-target={@cell_id}
      >
        <.remix_icon icon="loader-3-line" class="animate-spin" />
      </button>
    </span>
    """
  end

  defp global_status(%{status: :evaluated} = assigns) do
    ~H"""
    <span class="tooltip left" data-tooltip="Go to last evaluated cell">
      <button
        class={status_button_classes(:green)}
        aria-label="go to last evaluated cell"
        data-el-focus-cell-button
        data-target={@cell_id}
      >
        <.remix_icon icon="loader-3-line" />
      </button>
    </span>
    """
  end

  defp global_status(%{status: :errored} = assigns) do
    ~H"""
    <span class="tooltip left" data-tooltip="Go to last evaluated cell">
      <button
        class={status_button_classes(:red)}
        aria-label="go to last evaluated cell"
        data-el-focus-cell-button
        data-target={@cell_id}
      >
        <.remix_icon icon="loader-3-line" />
      </button>
    </span>
    """
  end

  defp global_status(%{status: :stale} = assigns) do
    ~H"""
    <span class="tooltip left" data-tooltip="Go to first stale cell">
      <button
        class={status_button_classes(:yellow)}
        aria-label="go to first stale cell"
        data-el-focus-cell-button
        data-target={@cell_id}
      >
        <.remix_icon icon="loader-3-line" />
      </button>
    </span>
    """
  end

  defp global_status(%{status: :fresh} = assigns) do
    ~H"""
    <span class="tooltip left" data-tooltip="Ready to evaluate">
      <div class={status_button_classes(:gray)} aria-label="ready to evaluate">
        <.remix_icon icon="loader-3-line" />
      </div>
    </span>
    """
  end

  defp status_button_classes(color) do
    [
      "text-xl leading-none p-1 flex items-center justify-center rounded-full rounded-full border-2 focus-visible:outline-none",
      case color do
        :gray ->
          "text-gray-400 border-gray-200 hover:bg-gray-100 focus-visible:bg-gray-100"

        :blue ->
          "text-blue-500 border-blue-400 hover:bg-blue-50 focus-visible:bg-blue-50"

        :green ->
          "text-green-bright-400 border-green-bright-300 hover:bg-green-bright-50 focus-visible:bg-green-bright-50"

        :yellow ->
          "text-yellow-bright-300 border-yellow-bright-200 hover:bg-yellow-bright-50 focus-visible:bg-yellow-bright-50"

        :red ->
          "text-red-400 border-red-300 hover:bg-red-50 focus-visible:bg-red-50"
      end
    ]
  end

  defp insert_mode_indicator(assigns) do
    ~H"""
    <%!-- Note: this indicator is shown/hidden using CSS based on the current mode --%>
    <span class="tooltip left" data-tooltip="Insert mode" data-el-insert-mode-indicator>
      <span class="text-sm font-medium text-gray-400 cursor-default">
        ins
      </span>
    </span>
    """
  end

  def notebook_content(assigns) do
    ~H"""
    <div
      class="relative w-full max-w-screen-lg px-4 sm:pl-8 sm:pr-16 md:pl-16 pt-4 sm:py-5 mx-auto"
      data-el-notebook-content
    >
      <div class="pb-4 mb-2 border-b border-gray-200">
        <div class="flex flex-nowrap items-center gap-2">
          <div
            class="grow"
            data-el-notebook-headline
            data-focusable-id="notebook"
            id="notebook"
            phx-hook="Headline"
            data-p-id={hook_prop("notebook")}
            data-p-on-value-change={hook_prop("set_notebook_name")}
            data-p-metadata={hook_prop("notebook")}
          >
            <h1
              class="px-1 -ml-1.5 text-3xl font-semibold text-gray-800 border border-transparent rounded-lg whitespace-pre-wrap"
              tabindex="0"
              id="notebook-heading"
              data-el-heading
              spellcheck="false"
              phx-no-format
            >{@data_view.notebook_name}</h1>
          </div>
          <.session_menu session={@session} />
        </div>
        <div class="flex flex-nowrap place-content-between items-center gap-2">
          <.menu position="bottom-left" id="notebook-hub-menu">
            <:toggle>
              <div
                class="inline-flex items-center cursor-pointer gap-1 mt-1 text-sm text-gray-600 hover:text-gray-800 focus:text-gray-800"
                aria-label={@data_view.hub.hub_name}
              >
                <span>Using</span>
                <span class="pl-1">{@data_view.hub.hub_emoji}</span>
                <span>{@data_view.hub.hub_name}</span>
                <.remix_icon icon="arrow-down-s-line" class="-ml-1" />
                <span>workspace</span>
              </div>
            </:toggle>
            <.menu_item :for={hub <- @saved_hubs}>
              <button
                id={"select-hub-#{hub.id}"}
                phx-click={JS.push("select_hub", value: %{id: hub.id})}
                aria-label={hub.name}
                role="menuitem"
              >
                {hub.emoji}
                <span class="ml-2">{hub.name}</span>
              </button>
            </.menu_item>
            <.menu_item>
              <.link navigate={~p"/hub"} aria-label="Add Organization" role="menuitem">
                <.remix_icon icon="add-line" class="align-middle mr-1" /> Add Organization
              </.link>
            </.menu_item>
          </.menu>
          <div class="px-[1px]">
            <.star_button file={@data_view.file} starred_files={@starred_files} />
          </div>
        </div>
      </div>
      <div data-el-setup-section>
        <div class="flex flex-col gap-2">
          <.live_component
            :for={setup_cell_view <- @data_view.setup_cell_views}
            module={LivebookWeb.SessionLive.CellComponent}
            id={setup_cell_view.id}
            session_id={@session.id}
            session_pid={@session.pid}
            client_id={@client_id}
            runtime={@data_view.runtime}
            installing?={@data_view.installing?}
            allowed_uri_schemes={@allowed_uri_schemes}
            enabled_languages={@data_view.enabled_languages}
            cell_view={setup_cell_view}
          />
        </div>
        <div
          :if={:python not in @data_view.enabled_languages}
          class="flex mt-2"
          data-el-language-buttons
        >
          <.button
            color="gray"
            outlined
            small
            phx-click="enable_language"
            phx-value-language="python"
            disabled={Livebook.Runtime.fixed_dependencies?(@data_view.runtime)}
          >
            <.remix_icon icon="add-line" class="text-sm -mx-0.5 leading-none" />
            <span>Python</span>
          </.button>
        </div>
      </div>
      <div class="mt-8 flex flex-col w-full space-y-16" data-el-sections-container>
        <div :if={@data_view.section_views == []} class="flex justify-center">
          <LivebookWeb.SessionLive.InsertButtonsComponent.insert_button phx-click="append_section">
            + Section
          </LivebookWeb.SessionLive.InsertButtonsComponent.insert_button>
        </div>
        <.live_component
          :for={{section_view, index} <- Enum.with_index(@data_view.section_views)}
          module={LivebookWeb.SessionLive.SectionComponent}
          id={section_view.id}
          index={index}
          session_id={@session.id}
          session_pid={@session.pid}
          client_id={@client_id}
          runtime_status={@data_view.runtime_status}
          smart_cell_definitions={@data_view.smart_cell_definitions}
          example_snippet_definitions={@data_view.example_snippet_definitions}
          installing?={@data_view.installing?}
          allowed_uri_schemes={@allowed_uri_schemes}
          section_view={section_view}
          default_language={@data_view.default_language}
          enabled_languages={@data_view.enabled_languages}
        />
        <div style="height: 80vh"></div>
      </div>
    </div>
    """
  end

  defp star_button(%{file: nil} = assigns) do
    ~H"""
    <span class="tooltip left" data-tooltip="Save this notebook before starring it">
      <.icon_button disabled>
        <.remix_icon icon="star-line" />
      </.icon_button>
    </span>
    """
  end

  defp star_button(assigns) do
    ~H"""
    <%= if starred?(@file, @starred_files) do %>
      <span class="tooltip left" data-tooltip="Unstar notebook">
        <.icon_button phx-click="unstar_notebook">
          <.remix_icon icon="star-fill" class="text-yellow-600" />
        </.icon_button>
      </span>
    <% else %>
      <span class="tooltip left" data-tooltip="Star notebook">
        <.icon_button phx-click="star_notebook">
          <.remix_icon icon="star-line" />
        </.icon_button>
      </span>
    <% end %>
    """
  end

  defp starred?(file, starred_files) do
    Enum.any?(starred_files, &Livebook.FileSystem.File.equal?(&1, file))
  end
end
