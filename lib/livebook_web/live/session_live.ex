defmodule LivebookWeb.SessionLive do
  use LivebookWeb, :live_view

  import LivebookWeb.SessionHelpers
  import LivebookWeb.FileSystemComponents

  alias Livebook.Session
  alias Livebook.Text
  alias Livebook.Notebook
  alias Livebook.Notebook.Cell

  on_mount LivebookWeb.SidebarHook

  @impl true
  def mount(%{"id" => session_id}, _session, socket) do
    # We use the tracked sessions to locate the session pid, but then
    # we talk to the session process exclusively for getting all the information
    case Livebook.Sessions.fetch_session(session_id) do
      {:ok, %{pid: session_pid}} ->
        {data, client_id} =
          if connected?(socket) do
            {data, client_id} =
              Session.register_client(session_pid, self(), socket.assigns.current_user)

            Session.subscribe(session_id)
            Livebook.NotebookManager.subscribe_starred_notebooks()

            {data, client_id}
          else
            data = Session.get_data(session_pid)
            {data, nil}
          end

        app =
          if slug = data.deployed_app_slug do
            {:ok, app} = Livebook.Apps.fetch_app(slug)

            if connected?(socket) do
              Livebook.App.subscribe(slug)
            end

            app
          end

        socket =
          if connected?(socket) do
            payload = %{
              client_id: client_id,
              clients:
                Enum.map(data.clients_map, fn {client_id, user_id} ->
                  client_info(client_id, data.users_map[user_id])
                end)
            }

            socket = push_event(socket, "session_init", payload)

            cells = for {cell, _} <- Notebook.cells_with_section(data.notebook), do: cell
            push_cell_editor_payloads(socket, data, cells)
          else
            socket
          end

        session = Session.get_by_pid(session_pid)
        platform = platform_from_socket(socket)

        socket =
          if data.mode == :app do
            put_flash(
              socket,
              :info,
              "This session is a deployed app. Any changes will be reflected live."
            )
          else
            socket
          end

        {:ok,
         socket
         |> assign(
           self_path: ~p"/sessions/#{session.id}",
           session: session,
           app: app,
           client_id: client_id,
           platform: platform,
           data_view: data_to_view(data),
           autofocus_cell_id: autofocus_cell_id(data.notebook),
           page_title: get_page_title(data.notebook.name),
           action_assigns: %{},
           allowed_uri_schemes: Livebook.Config.allowed_uri_schemes(),
           starred_files: Livebook.NotebookManager.starred_notebooks() |> starred_files()
         )
         |> assign_private(data: data)
         |> prune_outputs()
         |> prune_cell_sources()}

      {:error, :not_found} ->
        {:ok, redirect(socket, to: ~p"/")}

      {:error, :different_boot_id} ->
        {:ok,
         socket
         |> put_flash(
           :error,
           "Could not find notebook session because Livebook has rebooted. " <>
             "This may happen if Livebook runs out of memory while installing dependencies or executing code."
         )
         |> redirect(to: ~p"/")}
    end
  end

  # Puts the given assigns in `socket.private`,
  # to ensure they are not used for rendering.
  defp assign_private(socket, assigns) do
    Enum.reduce(assigns, socket, fn {key, value}, socket ->
      put_in(socket.private[key], value)
    end)
  end

  defp platform_from_socket(socket) do
    with user_agent when is_binary(user_agent) <- get_connect_info(socket, :user_agent) do
      platform_from_user_agent(user_agent)
    else
      _ -> nil
    end
  end

  @impl true
  defdelegate render(assigns), to: LivebookWeb.SessionLive.Render

  @impl true
  def handle_params(params, url, socket) do
    {socket, action_assigns} = handle_params(socket.assigns.live_action, params, url, socket)
    socket = assign(socket, :action_assigns, action_assigns)
    {:noreply, socket}
  end

  defp handle_params(:cell_settings, %{"cell_id" => cell_id}, _url, socket) do
    {:ok, cell, _} = Notebook.fetch_cell_and_section(socket.private.data.notebook, cell_id)
    {socket, %{cell: cell}}
  end

  defp handle_params(:insert_image, %{}, _url, socket) do
    case pop_in(socket.assigns[:insert_image_metadata]) do
      {nil, socket} -> {redirect_to_self(socket), %{}}
      {metadata, socket} -> {socket, %{insert_image_metadata: metadata}}
    end
  end

  defp handle_params(:insert_file, %{}, _url, socket) do
    case pop_in(socket.assigns[:insert_file_metadata]) do
      {nil, socket} -> {redirect_to_self(socket), %{}}
      {metadata, socket} -> {socket, %{insert_file_metadata: metadata}}
    end
  end

  defp handle_params(:rename_file_entry, %{"name" => name}, _url, socket) do
    if file_entry = find_file_entry(socket, name) do
      {socket, %{renaming_file_entry: file_entry}}
    else
      {redirect_to_self(socket), %{}}
    end
  end

  defp handle_params(:export, %{"tab" => tab}, _url, socket) do
    any_stale_cell? = any_stale_cell?(socket.private.data)
    {socket, %{tab: tab, any_stale_cell?: any_stale_cell?}}
  end

  defp handle_params(:add_file_entry, %{"tab" => tab}, _url, socket) do
    {file_drop_metadata, socket} = pop_in(socket.assigns[:file_drop_metadata])
    {socket, %{tab: tab, file_drop_metadata: file_drop_metadata}}
  end

  defp handle_params(:secrets, params, _url, socket) do
    {select_secret_metadata, socket} = pop_in(socket.assigns[:select_secret_metadata])

    {socket,
     %{select_secret_metadata: select_secret_metadata, prefill_secret_name: params["secret_name"]}}
  end

  defp handle_params(live_action, params, _url, socket)
       when live_action in [:app_settings, :file_settings] do
    {socket, %{context: params["context"]}}
  end

  defp handle_params(:add_agent, params, _url, socket) do
    hub = Livebook.Hubs.fetch_hub!(socket.private.data.notebook.hub_id)
    deployment_groups = Livebook.Hubs.Provider.deployment_groups(hub)
    deployment_group_id = params["deployment_group_id"]
    deployment_group = Enum.find(deployment_groups, &(&1.id == deployment_group_id))
    {socket, %{deployment_group: deployment_group}}
  end

  defp handle_params(:catch_all, %{"path_parts" => path_parts}, requested_url, socket) do
    path_parts =
      Enum.map(path_parts, fn
        "__parent__" -> ".."
        part -> part
      end)

    path = Path.join(path_parts)
    socket = handle_relative_path(socket, path, requested_url)
    {socket, %{}}
  end

  defp handle_params(_live_action, _params, _url, socket) do
    {socket, %{}}
  end

  @impl true
  def handle_event("append_section", %{}, socket) do
    idx = length(socket.private.data.notebook.sections)
    Session.insert_section(socket.assigns.session.pid, idx)

    {:noreply, socket}
  end

  def handle_event("insert_section_below", params, socket) do
    with {:ok, section, index} <-
           section_with_next_index(
             socket.private.data.notebook,
             params["section_id"],
             params["cell_id"]
           ) do
      Session.insert_section_into(socket.assigns.session.pid, section.id, index)
    end

    {:noreply, socket}
  end

  def handle_event("insert_branching_section_below", params, socket) do
    with {:ok, section, index} <-
           section_with_next_index(
             socket.private.data.notebook,
             params["section_id"],
             params["cell_id"]
           ) do
      Session.insert_branching_section_into(socket.assigns.session.pid, section.id, index)
    end

    {:noreply, socket}
  end

  def handle_event(
        "set_section_parent",
        %{"section_id" => section_id, "parent_id" => parent_id},
        socket
      ) do
    Session.set_section_parent(socket.assigns.session.pid, section_id, parent_id)

    {:noreply, socket}
  end

  def handle_event("unset_section_parent", %{"section_id" => section_id}, socket) do
    Session.unset_section_parent(socket.assigns.session.pid, section_id)

    {:noreply, socket}
  end

  def handle_event("enable_language", %{"language" => language}, socket) do
    language = language_to_string(language)
    Session.enable_language(socket.assigns.session.pid, language)
    {:noreply, socket}
  end

  def handle_event("disable_language", %{"language" => language}, socket) do
    language = language_to_string(language)
    Session.disable_language(socket.assigns.session.pid, language)
    {:noreply, socket}
  end

  def handle_event("insert_cell_below", params, socket) do
    {:noreply, insert_cell_below(socket, params)}
  end

  def handle_event("insert_example_snippet_below", params, socket) do
    data = socket.private.data
    %{"section_id" => section_id, "cell_id" => cell_id} = params

    socket =
      case socket.private.data.runtime_status do
        :disconnected ->
          reason = "To insert this block, you need a connected runtime."
          confirm_setup_runtime(socket, reason)

        :connecting ->
          message = "To insert this block, wait for the runtime to finish connecting."
          {:noreply, put_flash(socket, :info, message)}

        :connected ->
          case example_snippet_definition_by_name(data, params["definition_name"]) do
            {:ok, definition} ->
              variant = Enum.fetch!(definition.variants, params["variant_idx"])

              fun = fn socket ->
                with {:ok, section, index} <-
                       section_with_next_index(socket.private.data.notebook, section_id, cell_id) do
                  attrs = %{source: variant.source}
                  Session.insert_cell(socket.assigns.session.pid, section.id, index, :code, attrs)
                  {:ok, socket}
                end
              end

              ensure_packages_then(socket, variant.packages, definition.name, "block", fun)

            _ ->
              socket
          end
      end

    {:noreply, socket}
  end

  def handle_event("insert_smart_cell_below", params, socket) do
    data = socket.private.data
    %{"section_id" => section_id, "cell_id" => cell_id} = params

    case smart_cell_definition_by_kind(data, params["kind"]) do
      {:ok, definition} ->
        preset =
          if preset_idx = params["preset_idx"] do
            Enum.at(definition.requirement_presets, preset_idx)
          end

        packages = if(preset, do: preset.packages, else: [])

        socket =
          ensure_packages_then(socket, packages, definition.name, "smart cell", fn socket ->
            with {:ok, section, index} <-
                   section_with_next_index(socket.private.data.notebook, section_id, cell_id) do
              attrs = %{kind: definition.kind}
              Session.insert_cell(socket.assigns.session.pid, section.id, index, :smart, attrs)
              {:ok, socket}
            end
          end)

        {:noreply, socket}

      _ ->
        {:noreply, socket}
    end
  end

  def handle_event("set_default_language", %{"language" => language} = params, socket) do
    language = language_to_string(language)
    Session.set_notebook_attributes(socket.assigns.session.pid, %{default_language: language})
    {:noreply, insert_cell_below(socket, params)}
  end

  def handle_event("delete_cell", %{"cell_id" => cell_id}, socket) do
    on_confirm = fn socket ->
      Session.delete_cell(socket.assigns.session.pid, cell_id)
      socket
    end

    {:noreply,
     confirm(socket, on_confirm,
       title: "Delete cell",
       description: "Once you delete this cell, it will be moved to the bin.",
       confirm_text: "Delete",
       confirm_icon: "delete-bin-6-line",
       opt_out_id: "delete-cell"
     )}
  end

  def handle_event("set_notebook_name", %{"value" => name}, socket) do
    name = normalize_name(name)
    Session.set_notebook_name(socket.assigns.session.pid, name)

    {:noreply, socket}
  end

  def handle_event("set_section_name", %{"metadata" => section_id, "value" => name}, socket) do
    name = normalize_name(name)
    Session.set_section_name(socket.assigns.session.pid, section_id, name)

    {:noreply, socket}
  end

  def handle_event(
        "apply_cell_delta",
        %{
          "cell_id" => cell_id,
          "tag" => tag,
          "delta" => delta,
          "selection" => selection,
          "revision" => revision
        },
        socket
      ) do
    tag = String.to_atom(tag)
    delta = Text.Delta.from_compressed(delta)
    selection = selection && Text.Selection.from_compressed(selection)
    Session.apply_cell_delta(socket.assigns.session.pid, cell_id, tag, delta, selection, revision)

    {:noreply, socket}
  end

  def handle_event(
        "report_cell_selection",
        %{"cell_id" => cell_id, "tag" => tag, "selection" => selection, "revision" => revision},
        socket
      ) do
    selection = selection && Text.Selection.from_compressed(selection)
    tag = String.to_atom(tag)

    Phoenix.PubSub.broadcast_from(
      Livebook.PubSub,
      self(),
      "sessions:#{socket.assigns.session.id}",
      {:report_cell_selection, socket.assigns.client_id, cell_id, tag, selection, revision}
    )

    {:noreply, socket}
  end

  def handle_event(
        "report_cell_revision",
        %{"cell_id" => cell_id, "tag" => tag, "revision" => revision},
        socket
      ) do
    tag = String.to_atom(tag)
    Session.report_cell_revision(socket.assigns.session.pid, cell_id, tag, revision)

    {:noreply, socket}
  end

  def handle_event("move_cell", %{"cell_id" => cell_id, "offset" => offset}, socket) do
    offset = ensure_integer(offset)
    Session.move_cell(socket.assigns.session.pid, cell_id, offset)

    {:noreply, socket}
  end

  def handle_event("move_section", %{"section_id" => section_id, "offset" => offset}, socket) do
    offset = ensure_integer(offset)
    Session.move_section(socket.assigns.session.pid, section_id, offset)

    {:noreply, socket}
  end

  def handle_event("delete_section", %{"section_id" => section_id}, socket) do
    socket =
      case Notebook.fetch_section(socket.private.data.notebook, section_id) do
        {:ok, %{cells: []} = section} ->
          Session.delete_section(socket.assigns.session.pid, section.id, true)
          socket

        {:ok, section} ->
          section_id = section.id
          first_section? = hd(socket.private.data.notebook.sections).id == section.id

          on_confirm = fn socket, %{"delete_cells" => delete_cells} ->
            Livebook.Session.delete_section(socket.assigns.session.pid, section_id, delete_cells)
            socket
          end

          assigns = %{section_name: section.name}

          description = ~H"""
          Are you sure you want to delete this section - <span class="font-semibold">“{@section_name}”</span>?
          """

          confirm(socket, on_confirm,
            title: "Delete section",
            description: description,
            confirm_text: "Delete",
            confirm_icon: "delete-bin-6-line",
            options: [
              %{
                name: "delete_cells",
                label: "Delete all cells in this section",
                default: first_section?,
                disabled: first_section?
              }
            ]
          )

        :error ->
          socket
      end

    {:noreply, socket}
  end

  def handle_event("recover_smart_cell", %{"cell_id" => cell_id}, socket) do
    Session.recover_smart_cell(socket.assigns.session.pid, cell_id)

    {:noreply, socket}
  end

  def handle_event("convert_smart_cell", %{"cell_id" => cell_id}, socket) do
    on_confirm = fn socket ->
      Session.convert_smart_cell(socket.assigns.session.pid, cell_id)
      socket
    end

    {:noreply,
     confirm(socket, on_confirm,
       title: "Convert cell",
       description:
         "Once you convert this Smart cell to a Code cell, the Smart cell will be moved to the bin.",
       confirm_text: "Convert",
       confirm_icon: "arrow-up-down-line",
       opt_out_id: "convert-smart-cell"
     )}
  end

  def handle_event("queue_cell_evaluation", %{"cell_id" => cell_id} = params, socket) do
    opts =
      if params["disable_dependencies_cache"] do
        [disable_dependencies_cache: true]
      else
        []
      end

    Session.queue_cell_evaluation(socket.assigns.session.pid, cell_id, opts)

    {:noreply, socket}
  end

  @impl true
  def handle_event("queue_interrupted_cell_evaluation", %{"cell_id" => cell_id}, socket) do
    data = socket.private.data

    with {:ok, cell, _section} <- Notebook.fetch_cell_and_section(data.notebook, cell_id),
         true <- data.cell_infos[cell.id].eval.interrupted do
      Session.queue_cell_evaluation(socket.assigns.session.pid, cell_id)
    end

    {:noreply, socket}
  end

  def handle_event("queue_section_evaluation", %{"section_id" => section_id}, socket) do
    Session.queue_section_evaluation(socket.assigns.session.pid, section_id)

    {:noreply, socket}
  end

  def handle_event("queue_full_evaluation", %{"forced_cell_ids" => forced_cell_ids}, socket) do
    Session.queue_full_evaluation(socket.assigns.session.pid, forced_cell_ids)

    {:noreply, socket}
  end

  def handle_event("cancel_cell_evaluation", %{"cell_id" => cell_id}, socket) do
    Session.cancel_cell_evaluation(socket.assigns.session.pid, cell_id)

    {:noreply, socket}
  end

  def handle_event(
        "set_reevaluate_automatically",
        %{"value" => value, "cell_id" => cell_id},
        socket
      ) do
    Session.set_cell_attributes(socket.assigns.session.pid, cell_id, %{
      reevaluate_automatically: value
    })

    {:noreply, socket}
  end

  def handle_event("set_cell_language", %{"cell_id" => cell_id, "language" => language}, socket) do
    language = language_to_string(language)
    Session.set_cell_attributes(socket.assigns.session.pid, cell_id, %{language: language})
    {:noreply, socket}
  end

  def handle_event("save", %{}, socket) do
    if socket.private.data.file do
      Session.save(socket.assigns.session.pid)
      {:noreply, socket}
    else
      {:noreply, push_patch(socket, to: ~p"/sessions/#{socket.assigns.session.id}/settings/file")}
    end
  end

  def handle_event("connect_runtime", %{}, socket) do
    Session.connect_runtime(socket.assigns.session.pid)
    {:noreply, socket}
  end

  def handle_event("reconnect_runtime", %{}, socket) do
    Session.disconnect_runtime(socket.assigns.session.pid)
    Session.connect_runtime(socket.assigns.session.pid)
    {:noreply, socket}
  end

  def handle_event("disconnect_runtime", %{}, socket) do
    Session.disconnect_runtime(socket.assigns.session.pid)
    {:noreply, socket}
  end

  def handle_event("setup_runtime", %{"reason" => reason}, socket) do
    {:noreply, confirm_setup_runtime(socket, reason)}
  end

  def handle_event("runtime_disconnect_node", %{"node" => node}, socket) do
    node = Enum.find(socket.private.data.runtime_connected_nodes, &(Atom.to_string(&1) == node))

    if node do
      Livebook.Runtime.disconnect_node(socket.private.data.runtime, node)
    end

    {:noreply, socket}
  end

  def handle_event("deploy_app", _, socket) do
    data = socket.private.data
    app_settings = data.notebook.app_settings

    if Livebook.Notebook.AppSettings.valid?(app_settings) do
      {:noreply,
       LivebookWeb.SessionLive.AppSettingsComponent.preview_app(
         socket,
         app_settings,
         data.deployed_app_slug
       )}
    else
      {:noreply,
       push_patch(socket,
         to: ~p"/sessions/#{socket.assigns.session.id}/settings/app?context=preview"
       )}
    end
  end

  def handle_event("intellisense_request", %{"cell_id" => cell_id} = params, socket) do
    request =
      case params do
        %{"type" => "completion", "hint" => hint} ->
          {:completion, hint}

        %{"type" => "details", "line" => line, "column" => column} ->
          column = Text.JS.js_column_to_elixir(column, line)
          {:details, line, column}

        %{"type" => "signature", "hint" => hint} ->
          {:signature, hint}

        %{"type" => "format", "code" => code} ->
          {:format, code}
      end

    data = socket.private.data

    with {:ok, cell, _section} <- Notebook.fetch_cell_and_section(data.notebook, cell_id) do
      if data.runtime_status == :connected do
        parent_locators = Session.parent_locators_for_cell(data, cell)
        node = intellisense_node(cell)

        ref =
          Livebook.Runtime.handle_intellisense(
            data.runtime,
            self(),
            request,
            parent_locators,
            node
          )

        {:reply, %{"ref" => inspect(ref)}, socket}
      else
        reason =
          cond do
            params["type"] == "completion" and not params["editor_auto_completion"] ->
              "You need a connected runtime to enable code completion."

            params["type"] == "format" ->
              "You need a connected runtime to enable code formatting."

            true ->
              nil
          end

        socket = if reason, do: confirm_setup_runtime(socket, reason), else: socket

        {:reply, %{"ref" => nil}, socket}
      end
    else
      _ -> {:noreply, socket}
    end
  end

  def handle_event("fork_session", %{}, socket) do
    %{pid: pid, files_dir: files_dir} = socket.assigns.session
    # Fetch the data, as we don't keep cells' source in the state
    data = Session.get_data(pid)
    notebook = Notebook.forked(data.notebook)
    {:noreply, create_session(socket, notebook: notebook, files_source: {:dir, files_dir})}
  end

  def handle_event("close_session", %{}, socket) do
    {:noreply, confirm_close_session(socket, socket.assigns.session, redirect_to: ~p"/")}
  end

  def handle_event("star_notebook", %{}, socket) do
    data = socket.private.data
    Livebook.NotebookManager.add_starred_notebook(data.file, data.notebook.name)
    {:noreply, socket}
  end

  def handle_event("unstar_notebook", %{}, socket) do
    Livebook.NotebookManager.remove_starred_notebook(socket.private.data.file)
    {:noreply, socket}
  end

  def handle_event("erase_outputs", %{}, socket) do
    Session.erase_outputs(socket.assigns.session.pid)
    {:noreply, socket}
  end

  def handle_event("location_report", report, socket) do
    Phoenix.PubSub.broadcast_from(
      Livebook.PubSub,
      self(),
      "sessions:#{socket.assigns.session.id}",
      {:location_report, socket.assigns.client_id, report}
    )

    {:noreply, socket}
  end

  def handle_event(
        "select_secret",
        %{
          "js_view_ref" => select_secret_ref,
          "preselect_name" => preselect_name,
          "options" => select_secret_options
        },
        socket
      ) do
    socket =
      assign(socket,
        select_secret_metadata: %{
          ref: select_secret_ref,
          options: select_secret_options
        }
      )

    {:noreply,
     push_patch(socket,
       to: ~p"/sessions/#{socket.assigns.session.id}/secrets?secret_name=#{preselect_name}"
     )}
  end

  def handle_event("select_hub", %{"id" => id}, socket) do
    Session.set_notebook_hub(socket.assigns.session.pid, id)

    {:noreply, socket}
  end

  def handle_event("review_file_entry_access", %{"name" => name}, socket) do
    if file_entry = find_file_entry(socket, name) do
      on_confirm = fn socket ->
        Session.allow_file_entry(socket.assigns.session.pid, file_entry.name)
        socket
      end

      file_system_label =
        case Livebook.FileSystem.File.fetch_file_system(file_entry.file) do
          {:ok, file_system} -> file_system_label(file_system)
          _ -> "Not available"
        end

      assigns = %{
        name: file_entry.name,
        file: file_entry.file,
        file_system_label: file_system_label
      }

      description = ~H"""
      <div>
        File <span class="font-semibold">“{@name}“</span>
        points to an absolute path, do you want the notebook to access it?
      </div>
      <div class="mt-4 flex flex-col gap-2 border border-gray-200 rounded-lg p-4">
        <.labeled_text label="Path">{@file.path}</.labeled_text>
        <.labeled_text label="File system">{@file_system_label}</.labeled_text>
      </div>
      """

      {:noreply,
       confirm(socket, on_confirm,
         title: "Review access",
         description: description,
         confirm_text: "Allow access",
         confirm_icon: "shield-check-line",
         danger: false
       )}
    else
      {:noreply, socket}
    end
  end

  def handle_event("insert_image", params, socket) do
    {:noreply,
     socket
     |> assign(
       insert_image_metadata: %{section_id: params["section_id"], cell_id: params["cell_id"]}
     )
     |> push_patch(to: ~p"/sessions/#{socket.assigns.session.id}/insert-image")}
  end

  def handle_event(
        "insert_file",
        %{"section_id" => section_id, "cell_id" => cell_id, "file_entry_name" => file_entry_name},
        socket
      ) do
    if file_entry = find_file_entry(socket, file_entry_name) do
      case socket.private.data.runtime_status do
        :connected ->
          {:noreply,
           socket
           |> assign(
             insert_file_metadata: %{
               section_id: section_id,
               cell_id: cell_id,
               file_entry: file_entry,
               handlers: handlers_for_file_entry(file_entry, socket.private.data.runtime)
             }
           )
           |> push_patch(to: ~p"/sessions/#{socket.assigns.session.id}/insert-file")}

        :connecting ->
          message = "To see the available options, wait for the runtime to finish connecting."
          {:noreply, put_flash(socket, :info, message)}

        :disconnected ->
          reason = "To see the available options, you need a connected runtime."
          {:noreply, confirm_setup_runtime(socket, reason)}
      end
    else
      {:noreply, socket}
    end
  end

  def handle_event("insert_file_action", %{"idx" => idx}, socket) do
    %{section_id: section_id, cell_id: cell_id, file_entry: file_entry, handlers: handlers} =
      socket.assigns.action_assigns.insert_file_metadata

    handler = Enum.fetch!(handlers, idx)
    source = String.replace(handler.definition.source, "{{NAME}}", file_entry.name)

    socket =
      socket
      |> redirect_to_self()
      |> ensure_packages_then(
        handler.definition.packages,
        handler.definition.description,
        "action",
        fn socket ->
          with {:ok, section, index} <-
                 section_with_next_index(socket.private.data.notebook, section_id, cell_id) do
            attrs = %{source: source}

            Session.insert_cell(
              socket.assigns.session.pid,
              section.id,
              index,
              handler.cell_type,
              attrs
            )

            {:ok, socket}
          end
        end
      )

    {:noreply, socket}
  end

  def handle_event(
        "handle_file_drop",
        %{"section_id" => section_id, "cell_id" => cell_id},
        socket
      ) do
    case socket.private.data.runtime_status do
      :disconnected ->
        reason = "To see the available options, you need a connected runtime."
        {:noreply, confirm_setup_runtime(socket, reason)}

      :connecting ->
        message = "To see the available options, wait for the runtime to finish connecting."
        {:noreply, put_flash(socket, :info, message)}

      :connected ->
        {:noreply,
         socket
         |> assign(file_drop_metadata: %{section_id: section_id, cell_id: cell_id})
         |> push_patch(to: ~p"/sessions/#{socket.assigns.session.id}/add-file/upload")
         |> push_event("finish_file_drop", %{})}
    end
  end

  def handle_event("handle_file_drop", %{}, socket) do
    {:noreply,
     socket
     |> push_patch(to: ~p"/sessions/#{socket.assigns.session.id}/add-file/upload")
     |> push_event("finish_file_drop", %{})}
  end

  def handle_event("open_custom_view_settings", %{}, socket) do
    {:noreply,
     push_patch(socket, to: ~p"/sessions/#{socket.assigns.session.id}/settings/custom-view")}
  end

  @impl true
  def handle_call({:get_input_value, input_id}, _from, socket) do
    reply =
      case socket.private.data.input_infos do
        %{^input_id => %{value: value}} -> {:ok, socket.assigns.session.id, value}
        %{} -> :error
      end

    {:reply, reply, socket}
  end

  @impl true
  def handle_info({:operation, operation}, socket) do
    {:noreply, handle_operation(socket, operation)}
  end

  def handle_info({:error, error}, socket) when socket.assigns.live_action == :runtime_settings do
    # When the runtime settings modal is open we assume the error is
    # related to connecting the runtime and we show it dirrectly there

    message = error |> to_string() |> upcase_first()

    send_update(LivebookWeb.SessionLive.RuntimeComponent,
      id: "runtime-settings",
      event: {:error, message}
    )

    {:noreply, socket}
  end

  def handle_info({:error, error}, socket) do
    message = error |> to_string() |> upcase_first()
    socket = put_flash(socket, :error, message)
    {:noreply, socket}
  end

  def handle_info({:hydrate_bin_entries, hydrated_entries}, socket) do
    hydrated_entries_map = Map.new(hydrated_entries, fn entry -> {entry.cell.id, entry} end)

    data =
      Map.update!(socket.private.data, :bin_entries, fn bin_entries ->
        Enum.map(bin_entries, fn entry ->
          case Map.fetch(hydrated_entries_map, entry.cell.id) do
            {:ok, hydrated_entry} -> hydrated_entry
            :error -> entry
          end
        end)
      end)

    {:noreply,
     socket
     |> assign_private(data: data)
     |> assign(data_view: data_to_view(data))}
  end

  def handle_info({:hydrate_cell_source_digest, cell_id, tag, digest}, socket) do
    data = socket.private.data
    data = put_in(data.cell_infos[cell_id].sources[tag].digest, digest)
    # We don't recompute data_view, because for the cell indicator we
    # still compute the digest on the client side
    {:noreply, assign_private(socket, data: data)}
  end

  def handle_info({:session_updated, session}, socket) do
    {:noreply, assign(socket, :session, session)}
  end

  def handle_info(:session_closed, socket) do
    {:noreply,
     socket
     |> put_flash(:info, "Session has been closed")
     |> push_navigate(to: ~p"/")}
  end

  def handle_info({:runtime_intellisense_response, ref, request, response}, socket) do
    response = process_intellisense_response(response, request)
    payload = %{"ref" => inspect(ref), "response" => response}
    {:noreply, push_event(socket, "intellisense_response", payload)}
  end

  def handle_info({:location_report, client_id, report}, socket) do
    report = Map.put(report, :client_id, client_id)
    {:noreply, push_event(socket, "location_report", report)}
  end

  def handle_info({:report_cell_selection, client_id, cell_id, tag, selection, revision}, socket) do
    # Note that we transform the delta separately in each client's LV.
    # This way we avoid a race condition with session sending us a new
    # delta. We either acknowledge such delta and use it to transform
    # the incoming selection, or send the selection as is and only then
    # acknowledge the delta.

    case Notebook.fetch_cell_and_section(socket.private.data.notebook, cell_id) do
      {:ok, _cell, _section} ->
        selection =
          selection &&
            Session.Data.transform_selection(
              socket.private.data,
              cell_id,
              tag,
              selection,
              revision
            )

        payload = %{
          "client_id" => client_id,
          "selection" => selection && Text.Selection.to_compressed(selection)
        }

        {:noreply, push_event(socket, "cell_selection:#{cell_id}:#{tag}", payload)}

      _ ->
        {:noreply, socket}
    end
  end

  def handle_info({:set_input_values, values, local}, socket) do
    if local do
      socket =
        Enum.reduce(values, socket, fn {input_id, value}, socket ->
          operation = {:set_input_value, socket.assigns.client_id, input_id, value}
          handle_operation(socket, operation)
        end)

      {:noreply, socket}
    else
      for {input_id, value} <- values do
        Session.set_input_value(socket.assigns.session.pid, input_id, value)
      end

      {:noreply, socket}
    end
  end

  def handle_info({:queue_bound_cells_evaluation, input_id}, socket) do
    Session.queue_bound_cells_evaluation(socket.assigns.session.pid, input_id)

    {:noreply, socket}
  end

  def handle_info({:insert_image_complete, metadata, url}, socket) do
    params = %{
      "type" => "image",
      "section_id" => metadata.section_id,
      "cell_id" => metadata.cell_id,
      "url" => url
    }

    {:noreply, insert_cell_below(socket, params)}
  end

  def handle_info({:file_entry_uploaded, file_entry}, socket) do
    case socket.assigns.action_assigns[:file_drop_metadata] do
      %{section_id: section_id, cell_id: cell_id} ->
        {:noreply,
         socket
         |> assign(
           insert_file_metadata: %{
             section_id: section_id,
             cell_id: cell_id,
             file_entry: file_entry,
             handlers: handlers_for_file_entry(file_entry, socket.private.data.runtime)
           }
         )
         |> push_patch(to: ~p"/sessions/#{socket.assigns.session.id}/insert-file")}

      nil ->
        {:noreply, push_patch(socket, to: ~p"/sessions/#{socket.assigns.session.id}")}
    end
  end

  def handle_info({:put_flash, kind, message}, socket) do
    {:noreply, put_flash(socket, kind, message)}
  end

  def handle_info({:push_patch, to}, socket) do
    {:noreply, push_patch(socket, to: to)}
  end

  def handle_info({:starred_notebooks_updated, starred_notebooks}, socket) do
    {:noreply, assign(socket, starred_files: starred_files(starred_notebooks))}
  end

  def handle_info({:app_updated, app}, socket) when socket.assigns.app != nil do
    {:noreply, assign(socket, :app, app)}
  end

  def handle_info(_message, socket), do: {:noreply, socket}

  defp handle_relative_path(socket, path, requested_url) do
    cond do
      String.ends_with?(path, Livebook.LiveMarkdown.extension()) ->
        handle_relative_notebook_path(socket, path, requested_url)

      true ->
        socket
        |> put_flash(
          :error,
          "Got unrecognised session path: #{path}\nIf you want to link another notebook, make sure to include the .livemd extension"
        )
        |> redirect_to_self()
    end
  end

  defp handle_relative_notebook_path(socket, relative_path, requested_url) do
    resolution_location = location(socket.private.data)

    case resolution_location do
      nil ->
        socket
        |> put_flash(
          :info,
          "Cannot resolve notebook path #{relative_path}, because the current notebook has no location"
        )
        |> redirect_to_self()

      resolution_location ->
        origin = Notebook.ContentLoader.resolve_location(resolution_location, relative_path)
        fallback_locations = fallback_relative_locations(resolution_location, relative_path)

        case session_id_by_location(origin) do
          {:ok, session_id} ->
            redirect_path = session_path(session_id, url_hash: get_url_hash(requested_url))
            push_navigate(socket, to: redirect_path)

          {:error, :none} ->
            open_notebook(socket, origin, fallback_locations, requested_url)

          {:error, :many} ->
            origin_str =
              case origin do
                {:url, url} -> url
                {:file, file} -> file.path
              end

            socket
            |> put_flash(
              :error,
              "Cannot navigate, because multiple sessions were found for #{origin_str}"
            )
            |> redirect_to_self()
        end
    end
  end

  defp location(data)
  defp location(%{file: file}) when is_map(file), do: {:file, file}
  defp location(%{origin: origin}), do: origin

  defp get_url_hash(requested_url) do
    case String.split(requested_url, "#") do
      [_, url_hash] -> url_hash
      _ -> nil
    end
  end

  defp open_notebook(socket, origin, fallback_locations, requested_url) do
    case fetch_content_with_fallbacks(origin, fallback_locations) do
      {:ok, content} ->
        {notebook, %{warnings: messages}} = Livebook.LiveMarkdown.notebook_from_livemd(content)

        # If the current session has no file, fork the notebook
        fork? = socket.private.data.file == nil
        {file, notebook} = file_and_notebook(fork?, origin, notebook)
        url_hash = get_url_hash(requested_url)

        socket
        |> put_import_warnings(messages)
        |> create_session(
          notebook: notebook,
          origin: origin,
          file: file,
          url_hash: url_hash
        )

      {:error, message} ->
        socket
        |> put_flash(:error, "Cannot navigate, " <> message)
        |> redirect_to_self()
    end
  end

  def fallback_relative_locations({:file, _}, _relative_path), do: []

  def fallback_relative_locations(resolution_location, relative_path) do
    # Other locations to check in case the relative location doesn't
    # exist. For example, in ExDoc all pages (including notebooks) are
    # flat, regardless of how they are structured in the file system

    name = relative_path |> String.split("/") |> Enum.at(-1)
    flat_location = Notebook.ContentLoader.resolve_location(resolution_location, name)
    [flat_location]
  end

  defp fetch_content_with_fallbacks(location, fallbacks) do
    case Notebook.ContentLoader.fetch_content_from_location(location) do
      {:ok, content} ->
        {:ok, content}

      error ->
        fallbacks
        |> Enum.reject(&(&1 == location))
        |> Enum.find_value(error, fn fallback ->
          with {:error, _} <- Notebook.ContentLoader.fetch_content_from_location(fallback),
               do: nil
        end)
    end
  end

  defp file_and_notebook(fork?, origin, notebook)
  defp file_and_notebook(false, {:file, file}, notebook), do: {file, notebook}
  defp file_and_notebook(true, {:file, _file}, notebook), do: {nil, Notebook.forked(notebook)}
  defp file_and_notebook(_fork?, _origin, notebook), do: {nil, notebook}

  defp session_id_by_location(location) do
    sessions = Livebook.Sessions.list_sessions()

    session_with_file =
      Enum.find(sessions, fn session ->
        session.file && {:file, session.file} == location
      end)

    # A session associated with the given file takes
    # precedence over sessions originating from this file
    if session_with_file do
      {:ok, session_with_file.id}
    else
      sessions
      |> Enum.filter(fn session -> session.origin == location end)
      |> case do
        [session] -> {:ok, session.id}
        [] -> {:error, :none}
        _ -> {:error, :many}
      end
    end
  end

  defp redirect_to_self(socket) do
    push_patch(socket, to: ~p"/sessions/#{socket.assigns.session.id}")
  end

  defp handle_operation(socket, operation) do
    case Session.Data.apply_operation(socket.private.data, operation) do
      {:ok, data, actions} ->
        socket
        |> assign_private(data: data)
        |> assign(
          data_view:
            update_data_view(socket.assigns.data_view, socket.private.data, data, operation)
        )
        |> after_operation(socket, operation)
        |> handle_actions(actions)

      :error ->
        socket
    end
  end

  defp after_operation(socket, _prev_socket, {:client_join, client_id, user}) do
    push_event(socket, "client_joined", %{client: client_info(client_id, user)})
  end

  defp after_operation(socket, _prev_socket, {:client_leave, client_id}) do
    push_event(socket, "client_left", %{client_id: client_id})
  end

  defp after_operation(socket, _prev_socket, {:update_user, _client_id, user}) do
    updated_clients =
      socket.private.data.clients_map
      |> Enum.filter(fn {_client_id, user_id} -> user_id == user.id end)
      |> Enum.map(fn {client_id, _user_id} -> client_info(client_id, user) end)

    push_event(socket, "clients_updated", %{clients: updated_clients})
  end

  defp after_operation(socket, _prev_socket, {:set_notebook_name, _client_id, name}) do
    assign(socket, page_title: get_page_title(name))
  end

  defp after_operation(socket, _prev_socket, {:insert_section, client_id, _index, section_id}) do
    if client_id == socket.assigns.client_id do
      push_event(socket, "section_inserted", %{section_id: section_id})
    else
      socket
    end
  end

  defp after_operation(
         socket,
         _prev_socket,
         {:insert_section_into, client_id, _section_id, _index, section_id}
       ) do
    if client_id == socket.assigns.client_id do
      push_event(socket, "section_inserted", %{section_id: section_id})
    else
      socket
    end
  end

  defp after_operation(
         socket,
         _prev_socket,
         {:delete_section, _client_id, section_id, _delete_cells}
       ) do
    push_event(socket, "section_deleted", %{section_id: section_id})
  end

  defp after_operation(socket, _prev_socket, {:insert_cell, client_id, _, _, _, cell_id, _attrs}) do
    {:ok, cell, _section} = Notebook.fetch_cell_and_section(socket.private.data.notebook, cell_id)

    socket = push_cell_editor_payloads(socket, socket.private.data, [cell])

    socket = prune_cell_sources(socket)

    if client_id == socket.assigns.client_id do
      push_event(socket, "cell_inserted", %{cell_id: cell_id})
    else
      socket
    end
  end

  defp after_operation(socket, prev_socket, {:delete_cell, _client_id, cell_id}) do
    # Find a sibling cell that the client would focus if the deleted cell has focus.
    sibling_cell_id =
      case Notebook.fetch_cell_sibling(prev_socket.private.data.notebook, cell_id, 1) do
        {:ok, next_cell} ->
          next_cell.id

        :error ->
          case Notebook.fetch_cell_sibling(prev_socket.private.data.notebook, cell_id, -1) do
            {:ok, previous_cell} -> previous_cell.id
            :error -> nil
          end
      end

    push_event(socket, "cell_deleted", %{cell_id: cell_id, sibling_cell_id: sibling_cell_id})
  end

  defp after_operation(socket, _prev_socket, {:restore_cell, client_id, cell_id}) do
    {:ok, cell, _section} = Notebook.fetch_cell_and_section(socket.private.data.notebook, cell_id)

    socket = push_cell_editor_payloads(socket, socket.private.data, [cell])

    socket = prune_cell_sources(socket)

    if client_id == socket.assigns.client_id do
      push_event(socket, "cell_restored", %{cell_id: cell_id})
    else
      socket
    end
  end

  defp after_operation(socket, _prev_socket, {:move_cell, client_id, cell_id, _offset}) do
    if client_id == socket.assigns.client_id do
      push_event(socket, "cell_moved", %{cell_id: cell_id})
    else
      socket
    end
  end

  defp after_operation(socket, _prev_socket, {:move_section, client_id, section_id, _offset}) do
    if client_id == socket.assigns.client_id do
      push_event(socket, "section_moved", %{section_id: section_id})
    else
      socket
    end
  end

  defp after_operation(socket, _prev_socket, {:enable_language, client_id, language}) do
    cell = Notebook.get_extra_setup_cell(socket.private.data.notebook, language)

    socket = push_cell_editor_payloads(socket, socket.private.data, [cell])

    socket = prune_cell_sources(socket)

    if client_id == socket.assigns.client_id do
      push_event(socket, "cell_inserted", %{cell_id: cell.id})
    else
      socket
    end
  end

  defp after_operation(
         socket,
         _prev_socket,
         {:add_cell_evaluation_output, _client_id, _cell_id, _output}
       ) do
    prune_outputs(socket)
  end

  defp after_operation(
         socket,
         _prev_socket,
         {:add_cell_evaluation_response, _client_id, cell_id, _output, metadata}
       ) do
    socket
    |> prune_outputs()
    |> push_event("evaluation_finished:#{cell_id}", %{code_markers: metadata.code_markers})
  end

  defp after_operation(
         socket,
         _prev_socket,
         {:add_cell_doctest_report, _client_id, cell_id, doctest_report}
       ) do
    push_event(socket, "doctest_report:#{cell_id}", doctest_report_payload(doctest_report))
  end

  defp after_operation(
         socket,
         _prev_socket,
         {:smart_cell_started, _client_id, cell_id, _delta, _chunks, _js_view, _editor}
       ) do
    {:ok, cell, _section} = Notebook.fetch_cell_and_section(socket.private.data.notebook, cell_id)

    socket
    |> push_cell_editor_payloads(socket.private.data, [cell], [:secondary])
    |> prune_cell_sources()
  end

  defp after_operation(socket, _prev_socket, {:erase_outputs, _client_id}) do
    push_event(socket, "erase_outputs", %{})
  end

  defp after_operation(socket, prev_socket, {:set_deployed_app_slug, _client_id, slug}) do
    prev_slug = prev_socket.private.data.deployed_app_slug

    if slug == prev_slug do
      socket
    else
      if prev_slug do
        Livebook.App.unsubscribe(prev_slug)
      end

      app =
        if slug do
          {:ok, app} = Livebook.Apps.fetch_app(slug)
          Livebook.App.subscribe(slug)
          app
        end

      assign(socket, app: app)
    end
  end

  defp after_operation(socket, _prev_socket, _operation), do: socket

  defp handle_actions(socket, actions) do
    Enum.reduce(actions, socket, &handle_action(&2, &1))
  end

  defp handle_action(socket, {:report_delta, client_id, cell, tag, delta, selection}) do
    if client_id == socket.assigns.client_id do
      push_event(socket, "cell_acknowledgement:#{cell.id}:#{tag}", %{})
    else
      push_event(socket, "cell_delta:#{cell.id}:#{tag}", %{
        client_id: client_id,
        delta: Text.Delta.to_compressed(delta),
        selection: selection && Text.Selection.to_compressed(selection)
      })
    end
  end

  defp handle_action(socket, {:start_evaluation, cell, _section}) do
    push_event(socket, "start_evaluation:#{cell.id}", %{})
  end

  defp handle_action(socket, _action), do: socket

  defp client_info(id, user) do
    %{id: id, hex_color: user.hex_color, name: user.name || "Anonymous"}
  end

  defp normalize_name(name) do
    name
    |> String.trim()
    |> String.replace(~r/\s+/, " ")
    |> case do
      "" -> "Untitled"
      name -> name
    end
  end

  def upcase_first(string) do
    {head, tail} = String.split_at(string, 1)
    String.upcase(head) <> tail
  end

  defp insert_cell_below(socket, params) do
    {type, attrs} = cell_type_and_attrs_from_params(params, socket)

    with {:ok, section, index} <-
           section_with_next_index(
             socket.private.data.notebook,
             params["section_id"],
             params["cell_id"]
           ) do
      Session.insert_cell(socket.assigns.session.pid, section.id, index, type, attrs)
    end

    socket
  end

  defp cell_type_and_attrs_from_params(%{"type" => "markdown"}, _socket), do: {:markdown, %{}}

  defp cell_type_and_attrs_from_params(%{"type" => "code"} = params, socket) do
    language =
      if language = params["language"] do
        language_to_string(language)
      else
        socket.private.data.notebook.default_language
      end

    {:code, %{language: language}}
  end

  defp cell_type_and_attrs_from_params(%{"type" => "diagram"}, _socket) do
    source = """
    <!-- Learn more at https://mermaid-js.github.io/mermaid -->

    ```mermaid
    graph TD;
      A-->B;
      A-->C;
      B-->D;
      C-->D;
    ```\
    """

    {:markdown, %{source: source}}
  end

  defp cell_type_and_attrs_from_params(%{"type" => "image", "url" => url}, _socket) do
    source = "![](#{url})"

    {:markdown, %{source: source}}
  end

  defp section_with_next_index(notebook, section_id, cell_id)

  defp section_with_next_index(notebook, section_id, nil) do
    with {:ok, section} <- Notebook.fetch_section(notebook, section_id) do
      {:ok, section, 0}
    end
  end

  defp section_with_next_index(notebook, _section_id, cell_id) do
    with {:ok, cell, section} <- Notebook.fetch_cell_and_section(notebook, cell_id) do
      index = Enum.find_index(section.cells, &(&1 == cell))
      {:ok, section, index + 1}
    end
  end

  defp ensure_integer(n) when is_integer(n), do: n
  defp ensure_integer(n) when is_binary(n), do: String.to_integer(n)

  defp encode_digest(nil), do: nil
  defp encode_digest(digest), do: Base.encode64(digest)

  defp process_intellisense_response(
         %{range: %{from: from, to: to}} = response,
         {:details, line, _column}
       ) do
    %{
      response
      | range: %{
          from: Text.JS.elixir_column_to_js(from, line),
          to: Text.JS.elixir_column_to_js(to, line)
        }
    }
  end

  defp process_intellisense_response(%{code: code} = response, {:format, original_code}) do
    delta =
      if code do
        original_code
        |> Text.Delta.diff(code)
        |> Text.Delta.to_compressed()
      end

    response
    |> Map.delete(:code)
    |> Map.put(:delta, delta)
  end

  defp process_intellisense_response(response, _request), do: response

  defp autofocus_cell_id(%Notebook{sections: [%{cells: [%{id: id, source: ""}]}]}), do: id
  defp autofocus_cell_id(_notebook), do: nil

  defp confirm_setup_runtime(socket, reason) do
    on_confirm = fn socket ->
      Session.queue_cell_evaluation(socket.assigns.session.pid, Cell.main_setup_cell_id())
      socket
    end

    confirm(socket, on_confirm,
      title: "Setup runtime",
      description: "#{reason} Do you want to connect and setup the current one?",
      confirm_text: "Setup runtime",
      confirm_icon: "play-line",
      danger: false
    )
  end

  defp starred_files(starred_notebooks) do
    for info <- starred_notebooks, into: MapSet.new(), do: info.file
  end

  defp example_snippet_definition_by_name(data, name) do
    data.runtime
    |> Livebook.Runtime.snippet_definitions()
    |> Enum.find_value(:error, &(&1.type == :example && &1.name == name && {:ok, &1}))
  end

  defp smart_cell_definition_by_kind(data, kind) do
    Enum.find_value(data.smart_cell_definitions, :error, &(&1.kind == kind && {:ok, &1}))
  end

  defp ensure_packages_then(socket, packages, target_name, target_type, fun) do
    dependencies = Enum.map(packages, & &1.dependency)

    has_dependencies? =
      dependencies == [] or
        Livebook.Runtime.has_dependencies?(socket.private.data.runtime, dependencies)

    cond do
      has_dependencies? ->
        case fun.(socket) do
          {:ok, socket} -> socket
          :error -> socket
        end

      Livebook.Runtime.fixed_dependencies?(socket.private.data.runtime) ->
        put_flash(socket, :error, "This runtime doesn't support adding dependencies")

      true ->
        on_confirm = fn socket ->
          case fun.(socket) do
            {:ok, socket} -> add_dependencies_and_reevaluate(socket, dependencies)
            :error -> socket
          end
        end

        confirm_add_packages(socket, on_confirm, packages, target_name, target_type)
    end
  end

  defp add_dependencies_and_reevaluate(socket, dependencies) do
    Session.add_dependencies(socket.assigns.session.pid, dependencies)
    Session.queue_cell_evaluation(socket.assigns.session.pid, Cell.main_setup_cell_id())
    Session.queue_cells_reevaluation(socket.assigns.session.pid)
    socket
  end

  defp confirm_add_packages(socket, on_confirm, packages, target_name, target_type) do
    assigns = %{packages: packages, target_name: target_name, target_type: target_type}

    description = ~H"""
    The <span class="font-semibold">“{@target_name}“</span> {@target_type} requires the
    <.listing items={@packages}>
      <:item :let={package}><code>{package.name}</code></:item>
      <:singular_suffix>package. Do you want to add it as a dependency and restart?</:singular_suffix>
      <:plural_suffix>packages. Do you want to add them as dependencies and restart?</:plural_suffix>
    </.listing>
    """

    confirm(socket, on_confirm,
      title: "Add packages",
      description: description,
      confirm_text: "Add and restart",
      confirm_icon: "add-line",
      danger: false
    )
  end

  defp push_cell_editor_payloads(socket, data, cells, tags \\ :all) do
    for cell <- cells,
        {tag, payload} <- cell_editor_init_payloads(cell, data.cell_infos[cell.id]),
        tags == :all or tag in tags,
        reduce: socket do
      socket ->
        push_event(socket, "cell_editor_init:#{cell.id}:#{tag}", payload)
    end
  end

  defp cell_editor_init_payloads(%Cell.Code{} = cell, cell_info) do
    [
      primary: %{
        source: cell.source,
        revision: cell_info.sources.primary.revision,
        code_markers: cell_info.eval.code_markers,
        doctest_reports:
          for {_, doctest_report} <- cell_info.eval.doctest_reports do
            doctest_report_payload(doctest_report)
          end
      }
    ]
  end

  defp cell_editor_init_payloads(%Cell.Markdown{} = cell, cell_info) do
    [
      primary: %{
        source: cell.source,
        revision: cell_info.sources.primary.revision,
        code_markers: [],
        doctest_reports: []
      }
    ]
  end

  defp cell_editor_init_payloads(%Cell.Smart{} = cell, cell_info) do
    [
      primary: %{
        source: cell.source,
        revision: cell_info.sources.primary.revision,
        code_markers: cell_info.eval.code_markers,
        doctest_reports:
          for {_, doctest_report} <- cell_info.eval.doctest_reports do
            doctest_report_payload(doctest_report)
          end
      }
    ] ++
      if cell.editor && cell_info.status == :started do
        [
          secondary: %{
            source: cell.editor.source,
            revision: cell_info.sources.secondary.revision,
            code_markers: [],
            doctest_reports: []
          }
        ]
      else
        []
      end
  end

  defp doctest_report_payload(doctest_report) do
    Map.replace_lazy(doctest_report, :details, fn details ->
      details
      |> LivebookWeb.ANSIHelpers.ansi_string_to_html()
      |> Phoenix.HTML.safe_to_string()
    end)
  end

  defp find_file_entry(socket, name) do
    Enum.find(socket.private.data.notebook.file_entries, &(&1.name == name))
  end

  defp handlers_for_file_entry(file_entry, runtime) do
    handlers =
      for definition <- Livebook.Runtime.snippet_definitions(runtime),
          definition.type == :file_action,
          do: %{definition: definition, cell_type: :code}

    handlers =
      if file_entry.type == :attachment do
        [
          %{
            cell_type: :markdown,
            definition: %{
              type: :file_action,
              description: "Insert as Markdown image",
              source: "![](files/{{NAME}})",
              file_types: ["image/*"],
              packages: []
            }
          }
          | handlers
        ]
      else
        handlers
      end

    handlers
    |> Enum.filter(&matches_file_types?(file_entry.name, &1.definition.file_types))
    |> Enum.sort_by(& &1.definition.description)
  end

  defp matches_file_types?(_name, :any), do: true

  defp matches_file_types?(name, file_types) do
    mime_type = MIME.from_path(name)
    extension = Path.extname(name)

    Enum.any?(file_types, fn file_type ->
      case String.split(file_type, "/") do
        [group, "*"] ->
          String.starts_with?(mime_type, group <> "/")

        _ ->
          file_type == mime_type or file_type == extension
      end
    end)
  end

  defp language_to_string(language) do
    %{language: language} =
      Enum.find(Cell.Code.languages(), &(Atom.to_string(&1.language) == language))

    language
  end

  # Builds view-specific structure of data by cherry-picking
  # only the relevant attributes.
  # We then use `@data_view` in the templates and consequently
  # irrelevant changes to data don't change `@data_view`, so LV doesn't
  # have to traverse the whole template tree and no diff is sent to the client.
  defp data_to_view(data) do
    changed_input_ids = Session.Data.changed_input_ids(data)

    %{
      file: data.file,
      persist_outputs: data.notebook.persist_outputs,
      autosave_interval_s: data.notebook.autosave_interval_s,
      default_language: data.notebook.default_language,
      dirty: data.dirty,
      persistence_warnings: data.persistence_warnings,
      runtime: data.runtime,
      runtime_metadata: Livebook.Runtime.describe(data.runtime),
      runtime_status: data.runtime_status,
      runtime_connect_info: data.runtime_connect_info,
      runtime_connected_nodes: Enum.sort(data.runtime_connected_nodes),
      smart_cell_definitions: Enum.sort_by(data.smart_cell_definitions, & &1.name),
      example_snippet_definitions:
        data.runtime
        |> Livebook.Runtime.snippet_definitions()
        |> Enum.filter(&(&1.type == :example))
        |> Enum.sort_by(& &1.name),
      global_status: global_status(data),
      notebook_name: data.notebook.name,
      sections_items:
        for section <- data.notebook.sections do
          %{
            id: section.id,
            name: section.name,
            parent: parent_section_view(section.parent_id, data),
            status: cells_status(section.cells, data),
            identifier_definitions: cells_identifier_definitions(section.cells, data)
          }
        end,
      clients:
        data.clients_map
        |> Enum.map(fn {client_id, user_id} -> {client_id, data.users_map[user_id]} end)
        |> Enum.sort_by(fn {_client_id, user} -> user.name || "Anonymous" end),
      enabled_languages: Notebook.enabled_languages(data.notebook),
      installing?: data.cell_infos[Cell.main_setup_cell_id()].eval.status == :evaluating,
      setup_cell_views:
        Enum.map(data.notebook.setup_section.cells, &cell_to_view(&1, data, changed_input_ids)),
      section_views: section_views(data.notebook.sections, data, changed_input_ids),
      bin_entries: data.bin_entries,
      secrets: data.secrets,
      hub: Livebook.Hubs.fetch_hub!(data.notebook.hub_id),
      hub_secrets: data.hub_secrets,
      any_session_secrets?:
        Session.Data.session_secrets(data.secrets, data.notebook.hub_id) != [],
      file_entries: Enum.sort_by(data.notebook.file_entries, & &1.name),
      quarantine_file_entry_names: data.notebook.quarantine_file_entry_names,
      app_settings: data.notebook.app_settings,
      deployed_app_slug: data.deployed_app_slug,
      deployment_group_id: data.notebook.deployment_group_id
    }
  end

  defp cells_status(cells, data) do
    eval_infos =
      for cell <- cells,
          Cell.evaluable?(cell),
          info = data.cell_infos[cell.id].eval,
          do: Map.put(info, :id, cell.id)

    most_recent =
      eval_infos
      |> Enum.filter(& &1.evaluation_end)
      |> Enum.max_by(& &1.evaluation_end, DateTime, fn -> nil end)

    cond do
      evaluating = Enum.find(eval_infos, &(&1.status == :evaluating)) ->
        {:evaluating, evaluating.id}

      most_recent != nil and most_recent.errored ->
        {:errored, most_recent.id}

      stale = Enum.find(eval_infos, &(&1.validity == :stale)) ->
        {:stale, stale.id}

      most_recent != nil ->
        {:evaluated, most_recent.id}

      true ->
        {:fresh, nil}
    end
  end

  defp cells_identifier_definitions(cells, data) do
    for %Cell.Code{} = cell <- cells,
        Cell.evaluable?(cell),
        info = data.cell_infos[cell.id].eval,
        definition <- info.identifier_definitions,
        do: definition
  end

  defp global_status(data) do
    cells =
      data.notebook
      |> Notebook.evaluable_cells_with_section()
      |> Enum.map(fn {cell, _} -> cell end)

    cells_status(cells, data)
  end

  defp section_views(sections, data, changed_input_ids) do
    sections
    |> Enum.map(& &1.name)
    |> LivebookWeb.HTMLHelpers.names_to_html_ids()
    |> Enum.zip(sections)
    |> Enum.map(fn {html_id, section} ->
      %{
        id: section.id,
        html_id: html_id,
        name: section.name,
        parent: parent_section_view(section.parent_id, data),
        has_children?: Notebook.child_sections(data.notebook, section.id) != [],
        valid_parents:
          for parent <- Notebook.valid_parents_for(data.notebook, section.id) do
            %{id: parent.id, name: parent.name}
          end,
        cell_views: Enum.map(section.cells, &cell_to_view(&1, data, changed_input_ids))
      }
    end)
  end

  defp parent_section_view(nil, _data), do: nil

  defp parent_section_view(parent_id, data) do
    {:ok, section} = Notebook.fetch_section(data.notebook, parent_id)
    %{id: section.id, name: section.name}
  end

  defp cell_to_view(%Cell.Markdown{} = cell, _data, _changed_input_ids) do
    %{
      id: cell.id,
      type: :markdown,
      empty: cell.source == ""
    }
  end

  defp cell_to_view(%Cell.Code{} = cell, data, changed_input_ids) do
    info = data.cell_infos[cell.id]

    %{
      id: cell.id,
      type: :code,
      setup: Cell.setup?(cell),
      language: cell.language,
      empty: cell.source == "",
      eval: eval_info_to_view(cell, info.eval, data, changed_input_ids),
      reevaluate_automatically: cell.reevaluate_automatically
    }
  end

  defp cell_to_view(%Cell.Smart{} = cell, data, changed_input_ids) do
    info = data.cell_infos[cell.id]

    %{
      id: cell.id,
      type: :smart,
      empty: cell.source == "",
      eval: eval_info_to_view(cell, info.eval, data, changed_input_ids),
      status: info.status,
      js_view: cell.js_view,
      editor:
        cell.editor &&
          %{
            empty: cell.editor.source == "",
            language: cell.editor.language,
            placement: cell.editor.placement,
            visible: cell.editor.visible
          }
    }
  end

  defp eval_info_to_view(cell, eval_info, data, changed_input_ids) do
    %{
      outputs: cell.outputs,
      doctest_summary: doctest_summary(eval_info.doctest_reports),
      validity: eval_info.validity,
      status: eval_info.status,
      errored: eval_info.errored,
      reevaluates_automatically: eval_info.reevaluates_automatically,
      evaluation_time_ms: eval_info.evaluation_time_ms,
      evaluation_start: eval_info.evaluation_start,
      evaluation_digest: encode_digest(eval_info.evaluation_digest),
      outputs_batch_number: eval_info.outputs_batch_number,
      # Pass input values relevant to the given cell
      input_views: input_views_for_cell(cell, data, changed_input_ids)
    }
  end

  defp doctest_summary(doctests) do
    {doctests_count, failures_count} =
      Enum.reduce(doctests, {0, 0}, fn
        {_line, %{status: status}}, {total, failed} ->
          {total + 1, if(status == :failed, do: failed + 1, else: failed)}
      end)

    %{doctests_count: doctests_count, failures_count: failures_count}
  end

  defp input_views_for_cell(cell, data, changed_input_ids) do
    input_ids =
      for output <- cell.outputs,
          input <- Cell.find_inputs_in_output(output),
          do: input.id

    data.input_infos
    |> Map.take(input_ids)
    |> Map.new(fn {input_id, %{value: value}} ->
      {input_id, %{value: value, changed: MapSet.member?(changed_input_ids, input_id)}}
    end)
  end

  # Updates current data_view in response to an operation.
  # In most cases we simply recompute data_view, but for the
  # most common ones we only update the relevant parts.
  defp update_data_view(data_view, prev_data, data, operation) do
    case operation do
      {:report_cell_revision, _client_id, _cell_id, _tag, _revision} ->
        data_view

      {:apply_cell_delta, _client_id, _cell_id, _tag, _delta, _selection, _revision} ->
        update_dirty_status(data_view, data)

      {:update_smart_cell, _client_id, _cell_id, _cell_state, _delta, _chunks} ->
        update_dirty_status(data_view, data)

      {:add_cell_evaluation_output, _client_id, cell_id, output} ->
        case send_output_update(prev_data, data, cell_id, output) do
          :ok -> data_view
          :continue -> data_to_view(data)
        end

      {:add_cell_doctest_report, _client_id, _cell_id, _doctest_report} ->
        data_view

      _ ->
        data_to_view(data)
    end
  end

  # For outputs that update existing outputs we send the update directly
  # to the corresponding component, so the DOM patch is isolated and fast.
  # This is important for intensive output updates
  def send_output_update(prev_data, data, _cell_id, %{type: :frame_update} = output) do
    %{ref: ref, update: {update_type, new_outputs}} = output

    changed_input_ids = Session.Data.changed_input_ids(data)

    # Lookup in previous data to see if there is a chunked output,
    # and if so, send the chunk update directly to its component
    with :append <- update_type,
         [{{_idx, frame}, _cell} | _] <- Notebook.find_frame_outputs(prev_data.notebook, ref),
         %{outputs: [{idx, %{type: type, chunk: true}} | _]} <- frame do
      new_outputs
      |> Enum.reverse()
      |> Enum.take_while(&match?(%{type: ^type, chunk: true}, &1))
      |> Enum.each(fn output ->
        send_chunk_output_update(idx, output)
      end)
    end

    for {{idx, frame}, cell} <- Notebook.find_frame_outputs(data.notebook, ref) do
      # Note that we are not updating data_view to avoid re-render,
      # but any change that causes frame to re-render will update
      # data_view first
      input_views = input_views_for_cell(cell, data, changed_input_ids)

      send_update(LivebookWeb.Output.FrameComponent,
        id: "outputs-#{idx}-output",
        event: {:update, update_type, frame.outputs, input_views}
      )
    end

    :ok
  end

  def send_output_update(prev_data, _data, cell_id, %{type: type, chunk: true} = output)
      when type in [:terminal_text, :plain_text, :markdown] do
    # Lookup in previous data to see if the output is already there
    case Notebook.fetch_cell_and_section(prev_data.notebook, cell_id) do
      {:ok, %{outputs: [{idx, %{type: ^type, chunk: true}} | _]}, _section} ->
        send_chunk_output_update(idx, output)
        :ok

      _ ->
        :continue
    end
  end

  def send_output_update(_prev_data, _data, _cell_id, _output) do
    :continue
  end

  defp send_chunk_output_update(idx, output) do
    module =
      case output.type do
        :terminal_text -> LivebookWeb.Output.TerminalTextComponent
        :plain_text -> LivebookWeb.Output.PlainTextComponent
        :markdown -> LivebookWeb.Output.MarkdownComponent
      end

    send_update(module, id: "outputs-#{idx}-output", event: {:append, output})
  end

  defp prune_outputs(%{private: %{data: data}} = socket) do
    assign_private(
      socket,
      data: update_in(data.notebook, &Notebook.prune_cell_outputs/1)
    )
  end

  defp prune_cell_sources(%{private: %{data: data}} = socket) do
    assign_private(
      socket,
      data:
        update_in(
          data.notebook,
          &Notebook.update_cells(&1, fn
            %Notebook.Cell.Smart{} = cell ->
              %{cell | source: :__pruned__, attrs: :__pruned__}
              |> prune_smart_cell_editor_source()

            %{source: _} = cell ->
              %{cell | source: :__pruned__}

            cell ->
              cell
          end)
        )
    )
  end

  defp prune_smart_cell_editor_source(%{editor: %{source: _}} = cell),
    do: put_in(cell.editor.source, :__pruned__)

  defp prune_smart_cell_editor_source(cell), do: cell

  # Changes that affect only a single cell are still likely to
  # have impact on dirtiness, so we need to always mirror it
  defp update_dirty_status(data_view, data) do
    put_in(data_view.dirty, data.dirty)
  end

  defp get_page_title(notebook_name) do
    "#{notebook_name} - Livebook"
  end

  defp intellisense_node(%Cell.Smart{editor: %{intellisense_node: node_cookie}}), do: node_cookie
  defp intellisense_node(_), do: nil

  defp any_stale_cell?(data) do
    data.notebook
    |> Notebook.evaluable_cells_with_section()
    |> Enum.any?(fn {cell, _section} ->
      data.cell_infos[cell.id].eval.validity == :stale
    end)
  end
end
