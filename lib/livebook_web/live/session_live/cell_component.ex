defmodule LivebookWeb.SessionLive.CellComponent do
  use LivebookWeb, :live_component

  import LivebookWeb.NotebookComponents

  @impl true
  def mount(socket) do
    {:ok, stream(socket, :outputs, [])}
  end

  @impl true
  def update(assigns, socket) do
    socket = assign(socket, assigns)

    socket =
      case assigns.cell_view do
        %{eval: %{outputs: outputs}} ->
          stream_items =
            for {idx, output} <- Enum.reverse(outputs) do
              %{id: Integer.to_string(idx), idx: idx, output: output}
            end

          stream(socket, :outputs, stream_items)

        %{} ->
          socket
      end

    {:ok, socket}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div
      class="flex flex-col relative scroll-mt-[50px] sm:scroll-mt-0"
      data-el-cell
      id={"cell-#{@cell_view.id}"}
      data-type={@cell_view.type}
      data-setup={@cell_view[:setup]}
      data-focusable-id={@cell_view.id}
      data-js-empty={@cell_view.empty}
      data-eval-validity={get_in(@cell_view, [:eval, :validity])}
      data-eval-errored={get_in(@cell_view, [:eval, :errored])}
      phx-hook="Cell"
      data-p-cell-id={hook_prop(@cell_view.id)}
      data-p-type={hook_prop(@cell_view.type)}
      data-p-session-path={hook_prop(~p"/sessions/#{@session_id}")}
      data-p-evaluation-digest={hook_prop(get_in(@cell_view, [:eval, :evaluation_digest]))}
      data-p-smart-cell-js-view-ref={hook_prop(smart_cell_js_view_ref(@cell_view))}
      data-p-allowed-uri-schemes={hook_prop(@allowed_uri_schemes)}
    >
      {render_cell(assigns)}
    </div>
    """
  end

  defp render_cell(%{cell_view: %{type: :markdown}} = assigns) do
    ~H"""
    <.cell_actions>
      <:secondary>
        <.enable_insert_mode_button />
        <.cell_link_button cell_id={@cell_view.id} />
        <.move_cell_up_button cell_id={@cell_view.id} />
        <.move_cell_down_button cell_id={@cell_view.id} />
        <.delete_cell_button cell_id={@cell_view.id} />
      </:secondary>
    </.cell_actions>
    <.cell_body>
      <div class="pb-4" data-el-editor-box>
        <.cell_editor
          cell_id={@cell_view.id}
          tag="primary"
          empty={@cell_view.empty}
          language="markdown"
        />
      </div>
      <div
        class="markdown break-words"
        data-el-markdown-container
        id={"markdown-container-#{@cell_view.id}"}
        phx-update="ignore"
      >
        <.content_skeleton empty={@cell_view.empty} />
      </div>
    </.cell_body>
    """
  end

  defp render_cell(%{cell_view: %{type: :code, setup: true, language: :elixir}} = assigns) do
    ~H"""
    <.cell_actions>
      <:primary>
        <.setup_cell_evaluation_button
          cell_id={@cell_view.id}
          validity={@cell_view.eval.validity}
          status={@cell_view.eval.status}
          runtime={@runtime}
        />
      </:primary>
      <:secondary>
        <.package_search_button session_id={@session_id} runtime={@runtime} />
        <.cell_link_button cell_id={@cell_view.id} />
        <.setup_cell_info />
      </:secondary>
    </.cell_actions>
    <.cell_body>
      <div class="relative" data-el-cell-body-root>
        <div data-el-info-box>
          <div class="py-2 px-3 flex items-center justify-between border border-gray-200 text-sm text-gray-400 rounded-lg">
            <span class="font-medium">Notebook dependencies and setup</span>
          </div>
        </div>
        <div data-el-editor-box>
          <.cell_editor
            cell_id={@cell_view.id}
            tag="primary"
            empty={@cell_view.empty}
            language="elixir"
            intellisense
          />
        </div>
        <div class="absolute bottom-2 right-2" data-el-cell-indicators>
          <.cell_indicators id={@cell_view.id} cell_view={@cell_view} />
        </div>
      </div>
      <.evaluation_outputs
        outputs={@streams.outputs}
        cell_view={@cell_view}
        session_id={@session_id}
        session_pid={@session_pid}
        client_id={@client_id}
      />
    </.cell_body>
    """
  end

  defp render_cell(
         %{cell_view: %{type: :code, setup: true, language: :"pyproject.toml"}} = assigns
       ) do
    ~H"""
    <.cell_actions>
      <:primary>
        <div class="flex gap-1 items-center text-gray-500 text-sm">
          <.language_icon language="python" class="w-4 h-4" />
          <span>Python (pyproject.toml)</span>
        </div>
      </:primary>
      <:secondary>
        <.cell_link_button cell_id={@cell_view.id} />
        <.disable_language_button language={:python} />
        <.pyproject_toml_cell_info />
      </:secondary>
    </.cell_actions>
    <.cell_body>
      <div class="relative" data-el-cell-body-root>
        <div data-el-editor-box>
          <.cell_editor
            cell_id={@cell_view.id}
            tag="primary"
            empty={@cell_view.empty}
            language="pyproject.toml"
          />
        </div>
        <div class="absolute bottom-2 right-2" data-el-cell-indicators>
          <.cell_indicators id={@cell_view.id} cell_view={@cell_view} />
        </div>
      </div>
      <.evaluation_outputs
        outputs={@streams.outputs}
        cell_view={@cell_view}
        session_id={@session_id}
        session_pid={@session_pid}
        client_id={@client_id}
      />
    </.cell_body>
    """
  end

  defp render_cell(%{cell_view: %{type: :code}} = assigns) do
    ~H"""
    <.cell_actions>
      <:primary>
        <.cell_evaluation_button
          session_id={@session_id}
          cell_id={@cell_view.id}
          validity={@cell_view.eval.validity}
          status={@cell_view.eval.status}
          reevaluate_automatically={@cell_view.reevaluate_automatically}
          reevaluates_automatically={@cell_view.eval.reevaluates_automatically}
        />
      </:primary>
      <:secondary>
        <.cell_settings_button cell_id={@cell_view.id} session_id={@session_id} />
        <.amplify_output_button />
        <.cell_link_button cell_id={@cell_view.id} />
        <.move_cell_up_button cell_id={@cell_view.id} />
        <.move_cell_down_button cell_id={@cell_view.id} />
        <.delete_cell_button cell_id={@cell_view.id} />
      </:secondary>
    </.cell_actions>
    <.cell_body>
      <div class="relative" data-el-cell-body-root>
        <div class="relative" data-el-editor-box>
          <.cell_editor
            cell_id={@cell_view.id}
            tag="primary"
            empty={@cell_view.empty}
            language={@cell_view.language}
            intellisense={@cell_view.language == :elixir}
          />
        </div>
        <div class="absolute bottom-2 right-2" data-el-cell-indicators>
          <.cell_indicators id={@cell_view.id} cell_view={@cell_view} langauge_toggle />
        </div>
      </div>
      <div :if={@cell_view.language not in @enabled_languages} class="mt-2">
        <.message_box kind="error">
          <div class="flex items-center justify-between">
            {language_name(@cell_view.language)} is not enabled for the current notebook.
            <button
              class="flex gap-1 items-center font-medium text-blue-600"
              phx-click="enable_language"
              phx-value-language="python"
            >
              Enable Python
            </button>
          </div>
        </.message_box>
      </div>
      <.doctest_summary cell_id={@cell_view.id} doctest_summary={@cell_view.eval.doctest_summary} />
      <.evaluation_outputs
        outputs={@streams.outputs}
        cell_view={@cell_view}
        session_id={@session_id}
        session_pid={@session_pid}
        client_id={@client_id}
      />
    </.cell_body>
    """
  end

  defp render_cell(%{cell_view: %{type: :smart}} = assigns) do
    ~H"""
    <.cell_actions>
      <:primary>
        <.cell_evaluation_button
          session_id={@session_id}
          cell_id={@cell_view.id}
          validity={@cell_view.eval.validity}
          status={@cell_view.eval.status}
          reevaluate_automatically={false}
          reevaluates_automatically={@cell_view.eval.reevaluates_automatically}
        />
      </:primary>
      <:secondary>
        <.toggle_source_button />
        <.convert_smart_cell_button cell_id={@cell_view.id} />
        <.amplify_output_button />
        <.cell_link_button cell_id={@cell_view.id} />
        <.move_cell_up_button cell_id={@cell_view.id} />
        <.move_cell_down_button cell_id={@cell_view.id} />
        <.delete_cell_button cell_id={@cell_view.id} />
      </:secondary>
    </.cell_actions>
    <.cell_body>
      <div class="relative" data-el-cell-body-root>
        <div data-el-ui-box>
          <%= case @cell_view.status do %>
            <% :started -> %>
              <div class={
              "flex #{if(@cell_view.editor && @cell_view.editor.placement == :top, do: "flex-col-reverse", else: "flex-col")}"
            }>
                <.live_component
                  module={LivebookWeb.JSViewComponent}
                  id={@cell_view.id}
                  js_view={@cell_view.js_view}
                  session_id={@session_id}
                  client_id={@client_id}
                />
                <.cell_editor
                  :if={@cell_view.editor}
                  cell_id={@cell_view.id}
                  tag="secondary"
                  empty={@cell_view.editor.empty}
                  language={@cell_view.editor.language}
                  rounded={@cell_view.editor.placement}
                  intellisense={@cell_view.editor.language == "elixir"}
                  hidden={not @cell_view.editor.visible}
                />
              </div>
            <% :dead -> %>
              <div class="info-box">
                <%= if @installing? do %>
                  Waiting for dependency installation to complete...
                <% else %>
                  Run the notebook setup to show the contents of this Smart cell.
                <% end %>
              </div>
            <% :down -> %>
              <div class="info-box flex justify-between items-center">
                <span>
                  The Smart cell crashed unexpectedly, this is most likely a bug.
                </span>
                <.button
                  color="gray"
                  phx-click={JS.push("recover_smart_cell", value: %{cell_id: @cell_view.id})}
                >
                  Restart Smart cell
                </.button>
              </div>
            <% :starting -> %>
              <div class="delay-200">
                <.content_skeleton empty={false} />
              </div>
          <% end %>
        </div>
        <div data-el-editor-box>
          <div class="relative">
            <.cell_editor
              cell_id={@cell_view.id}
              tag="primary"
              empty={@cell_view.empty}
              language="elixir"
              intellisense
              read_only
            />
          </div>
        </div>
        <div class="absolute bottom-2 right-2" data-el-cell-indicators>
          <.cell_indicators id={@cell_view.id} cell_view={@cell_view} />
        </div>
      </div>
      <.evaluation_outputs
        outputs={@streams.outputs}
        cell_view={@cell_view}
        session_id={@session_id}
        session_pid={@session_pid}
        client_id={@client_id}
      />
    </.cell_body>
    """
  end

  defp cell_actions(assigns) do
    assigns =
      assigns
      |> assign_new(:primary, fn -> [] end)
      |> assign_new(:secondary, fn -> [] end)

    ~H"""
    <div class="mb-1 flex items-center justify-between">
      <div class="relative z-20 flex items-center justify-end space-x-2" data-el-actions data-primary>
        {render_slot(@primary)}
      </div>
      <div
        class="relative z-20 flex items-center justify-end md:space-x-2"
        role="toolbar"
        aria-label="cell actions"
        data-el-actions
        data-secondary
      >
        {render_slot(@secondary)}
      </div>
    </div>
    """
  end

  defp cell_body(assigns) do
    ~H"""
    <!-- By setting tabindex we can programmatically focus this element,
         also we actually want to make this element tab-focusable -->
    <div class="flex relative focus-visible:outline-none" data-el-cell-body tabindex="0">
      <div class="w-1 h-full rounded-lg absolute top-0 -left-3" data-el-cell-focus-indicator></div>
      <div class="w-full">
        {render_slot(@inner_block)}
      </div>
    </div>
    """
  end

  defp cell_evaluation_button(%{status: :ready} = assigns) do
    ~H"""
    <div class="flex items-center space-x-1">
      <button
        class="text-gray-600 hover:text-gray-800 flex space-x-1 items-center"
        data-el-queue-cell-evaluation-button
        data-cell-id={@cell_id}
      >
        <%= cond do %>
          <% @reevaluates_automatically -> %>
            <.remix_icon icon="check-line" class="text-xl" />
            <span class="text-sm font-medium">Reevaluates automatically</span>
          <% @validity == :evaluated -> %>
            <.remix_icon icon="play-circle-fill" class="text-xl" />
            <span class="text-sm font-medium">Reevaluate</span>
          <% true -> %>
            <.remix_icon icon="play-circle-fill" class="text-xl" />
            <span class="text-sm font-medium">Evaluate</span>
        <% end %>
      </button>
      <.menu id={"cell-#{@cell_id}-evaluation-menu"} position="bottom-left" distant>
        <:toggle>
          <button class="flex text-gray-600 hover:text-gray-800">
            <.remix_icon icon="arrow-down-s-line" class="text-xl" />
          </button>
        </:toggle>
        <.menu_item variant={if(not @reevaluate_automatically, do: "selected", else: "default")}>
          <button
            role="menuitem"
            phx-click={
              JS.push("set_reevaluate_automatically", value: %{value: false, cell_id: @cell_id})
            }
          >
            <.remix_icon icon="check-line" class={if(@reevaluate_automatically, do: "invisible")} />
            <span>Evaluate on demand</span>
          </button>
        </.menu_item>
        <.menu_item variant={if(@reevaluate_automatically, do: "selected", else: "default")}>
          <button
            role="menuitem"
            phx-click={
              JS.push("set_reevaluate_automatically", value: %{value: true, cell_id: @cell_id})
            }
          >
            <.remix_icon icon="check-line" class={if(not @reevaluate_automatically, do: "invisible")} />
            <span>Reevaluate automatically</span>
          </button>
        </.menu_item>
      </.menu>
    </div>
    """
  end

  defp cell_evaluation_button(assigns) do
    ~H"""
    <button
      class="text-gray-600 hover:text-gray-800 flex space-x-1 items-center"
      phx-click="cancel_cell_evaluation"
      phx-value-cell_id={@cell_id}
    >
      <.remix_icon icon="stop-circle-fill" class="text-xl" />
      <span class="text-sm font-medium">
        Stop
      </span>
    </button>
    """
  end

  defp setup_cell_evaluation_button(%{status: :ready} = assigns) do
    ~H"""
    <div class="flex items-center space-x-1">
      <button
        class="text-gray-600 hover:text-gray-800 flex space-x-1 items-center"
        data-el-queue-cell-evaluation-button
        data-cell-id={@cell_id}
      >
        <%= if @validity == :fresh do %>
          <.remix_icon icon="play-circle-fill" class="text-xl" />
          <span class="text-sm font-medium">Setup</span>
        <% else %>
          <.remix_icon icon="restart-fill" class="text-xl" />
          <span class="text-sm font-medium">Reconnect and setup</span>
        <% end %>
      </button>
      <%= unless Livebook.Runtime.fixed_dependencies?(@runtime) do %>
        <.menu id="setup-menu" position="bottom-left" distant>
          <:toggle>
            <button class="flex text-gray-600 hover:text-gray-800">
              <.remix_icon icon="arrow-down-s-line" class="text-xl" />
            </button>
          </:toggle>
          <.menu_item>
            <button
              role="menuitem"
              data-el-queue-cell-evaluation-button
              data-cell-id={@cell_id}
              data-disable-dependencies-cache
            >
              <.remix_icon icon="play-circle-fill" />
              <span>Setup without cache</span>
            </button>
          </.menu_item>
        </.menu>
      <% end %>
    </div>
    """
  end

  defp setup_cell_evaluation_button(assigns) do
    ~H"""
    <button
      class="text-gray-600 hover:text-gray-800 flex space-x-1 items-center"
      phx-click="cancel_cell_evaluation"
      phx-value-cell_id={@cell_id}
    >
      <.remix_icon icon="stop-circle-fill" class="text-xl" />
      <span class="text-sm font-medium">
        Stop
      </span>
    </button>
    """
  end

  defp enable_insert_mode_button(assigns) do
    ~H"""
    <span class="tooltip top" data-tooltip="Edit content" data-el-enable-insert-mode-button>
      <.icon_button aria-label="edit content">
        <.remix_icon icon="pencil-line" />
      </.icon_button>
    </span>
    """
  end

  defp toggle_source_button(assigns) do
    ~H"""
    <span class="tooltip top" data-tooltip="Toggle source" data-el-toggle-source-button>
      <.icon_button aria-label="toggle source">
        <.remix_icon icon="code-line" />
      </.icon_button>
    </span>
    """
  end

  defp convert_smart_cell_button(assigns) do
    ~H"""
    <span class="tooltip top" data-tooltip="Convert to Code cell">
      <.icon_button
        aria-label="toggle source"
        data-link-package-search
        phx-click={JS.push("convert_smart_cell", value: %{cell_id: @cell_id})}
      >
        <.remix_icon icon="pencil-line" />
      </.icon_button>
    </span>
    """
  end

  defp package_search_button(assigns) do
    ~H"""
    <%= if Livebook.Runtime.fixed_dependencies?(@runtime) do %>
      <span
        class="tooltip top"
        data-tooltip="The current runtime does not support adding dependencies"
      >
        <.icon_button disabled>
          <.remix_icon icon="play-list-add-line" />
        </.icon_button>
      </span>
    <% else %>
      <span class="tooltip top" data-tooltip="Add package (sp)">
        <.icon_button
          patch={~p"/sessions/#{@session_id}/package-search"}
          role="button"
          data-btn-package-search
        >
          <.remix_icon icon="play-list-add-line" />
        </.icon_button>
      </span>
    <% end %>
    """
  end

  defp cell_link_button(assigns) do
    ~H"""
    <span class="tooltip top" data-tooltip="Link">
      <.icon_button href={"#cell-#{@cell_id}"} role="button" aria-label="link to cell">
        <.remix_icon icon="link" />
      </.icon_button>
    </span>
    """
  end

  def amplify_output_button(assigns) do
    ~H"""
    <span class="tooltip top" data-tooltip="Amplify output" data-el-amplify-outputs-button>
      <.icon_button aria-label="amplify outputs">
        <.remix_icon icon="zoom-in-line" />
      </.icon_button>
    </span>
    """
  end

  defp cell_settings_button(assigns) do
    ~H"""
    <span class="tooltip top" data-tooltip="Cell settings">
      <.icon_button
        patch={~p"/sessions/#{@session_id}/cell-settings/#{@cell_id}"}
        aria-label="cell settings"
        role="button"
      >
        <.remix_icon icon="settings-3-line" />
      </.icon_button>
    </span>
    """
  end

  defp move_cell_up_button(assigns) do
    ~H"""
    <span class="tooltip top" data-tooltip="Move up">
      <.icon_button
        aria-label="move cell up"
        phx-click="move_cell"
        phx-value-cell_id={@cell_id}
        phx-value-offset="-1"
      >
        <.remix_icon icon="arrow-up-s-line" />
      </.icon_button>
    </span>
    """
  end

  defp move_cell_down_button(assigns) do
    ~H"""
    <span class="tooltip top" data-tooltip="Move down">
      <.icon_button
        aria-label="move cell down"
        phx-click="move_cell"
        phx-value-cell_id={@cell_id}
        phx-value-offset="1"
      >
        <.remix_icon icon="arrow-down-s-line" />
      </.icon_button>
    </span>
    """
  end

  defp delete_cell_button(assigns) do
    ~H"""
    <span class="tooltip top" data-tooltip="Delete">
      <.icon_button
        aria-label="delete cell"
        phx-click={JS.push("delete_cell", value: %{cell_id: @cell_id})}
      >
        <.remix_icon icon="delete-bin-6-line" />
      </.icon_button>
    </span>
    """
  end

  defp disable_language_button(assigns) do
    ~H"""
    <span class="tooltip top" data-tooltip="Delete">
      <.icon_button
        aria-label="delete cell"
        phx-click="disable_language"
        phx-value-language={@language}
      >
        <.remix_icon icon="delete-bin-6-line" />
      </.icon_button>
    </span>
    """
  end

  defp setup_cell_info(assigns) do
    ~H"""
    <span
      class="tooltip left"
      data-tooltip={
        ~s'''
        The setup cell includes code that initializes the notebook
        and should run only once. This is the best place to install
        dependencies and set global configuration.\
        '''
      }
    >
      <.icon_button>
        <.remix_icon icon="question-line" />
      </.icon_button>
    </span>
    """
  end

  defp pyproject_toml_cell_info(assigns) do
    ~H"""
    <span
      class="tooltip left"
      data-tooltip={
        ~s'''
        This cell specifies the Python environment using pyproject.toml
        configuration. While standardized to a certain extent, this
        configuration is used specifically with the uv package manager.\
        '''
      }
    >
      <.icon_button>
        <.remix_icon icon="question-line" />
      </.icon_button>
    </span>
    """
  end

  attr :cell_id, :string, required: true
  attr :tag, :string, required: true
  attr :empty, :boolean, required: true
  attr :language, :string, required: true
  attr :intellisense, :boolean, default: false
  attr :read_only, :boolean, default: false
  attr :rounded, :atom, default: :both
  attr :hidden, :boolean, default: false

  defp cell_editor(assigns) do
    ~H"""
    <div
      class={[@hidden && "hidden"]}
      id={"cell-editor-#{@cell_id}-#{@tag}"}
      phx-hook="CellEditor"
      data-p-cell-id={hook_prop(@cell_id)}
      data-p-tag={hook_prop(@tag)}
      data-p-language={hook_prop(@language)}
      data-p-intellisense={hook_prop(@intellisense)}
      data-p-read-only={hook_prop(@read_only)}
    >
      <div
        id={"cell-editor-#{@cell_id}-#{@tag}-container"}
        phx-update="ignore"
        class={["bg-editor", rounded_class(@rounded)]}
        data-el-editor-container
      >
        <div data-el-skeleton>
          <div class="py-3 px-8">
            <.content_skeleton bg_class="bg-gray-500" empty={@empty} />
          </div>
        </div>
      </div>
    </div>
    """
  end

  defp rounded_class(:both), do: "rounded-lg"
  defp rounded_class(:top), do: "rounded-t-lg"
  defp rounded_class(:bottom), do: "rounded-b-lg"

  defp doctest_summary(assigns) do
    ~H"""
    <div :if={@doctest_summary.failures_count > 0} class="pt-2" id={"doctest-summary-#{@cell_id}"}>
      <div class="error-box py-3">
        {doctest_summary_message(@doctest_summary)}
      </div>
    </div>
    """
  end

  defp doctest_summary_message(%{doctests_count: total, failures_count: failed}) do
    doctests_pl = LivebookWeb.HTMLHelpers.pluralize(total, "doctest", "doctests")
    failures_pl = if failed == 1, do: "failure has", else: "failures have"

    "#{failed} out of #{doctests_pl} failed (#{failures_pl} been reported above)"
  end

  defp evaluation_outputs(assigns) do
    ~H"""
    <div
      class="flex flex-col"
      data-el-outputs-container
      id={"outputs-#{@cell_view.id}-#{@cell_view.eval.outputs_batch_number}"}
      phx-update="stream"
    >
      <LivebookWeb.Output.output
        :for={{dom_id, output} <- @outputs}
        id={dom_id}
        output={output.output}
        session_id={@session_id}
        session_pid={@session_pid}
        client_id={@client_id}
        cell_id={@cell_view.id}
        input_views={@cell_view.eval.input_views}
      />
    </div>
    """
  end

  attr :id, :string, required: true
  attr :cell_view, :map, required: true
  attr :langauge_toggle, :boolean, default: false

  defp cell_indicators(assigns) do
    ~H"""
    <div class="flex gap-1">
      <.cell_indicator :if={has_status?(@cell_view)}>
        <.cell_status id={@id} cell_view={@cell_view} />
      </.cell_indicator>
      <%= if @langauge_toggle do %>
        <.menu id={"cell-#{@id}-language-menu"} position="bottom-right">
          <:toggle>
            <.cell_indicator class="cursor-pointer">
              <.language_icon language={cell_language(@cell_view)} class="w-3 h-3" />
            </.cell_indicator>
          </:toggle>
          <.menu_item :for={language <- Livebook.Notebook.Cell.Code.languages()}>
            <button
              role="menuitem"
              phx-click="set_cell_language"
              phx-value-language={language.language}
              phx-value-cell_id={@id}
            >
              <.cell_icon cell_type={:code} language={language.language} />
              <span>{language.name}</span>
            </button>
          </.menu_item>
        </.menu>
      <% else %>
        <.cell_indicator>
          <.language_icon language={cell_language(@cell_view)} class="w-3 h-3" />
        </.cell_indicator>
      <% end %>
    </div>
    """
  end

  attr :class, :string, default: nil
  slot :inner_block, required: true

  defp cell_indicator(assigns) do
    ~H"""
    <div
      data-el-cell-indicator
      class={[
        "px-1.5 h-[22px] rounded-lg flex items-center border bg-editor-lighter border-editor text-editor",
        @class
      ]}
    >
      {render_slot(@inner_block)}
    </div>
    """
  end

  defp cell_language(%{language: language}), do: Atom.to_string(language)
  defp cell_language(%{type: :smart}), do: "elixir"

  defp has_status?(%{eval: %{status: :ready, validity: :fresh}}), do: false
  defp has_status?(_cell_view), do: true

  defp cell_status(%{cell_view: %{eval: %{status: :evaluating}}} = assigns) do
    ~H"""
    <.cell_status_indicator variant="progressing" change_indicator={true}>
      <span
        class="font-mono"
        id={"#{@id}-cell-timer"}
        phx-hook="Timer"
        phx-update="ignore"
        data-p-start={hook_prop(DateTime.to_iso8601(@cell_view.eval.evaluation_start))}
      >
      </span>
    </.cell_status_indicator>
    """
  end

  defp cell_status(%{cell_view: %{eval: %{status: :queued}}} = assigns) do
    ~H"""
    <.cell_status_indicator variant="waiting">
      Queued
    </.cell_status_indicator>
    """
  end

  defp cell_status(%{cell_view: %{eval: %{validity: :evaluated}}} = assigns) do
    ~H"""
    <.cell_status_indicator
      variant={if(@cell_view.eval.errored, do: "error", else: "success")}
      change_indicator={true}
      tooltip={duration_label(@cell_view.eval.evaluation_time_ms)}
    >
      Evaluated
    </.cell_status_indicator>
    """
  end

  defp cell_status(%{cell_view: %{eval: %{validity: :stale}}} = assigns) do
    ~H"""
    <.cell_status_indicator
      variant="warning"
      change_indicator={true}
      tooltip={duration_label(@cell_view.eval.evaluation_time_ms)}
    >
      Stale
    </.cell_status_indicator>
    """
  end

  defp cell_status(%{cell_view: %{eval: %{validity: :aborted}}} = assigns) do
    ~H"""
    <.cell_status_indicator
      variant="inactive"
      tooltip={duration_label(@cell_view.eval.evaluation_time_ms)}
    >
      Aborted
    </.cell_status_indicator>
    """
  end

  defp cell_status(assigns), do: ~H""

  attr :variant, :string, required: true
  attr :tooltip, :string, default: nil
  attr :change_indicator, :boolean, default: false

  slot :inner_block, required: true

  defp cell_status_indicator(assigns) do
    ~H"""
    <div class={[@tooltip && "tooltip", "bottom distant-medium"]} data-tooltip={@tooltip}>
      <div class="flex items-center space-x-1">
        <div class="flex text-[11px]" data-el-cell-status>
          {render_slot(@inner_block)}
          <span :if={@change_indicator} data-el-change-indicator>*</span>
        </div>
        <.status_indicator variant={@variant} />
      </div>
    </div>
    """
  end

  defp duration_label(time_ms) when is_integer(time_ms) do
    evaluation_time =
      if time_ms > 100 do
        seconds = time_ms |> Kernel./(1000) |> Float.floor(1)
        "#{seconds}s"
      else
        "#{time_ms}ms"
      end

    "Took " <> evaluation_time
  end

  defp duration_label(_time_ms), do: nil

  defp smart_cell_js_view_ref(%{type: :smart, status: :started, js_view: %{ref: ref}}), do: ref
  defp smart_cell_js_view_ref(_cell_view), do: nil

  defp language_name(language) do
    Enum.find_value(
      Livebook.Notebook.Cell.Code.languages(),
      &(&1.language == language && &1.name)
    )
  end
end
