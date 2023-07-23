defmodule LivebookWeb.SessionLive.InsertButtonsComponent do
  use LivebookWeb, :live_component

  import LivebookWeb.SessionHelpers

  defguardp is_many(list) when tl(list) != []

  def render(assigns) do
    ~H"""
    <div
      class="relative top-0.5 m-0 flex justify-center"
      role="toolbar"
      aria-label="insert new"
      data-el-insert-buttons
    >
      <div
        class="absolute inset-0 h-[30px] z-[100] bg-white rounded-lg border-2 border-dashed border-gray-400"
        data-el-insert-drop-area
        data-section-id={@section_id}
        data-cell-id={@cell_id}
        id={"cell-#{@id}-dropzone"}
        phx-hook="Dropzone"
      >
      </div>
      <div class={
        "w-full md:absolute z-10 hover:z-[11] #{if(@persistent, do: "opacity-100", else: "opacity-0")} hover:opacity-100 focus-within:opacity-100 flex space-x-2 justify-center items-center"
      }>
        <.menu id={"cell-#{@id}-insert"} position={:bottom_left} distant>
          <:toggle>
            <button class="button-base button-small flex items-center pr-1">
              <div
                class="pr-2"
                phx-click="insert_cell_below"
                phx-value-type="code"
                phx-value-section_id={@section_id}
                phx-value-cell_id={@cell_id}
              >
                + <%= @default_language |> Atom.to_string() |> String.capitalize() %>
              </div>
              <div class="pl-1 flex items-center border-l border-gray-200">
                <.remix_icon icon="arrow-down-s-line" class="text-lg leading-none" />
              </div>
            </button>
          </:toggle>
          <.menu_item>
            <button
              role="menuitem"
              phx-click="set_default_language"
              phx-value-type="code"
              phx-value-language="elixir"
              phx-value-section_id={@section_id}
              phx-value-cell_id={@cell_id}
            >
              <.cell_icon cell_type={:code} language={:elixir} />
              <span>Elixir</span>
            </button>
          </.menu_item>
          <.menu_item>
            <button
              role="menuitem"
              phx-click="set_default_language"
              phx-value-type="code"
              phx-value-language="erlang"
              phx-value-section_id={@section_id}
              phx-value-cell_id={@cell_id}
            >
              <.cell_icon cell_type={:code} language={:erlang} />
              <span>Erlang</span>
            </button>
          </.menu_item>
        </.menu>
        <.menu id={"#{@id}-block-menu"} position={:bottom_left}>
          <:toggle>
            <button class="button-base button-small">+ Block</button>
          </:toggle>
          <.menu_item>
            <button
              role="menuitem"
              phx-click="insert_cell_below"
              phx-value-type="markdown"
              phx-value-section_id={@section_id}
              phx-value-cell_id={@cell_id}
            >
              <.remix_icon icon="markdown-fill" />
              <span>Markdown</span>
            </button>
          </.menu_item>
          <.menu_item>
            <button
              role="menuitem"
              phx-click="insert_section_below"
              phx-value-section_id={@section_id}
              phx-value-cell_id={@cell_id}
            >
              <.remix_icon icon="h-2" />
              <span>Section</span>
            </button>
          </.menu_item>
          <div class="flex items-center mt-4 mb-1 px-5 text-xs text-gray-400 font-light">
            MARKDOWN
          </div>
          <.menu_item>
            <button
              role="menuitem"
              phx-click="insert_cell_below"
              phx-value-type="diagram"
              phx-value-section_id={@section_id}
              phx-value-cell_id={@cell_id}
            >
              <.remix_icon icon="organization-chart" />
              <span>Diagram</span>
            </button>
          </.menu_item>
          <.menu_item>
            <button
              phx-click="insert_image"
              phx-value-section_id={@section_id}
              phx-value-cell_id={@cell_id}
              aria-label="insert image"
              role="menuitem"
            >
              <.remix_icon icon="image-add-line" />
              <span>Image</span>
            </button>
          </.menu_item>
          <%= if @example_snippet_definitions != [] do %>
            <div class="flex items-center mt-4 mb-1 px-5 text-xs text-gray-400 font-light">
              CODE
            </div>
            <.menu_item :for={definition <- @example_snippet_definitions}>
              <.example_snippet_insert_button
                definition={definition}
                section_id={@section_id}
                cell_id={@cell_id}
              />
            </.menu_item>
          <% end %>
        </.menu>
        <%= cond do %>
          <% not Livebook.Runtime.connected?(@runtime) -> %>
            <button
              class="button-base button-small"
              phx-click={
                JS.push("setup_default_runtime",
                  value: %{reason: "To see the available smart cells, you need a connected runtime."}
                )
              }
            >
              + Smart
            </button>
          <% @smart_cell_definitions == [] -> %>
            <span class="tooltip right" data-tooltip="No smart cells available">
              <button class="button-base button-small" disabled>+ Smart</button>
            </span>
          <% true -> %>
            <.menu id={"#{@id}-smart-menu"} position={:bottom_left}>
              <:toggle>
                <button class="button-base button-small">+ Smart</button>
              </:toggle>
              <.menu_item :for={definition <- @smart_cell_definitions}>
                <.smart_cell_insert_button
                  definition={definition}
                  section_id={@section_id}
                  cell_id={@cell_id}
                />
              </.menu_item>
            </.menu>
        <% end %>
      </div>
    </div>
    """
  end

  defp example_snippet_insert_button(assigns) when is_many(assigns.definition.variants) do
    ~H"""
    <.submenu>
      <:primary>
        <button role="menuitem">
          <.remix_icon icon={@definition.icon} />
          <span><%= @definition.name %></span>
        </button>
      </:primary>
      <.menu_item :for={{variant, idx} <- Enum.with_index(@definition.variants)}>
        <button
          role="menuitem"
          phx-click={on_example_snippet_click(@definition, idx, @section_id, @cell_id)}
        >
          <span><%= variant.name %></span>
        </button>
      </.menu_item>
    </.submenu>
    """
  end

  defp example_snippet_insert_button(assigns) do
    ~H"""
    <button
      role="menuitem"
      phx-click={on_example_snippet_click(@definition, 0, @section_id, @cell_id)}
    >
      <.remix_icon icon={@definition.icon} />
      <span><%= @definition.name %></span>
    </button>
    """
  end

  defp smart_cell_insert_button(assigns) when is_many(assigns.definition.requirement_presets) do
    ~H"""
    <.submenu>
      <:primary>
        <button role="menuitem">
          <span><%= @definition.name %></span>
        </button>
      </:primary>
      <.menu_item :for={{preset, idx} <- Enum.with_index(@definition.requirement_presets)}>
        <button
          role="menuitem"
          phx-click={on_smart_cell_click(@definition, idx, @section_id, @cell_id)}
        >
          <span><%= preset.name %></span>
        </button>
      </.menu_item>
    </.submenu>
    """
  end

  defp smart_cell_insert_button(assigns) do
    ~H"""
    <button role="menuitem" phx-click={on_smart_cell_click(@definition, @section_id, @cell_id)}>
      <span><%= @definition.name %></span>
    </button>
    """
  end

  defp on_example_snippet_click(definition, variant_idx, section_id, cell_id) do
    JS.push("insert_example_snippet_below",
      value: %{
        definition_name: definition.name,
        variant_idx: variant_idx,
        section_id: section_id,
        cell_id: cell_id
      }
    )
  end

  defp on_smart_cell_click(definition, section_id, cell_id) do
    preset_idx = if definition.requirement_presets == [], do: nil, else: 0
    on_smart_cell_click(definition, preset_idx, section_id, cell_id)
  end

  defp on_smart_cell_click(definition, preset_idx, section_id, cell_id) do
    JS.push("insert_smart_cell_below",
      value: %{
        kind: definition.kind,
        section_id: section_id,
        cell_id: cell_id,
        preset_idx: preset_idx
      }
    )
  end
end
