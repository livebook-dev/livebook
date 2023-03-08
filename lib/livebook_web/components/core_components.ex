defmodule LivebookWeb.CoreComponents do
  use Phoenix.Component

  alias Phoenix.LiveView.JS

  @doc """
  Renders [Remix](https://remixicon.com) icon.

  ## Examples

      <.remix_icon icon="cpu-line" />

      <.remix_icon icon="cpu-line" class="align-middle mr-1" />

  """
  attr :icon, :string, required: true
  attr :class, :string, default: nil
  attr :rest, :global

  def remix_icon(assigns) do
    ~H"""
    <i class={["ri-#{@icon}", @class]} aria-hidden="true" {@rest}></i>
    """
  end

  @doc """
  Renders flash notices.

  ## Examples

      <.flash kind={:info} flash={@flash} />

  """
  attr :flash, :map, default: %{}, doc: "the map of flash messages to display"

  attr :kind, :atom,
    values: [:info, :success, :warning, :error],
    doc: "used for styling and flash lookup"

  attr :rest, :global, doc: "the arbitrary HTML attributes to add to the flash container"

  def flash(assigns) do
    ~H"""
    <div
      :if={message = Phoenix.Flash.get(@flash, @kind)}
      class={[
        "shadow-custom-1 max-w-2xl flex items-center space-x-3 rounded-lg px-4 py-2 border-l-4 rounded-l-none bg-white text-gray-600 hover:bg-gray-50 hover:text-gray-500 cursor-pointer",
        @kind == :info && "border-blue-500",
        @kind == :success && "border-blue-500",
        @kind == :warning && "border-yellow-300",
        @kind == :error && "border-red-500"
      ]}
      role="alert"
      phx-click="lv:clear-flash"
      phx-value-key={@kind}
      {@rest}
    >
      <.remix_icon :if={@kind == :info} icon="information-line" class="text-2xl text-blue-500" />
      <.remix_icon :if={@kind == :success} icon="checkbox-circle-fill" class="text-2xl text-blue-500" />
      <.remix_icon :if={@kind == :warning} icon="alert-line" class="text-2xl text-yellow-400" />
      <.remix_icon :if={@kind == :error} icon="close-circle-line" class="text-2xl text-red-500" />
      <span class="whitespace-pre-wrap pr-2 max-h-52 overflow-y-auto tiny-scrollbar" phx-no-format><%= message %></span>
    </div>
    """
  end

  @doc """
  Shows the flash group with standard titles and content.

  ## Examples

      <.flash_group flash={@flash} />

  """
  attr :flash, :map, required: true, doc: "the map of flash messages"

  def flash_group(assigns) do
    ~H"""
    <div class="fixed right-8 top-5 z-[1000] flex flex-col space-y-3">
      <.flash kind={:info} flash={@flash} />
      <.flash kind={:success} flash={@flash} />
      <.flash kind={:warning} flash={@flash} />
      <.flash kind={:error} flash={@flash} />
    </div>
    """
  end

  @doc """
  Creates a live region with the given role.

  ## Examples

      <.live_region role="alert" />
      <.live_region role="status" />
  """
  def live_region(assigns) do
    ~H"""
    <div class="sr-only" role={@role} id="live-region"></div>
    """
  end

  @doc """
  Sends a message to be read by the screen reader by changing the text content of the live region
  """
  def sr_message(js \\ %JS{}, message) do
    JS.dispatch(js, "lb:set_text", to: "#live-region", detail: %{value: message})
  end

  @doc """
  Wraps the given content in a modal dialog.

  ## Example

      <.modal id="edit-modal" patch={...}>
        <.live_component module={MyComponent}  />
      </.modal>

  """
  attr :id, :string, required: true
  attr :show, :boolean, default: false
  attr :patch, :string, default: nil
  attr :navigate, :string, default: nil
  attr :class, :string, default: nil
  attr :width, :atom, values: [:small, :medium, :big, :large], required: true
  attr :rest, :global

  slot :inner_block, required: true

  def modal(assigns) do
    ~H"""
    <div
      id={@id}
      class={["fixed z-[10000] inset-0", if(@show, do: "fade-in", else: "hidden")]}
      phx-remove={JS.transition("fade-out")}
      {@rest}
    >
      <!-- Modal container -->
      <div class="h-screen flex items-center justify-center p-4">
        <!-- Overlay -->
        <div class="absolute z-0 inset-0 bg-gray-500 opacity-75" aria-hidden="true"></div>
        <!-- Modal box -->
        <.focus_wrap
          id={"#{@id}-content"}
          class={[
            "relative max-h-full overflow-y-auto bg-white rounded-lg shadow-xl",
            "w-full",
            modal_width_class(@width)
          ]}
          role="dialog"
          aria-modal="true"
          tabindex="0"
          autofocus
          phx-window-keydown={hide_modal(@id)}
          phx-click-away={hide_modal(@id)}
          phx-key="escape"
        >
          <.link :if={@patch} patch={@patch} class="hidden" id={"#{@id}-return"}></.link>
          <.link :if={@navigate} patch={@navigate} class="hidden" id={"#{@id}-return"}></.link>
          <button
            type="button"
            class="absolute top-6 right-6 text-gray-400 flex space-x-1 items-center"
            aria_label="close modal"
            phx-click={hide_modal(@id)}
          >
            <span class="text-sm">(esc)</span>
            <.remix_icon icon="close-line" class="text-2xl" />
          </button>
          <%= render_slot(@inner_block) %>
        </.focus_wrap>
      </div>
    </div>
    """
  end

  defp modal_width_class(:small), do: "max-w-sm"
  defp modal_width_class(:medium), do: "max-w-xl"
  defp modal_width_class(:big), do: "max-w-4xl"
  defp modal_width_class(:large), do: "max-w-6xl"

  @doc """
  Shows a modal rendered with `modal/1`.
  """
  def show_modal(js \\ %JS{}, id) do
    js
    |> JS.show(
      to: "##{id}",
      transition: {"ease-out duration-200", "opacity-0", "opacity-100"}
    )
  end

  @doc """
  Hides a modal rendered with `modal/1`.
  """
  def hide_modal(js \\ %JS{}, id) do
    js
    |> JS.hide(
      to: "##{id}",
      transition: {"ease-in duration-200", "opacity-100", "opacity-0"}
    )
    |> JS.dispatch("click", to: "##{id}-return")
  end

  @doc """
  Renders the confirmation modal for `with_confirm/3`.
  """
  attr :id, :string, required: true

  def confirm_modal(assigns) do
    ~H"""
    <.modal id={@id} width={:medium} phx-hook="ConfirmModal" data-js-show={show_modal(@id)}>
      <div id={"#{@id}-confirm-content"} class="p-6 flex flex-col" phx-update="ignore">
        <h3 class="text-2xl font-semibold text-gray-800" data-title></h3>
        <p class="mt-8 text-gray-700" data-description></p>
        <label class="mt-6 text-gray-700 flex items-center" data-opt-out>
          <input class="checkbox mr-3" type="checkbox" />
          <span class="text-sm">
            Don't show this message again
          </span>
        </label>
        <div class="mt-8 flex justify-end">
          <div class="flex space-x-2" data-actions>
            <button class="button-base button-outlined-gray" phx-click={hide_modal(@id)}>
              Cancel
            </button>
            <button
              class="button-base"
              phx-click={hide_modal(@id) |> JS.dispatch("lb:confirm", to: "##{@id}")}
              data-confirm-button
            >
              <i aria-hidden="true" data-confirm-icon></i>
              <span data-confirm-text></span>
            </button>
          </div>
        </div>
      </div>
    </.modal>
    """
  end

  @doc """
  Shows a confirmation modal before executing the given JS action.

  The modal template must already be on the page, see `confirm_modal/1`.

  ## Options

    * `:title` - title of the confirmation modal. Defaults to `"Are you sure?"`

    * `:description` - content of the confirmation modal. Required

    * `:confirm_text` - text of the confirm button. Defaults to `"Yes"`

    * `:confirm_icon` - icon in the confirm button. Optional

    * `:danger` - whether the action is destructive or regular. Defaults to `true`

    * `:html` - whether the `:description` is a raw HTML. Defaults to `false`

    * `:opt_out_id` - enables the "Don't show this message again"
      checkbox. Once checked by the user, the confirmation with this
      id is never shown again. Optional

  ## Examples

        <button class="..."
          phx-click={
            with_confirm(
              JS.push("delete_item", value: %{id: @item_id}),
              title: "Delete item",
              description: "Are you sure you want to delete item?",
              confirm_text: "Delete",
              confirm_icon: "delete-bin-6-line",
              opt_out_id: "delete-item"
            )
          }>
          Delete
        </button>
  """
  def with_confirm(js \\ %JS{}, on_confirm, opts) do
    opts =
      Keyword.validate!(
        opts,
        [
          :confirm_icon,
          :description,
          :opt_out_id,
          title: "Are you sure?",
          confirm_text: "Yes",
          danger: true,
          html: false
        ]
      )

    JS.dispatch(js, "lb:confirm_request",
      detail: %{
        on_confirm: Jason.encode!(on_confirm.ops),
        title: opts[:title],
        description: Keyword.fetch!(opts, :description),
        confirm_text: opts[:confirm_text],
        confirm_icon: opts[:confirm_icon],
        danger: opts[:danger],
        html: opts[:html],
        opt_out_id: opts[:opt_out_id]
      }
    )
  end

  @doc """
  Renders a popup menu that shows up on toggle click.

  ## Examples

      <.menu id="my-menu">
        <:toggle>
          <button>Open</button>
        </:toggle>
        <.menu_item>
          <button role="menuitem">Option 1</button>
        </.menu_item>
        <.menu_item>
          <button role="menuitem">Option 2</button>
        </.menu_item>
      </.menu>

  """
  attr :id, :string, required: true
  attr :disabled, :boolean, default: false, doc: "whether the menu is active"

  attr :position, :atom,
    default: :bottom_right,
    values: [:top_left, :top_right, :bottom_left, :bottom_right]

  attr :distant, :boolean,
    default: false,
    doc: "whether the menu should be further from the anchor element"

  attr :secondary_click, :boolean,
    default: false,
    doc: "whether secondary click (usually right mouse click) should open the menu"

  slot :toggle, required: true

  slot :inner_block, required: true

  def menu(assigns) do
    ~H"""
    <div class="relative" id={@id}>
      <div
        phx-click={not @disabled && show_menu(@id)}
        data-contextmenu-trigger-click={@secondary_click}
        phx-window-keydown={hide_menu(@id)}
        phx-key="escape"
      >
        <%= render_slot(@toggle) %>
      </div>
      <div id={"#{@id}-overlay"} class="fixed z-[90] inset-0 hidden" phx-click-away={hide_menu(@id)}>
      </div>
      <menu
        id={"#{@id}-content"}
        class={[
          "absolute z-[100] rounded-lg bg-white flex flex-col py-2 shadow-[0_15px_99px_-0px_rgba(12,24,41,0.15)] hidden",
          menu_position_class(@position),
          @distant && menu_distant_class(@position)
        ]}
        role="menu"
        phx-click-away={hide_menu(@id)}
      >
        <%= render_slot(@inner_block) %>
      </menu>
    </div>
    """
  end

  defp show_menu(id) do
    JS.show(to: "##{id}-overlay")
    |> JS.show(to: "##{id}-content", display: "flex")
  end

  defp hide_menu(id) do
    JS.hide(to: "##{id}-overlay")
    |> JS.hide(to: "##{id}-content")
  end

  defp menu_position_class(:top_left), do: "top-0 left-0 transform -translate-y-full -mt-1"
  defp menu_position_class(:top_right), do: "top-0 right-0 transform -translate-y-full -mt-1"
  defp menu_position_class(:bottom_left), do: "bottom-0 left-0 transform translate-y-full -mb-1"
  defp menu_position_class(:bottom_right), do: "bottom-0 right-0 transform translate-y-full -mb-1"

  defp menu_distant_class(position) when position in [:top_left, :top_right], do: "-mt-2"
  defp menu_distant_class(position) when position in [:bottom_left, :bottom_right], do: "-mb-2"

  @doc """
  Wraps a menu item that shows a submenu on hover.

  This component should be used within `menu/1` content.

  ## Example

      <.submenu>
        <:primary>
          <button class"menu-item" role="menuitem">Submenu</button>
        </:primary>
        <.menu_item>
          <button class"menu-item" role="menuitem">Option 1</button>
        </.menu_item>
      </.submenu>

  """
  slot :primary, required: true
  slot :inner_block, required: true

  def submenu(assigns) do
    ~H"""
    <div class="group relative">
      <%= render_slot(@primary) %>
      <div class="absolute -top-2 right-0 translate-x-full pl-2 hidden group-hover:flex group-focus-within:flex">
        <menu class="relative mt-0 z-[100] rounded-lg bg-white flex flex-col py-2 shadow-[0_15px_99px_-0px_rgba(12,24,41,0.15)]">
          <%= render_slot(@inner_block) %>
        </menu>
      </div>
    </div>
    """
  end

  @doc """
  Renders a menu item used in `menu/1` and `submenu/1`.
  """
  attr :disabled, :boolean, default: false
  attr :variant, :atom, default: :default, values: [:default, :selected, :danger]

  slot :inner_block, required: true

  def menu_item(assigns) do
    ~H"""
    <li class={[
      "w-full",
      "[&>:first-child]:w-full [&>:first-child]:flex [&>:first-child]:space-x-3 [&>:first-child]:px-5 [&>:first-child]:py-2 [&>:first-child]:items-center [&>:first-child:hover]:bg-gray-100 [&>:first-child:focus]:bg-gray-100 [&>:first-child]:whitespace-nowrap font-medium",
      menu_item_class(@variant),
      @disabled && "pointer-events-none opacity-50"
    ]}>
      <%= render_slot(@inner_block) %>
    </li>
    """
  end

  defp menu_item_class(:default), do: "text-gray-500"
  defp menu_item_class(:selected), do: "text-gray-900"
  defp menu_item_class(:danger), do: "text-red-600"

  @doc """
  Renders a text content skeleton.
  """
  attr :empty, :boolean, default: false, doc: "if the source is empty"
  attr :bg_class, :string, default: "bg-gray-200", doc: "the skeleton background color"

  def content_skeleton(assigns) do
    ~H"""
    <%= if @empty do %>
      <div class="h-4"></div>
    <% else %>
      <div class="max-w-2xl w-full animate-pulse">
        <div class="flex-1 space-y-4">
          <div class={[@bg_class, "h-4 rounded-lg w-3/4"]}></div>
          <div class={[@bg_class, "h-4 rounded-lg"]}></div>
          <div class={[@bg_class, "h-4 rounded-lg w-5/6"]}></div>
        </div>
      </div>
    <% end %>
    """
  end

  @doc """
  Renders a highlighted code snippet.

  ## Examples

      <.code_preview
        source_id="my-snippet"
        language="elixir"
        source="System.version()" />

  """
  attr :source_id, :string, required: true
  attr :language, :string, required: true
  attr :source, :string, required: true

  def code_preview(assigns) do
    ~H"""
    <div class="markdown">
      <pre><code
      class="tiny-scrollbar"
      id={"#{@source_id}-highlight"}
      phx-hook="Highlight"
      data-language={@language}
    ><div id={@source_id} data-source><%= @source %></div><div data-target></div></code></pre>
    </div>
    """
  end

  @doc """
  Renders text with a tiny label.

  ## Examples

      <.labeled_text label="Name">Sherlock Holmes</.labeled_text>

  """
  attr :label, :string, required: true

  attr :one_line, :boolean,
    default: false,
    doc: "whether to force the text into a single scrollable line"

  slot :inner_block, required: true

  def labeled_text(assigns) do
    ~H"""
    <div class="flex flex-col space-y-1">
      <span class="text-sm text-gray-500">
        <%= @label %>
      </span>
      <span class={
        "text-gray-800 text-sm font-semibold #{if @one_line, do: "whitespace-nowrap overflow-auto tiny-scrollbar"}"
      }>
        <%= render_slot(@inner_block) %>
      </span>
    </div>
    """
  end

  @doc """
  Renders a choice button that is either active or not.

  ## Examples

      <.choice_button active={@tab == "my_tab"} phx-click="set_my_tab">
        My tab
      </.choice_button>

  """
  attr :active, :boolean, required: true
  attr :disabled, :boolean
  attr :class, :string, default: nil
  attr :rest, :global

  slot :inner_block, required: true

  def choice_button(assigns) do
    assigns =
      assigns
      |> assign_new(:disabled, fn -> assigns.active end)

    ~H"""
    <button
      class={[
        "px-5 py-2 rounded-lg border text-gray-700 bg-white border-gray-200",
        @active && "bg-blue-100 border-blue-600",
        @class
      ]}
      disabled={@disabled}
      {@rest}
    >
      <%= render_slot(@inner_block) %>
    </button>
    """
  end

  @doc """
  Renders an status indicator circle.
  """
  attr :variant, :atom,
    required: true,
    values: [:success, :warning, :error, :inactive, :waiting, :progressing]

  def status_indicator(assigns) do
    ~H"""
    <span class="relative flex h-3 w-3">
      <span
        :if={animated_circle_class(@variant)}
        class={[
          animated_circle_class(@variant),
          "animate-ping absolute inline-flex h-full w-full rounded-full opacity-75"
        ]}
      >
      </span>
      <span class={[circle_class(@variant), "relative inline-flex rounded-full h-3 w-3"]}></span>
    </span>
    """
  end

  defp circle_class(:success), do: "bg-green-bright-400"
  defp circle_class(:warning), do: "bg-yellow-bright-200"
  defp circle_class(:error), do: "bg-red-400"
  defp circle_class(:inactive), do: "bg-gray-500"
  defp circle_class(:waiting), do: "bg-gray-400"
  defp circle_class(:progressing), do: "bg-blue-500"

  defp animated_circle_class(:waiting), do: "bg-gray-300"
  defp animated_circle_class(:progressing), do: "bg-blue-400"
  defp animated_circle_class(_other), do: nil

  @doc """
  Renders an informative box as a placeholder for a list.
  """

  slot :inner_block, required: true
  slot :actions

  def no_entries(assigns) do
    ~H"""
    <div class="p-5 flex space-x-4 items-center border border-gray-200 rounded-lg">
      <div>
        <.remix_icon icon="windy-line" class="text-gray-400 text-xl" />
      </div>
      <div class="grow flex items-center justify-between">
        <div class="text-gray-600">
          <%= render_slot(@inner_block) %>
        </div>
        <%= render_slot(@actions) %>
      </div>
    </div>
    """
  end

  # JS commands

  @doc """
  Toggles classes on elements.
  """
  def toggle_class(js \\ %JS{}, names, opts \\ []) do
    opts = Keyword.validate!(opts, [:to])

    to = Keyword.fetch!(opts, :to)

    names
    |> String.split()
    |> Enum.reduce(js, fn name, js ->
      js
      |> JS.remove_class(name, to: "#{to}.#{name}")
      |> JS.add_class(name, to: "#{to}:not(.#{name})")
    end)
  end
end
