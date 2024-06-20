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
  attr :class, :any, default: nil
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
      class="relative group shadow-lg max-w-xl px-3 py-2 flex items-center gap-2 rounded-lg bg-gray-900 text-gray-200 text-sm"
      role="alert"
      {@rest}
    >
      <div
        class="opacity-0 group-hover:opacity-100 absolute -left-2 -top-2 bg-gray-900 border border-gray-600 rounded-full p-0.5 flex leading-none text-gray-200 hover:text-gray-100 hover:bg-gray-700 text-sm cursor-pointer"
        phx-click="lv:clear-flash"
        phx-value-key={@kind}
      >
        <.remix_icon icon="close-line" />
      </div>
      <.remix_icon :if={@kind == :info} icon="information-fill" class="text-xl text-blue-500" />
      <.remix_icon
        :if={@kind == :success}
        icon="checkbox-circle-fill"
        class="text-xl text-green-bright-400"
      />
      <.remix_icon :if={@kind == :warning} icon="alert-fill" class="text-xl text-yellow-500" />
      <.remix_icon :if={@kind == :error} icon="error-warning-fill" class="text-xl text-red-500" />
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
  Renders a message notice.

  Similar to `flash/1`, but for permanent messages on the page.

  ## Examples

      <.message_box kind={:info} message="🦊 in a 📦" />

      <.message_box kind={:info}>
        <span>🦊</span> in a <span>📦</span>
      </.message_box>

  """

  attr :message, :string, default: nil
  attr :kind, :atom, values: [:info, :success, :warning, :error]

  slot :inner_block

  def message_box(assigns)

  def message_box(assigns) do
    if assigns.message && assigns.inner_block != [] do
      raise ArgumentError, "expected either message or inner_block, got both."
    end

    ~H"""
    <div class={[
      "shadow text-sm rounded-lg px-4 py-2 border-l-4 rounded-l-none bg-white text-gray-700",
      @kind == :info && "border-blue-500",
      @kind == :success && "border-green-bright-400",
      @kind == :warning && "border-yellow-300",
      @kind == :error && "border-red-500"
    ]}>
      <div
        :if={@message}
        class="whitespace-pre-wrap pr-2 max-h-52 overflow-y-auto tiny-scrollbar"
        phx-no-format
      ><%= @message %></div>
      <%= if @inner_block != [] do %>
        <%= render_slot(@inner_block) %>
      <% end %>
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
      class="fixed z-[10000] inset-0 hidden"
      phx-mounted={@show && show_modal(@id)}
      phx-remove={hide_modal(@id)}
      data-cancel={modal_on_cancel(@patch, @navigate)}
      {@rest}
    >
      <!-- Modal container -->
      <div id={"#{@id}-container"} class="h-screen flex items-center justify-center p-4">
        <!-- Overlay -->
        <div class="absolute z-0 inset-0 bg-gray-500 opacity-75" aria-hidden="true"></div>
        <!-- Modal box -->
        <.focus_wrap
          id={"#{@id}-content"}
          class={[
            "relative max-h-full overflow-y-auto bg-white rounded-lg shadow-xl",
            "w-full p-6",
            modal_width_class(@width)
          ]}
          role="dialog"
          aria-modal="true"
          tabindex="0"
          autofocus
          phx-window-keydown={JS.exec("data-cancel", to: "##{@id}")}
          phx-click-away={JS.exec("data-cancel", to: "##{@id}")}
          phx-key="escape"
        >
          <button
            type="button"
            class="absolute top-6 right-6 text-gray-400 flex space-x-1 items-center"
            aria_label="close modal"
            phx-click={JS.exec("data-cancel", to: "##{@id}")}
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

  defp modal_on_cancel(nil, nil), do: JS.exec("phx-remove")
  defp modal_on_cancel(nil, navigate), do: JS.patch(navigate)
  defp modal_on_cancel(patch, nil), do: JS.patch(patch)

  @doc """
  Shows a modal rendered with `modal/1`.
  """
  def show_modal(js \\ %JS{}, id) do
    js
    |> JS.show(to: "##{id}")
    |> JS.transition(
      {"ease-out duration-200", "opacity-0", "opacity-100"},
      to: "##{id}-container"
    )
  end

  @doc """
  Hides a modal rendered with `modal/1`.
  """
  def hide_modal(js \\ %JS{}, id) do
    js
    |> JS.transition(
      {"ease-in duration-200", "opacity-100", "opacity-0"},
      to: "##{id}-container"
    )
    |> JS.hide(to: "##{id}", transition: {"block", "block", "hidden"})
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

  attr :sm_position, :atom,
    default: nil,
    values: [nil, :top_left, :top_right, :bottom_left, :bottom_right]

  attr :md_position, :atom,
    default: nil,
    values: [nil, :top_left, :top_right, :bottom_left, :bottom_right]

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
          "absolute z-[100] hidden",
          menu_position_class(@position),
          @md_position && menu_md_position_class(@md_position),
          @sm_position && menu_sm_position_class(@sm_position),
          @distant && menu_distant_class(@position)
        ]}
        role="menu"
        phx-click-away={hide_menu(@id)}
      >
        <div
          id={"#{@id}-content-inner"}
          class="rounded-lg bg-white flex flex-col py-2 shadow-[0_15px_99px_-0px_rgba(12,24,41,0.15)]"
        >
          <%= render_slot(@inner_block) %>
        </div>
      </menu>
    </div>
    """
  end

  @doc """
  Shows a menu rendered with `menu/1`.

  ## Options

    * `:animate` - whether to play an animation when the menu is opened.
      Defaults to `false`

  """
  def show_menu(js \\ %JS{}, id, opts \\ []) do
    opts = Keyword.validate!(opts, animate: false)

    js =
      js
      |> JS.show(to: "##{id}-overlay")
      |> JS.show(to: "##{id}-content", display: "flex")
      |> JS.dispatch("lb:scroll_into_view", to: "##{id}-content")

    if opts[:animate] do
      JS.add_class(js, "animate-shake", to: "##{id}-content-inner")
    else
      js
    end
  end

  @doc """
  Hides a menu rendered with `menu/1`.
  """
  def hide_menu(js \\ %JS{}, id) do
    js
    |> JS.hide(to: "##{id}-overlay")
    |> JS.hide(to: "##{id}-content")
    |> JS.remove_class("animate-shake", to: "##{id}-content-inner")
  end

  defp menu_position_class(:top_left), do: "top-0 left-0 transform -translate-y-full -mt-1"
  defp menu_position_class(:top_right), do: "top-0 right-0 transform -translate-y-full -mt-1"
  defp menu_position_class(:bottom_left), do: "bottom-0 left-0 transform translate-y-full -mb-1"
  defp menu_position_class(:bottom_right), do: "bottom-0 right-0 transform translate-y-full -mb-1"

  defp menu_md_position_class(:top_left) do
    "md:top-0 md:left-0 md:bottom-auto md:right-auto md:transform md:-translate-y-full md:-mt-1 md:mb-0"
  end

  defp menu_md_position_class(:top_right) do
    "md:top-0 md:right-0 md:bottom-auto md:left-auto md:transform md:-translate-y-full md:-mt-1 md:mb-0"
  end

  defp menu_md_position_class(:bottom_left) do
    "md:bottom-0 md:left-0 md:top-auto md:right-auto md:transform md:translate-y-full md:-mb-1 md:mt-0"
  end

  defp menu_md_position_class(:bottom_right) do
    "md:bottom-0 md:right-0 md:top-auto md:left-auto md:transform md:translate-y-full md:-mb-1 md:mt-0"
  end

  defp menu_sm_position_class(:top_left) do
    "sm:top-0 sm:left-0 sm:bottom-auto sm:right-auto sm:transform sm:-translate-y-full sm:-mt-1 sm:mb-0"
  end

  defp menu_sm_position_class(:top_right) do
    "sm:top-0 sm:right-0 sm:bottom-auto sm:left-auto sm:transform sm:-translate-y-full sm:-mt-1 sm:mb-0"
  end

  defp menu_sm_position_class(:bottom_left) do
    "sm:bottom-0 sm:left-0 sm:top-auto sm:right-auto sm:transform sm:translate-y-full sm:-mb-1 sm:mt-0"
  end

  defp menu_sm_position_class(:bottom_right) do
    "sm:bottom-0 sm:right-0 sm:top-auto sm:left-auto sm:transform sm:translate-y-full sm:-mb-1 sm:mt-0"
  end

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
  attr :wrap, :boolean, default: false

  def code_preview(assigns) do
    ~H"""
    <div class="markdown">
      <pre><code
      class={if(@wrap, do: "break-all whitespace-pre-wrap", else: "tiny-scrollbar")}
      id={"#{@source_id}-highlight"}
      phx-hook="Highlight"
      data-p-language={hook_prop(@language)}
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

  attr :class, :string, default: nil

  slot :inner_block, required: true

  def labeled_text(assigns) do
    ~H"""
    <div class={["flex flex-col space-y-1", @class]}>
      <span class="text-sm text-gray-500">
        <%= @label %>
      </span>
      <span class={[
        "text-gray-800 text-sm font-semibold",
        @one_line &&
          "whitespace-nowrap overflow-hidden text-ellipsis hover:text-clip hover:overflow-auto hover:tiny-scrollbar"
      ]}>
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
        "px-5 py-2 rounded-lg text-gray-700 border",
        if(@active, do: "bg-blue-100 border-blue-600", else: "bg-white border-gray-200"),
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
        :if={animated_status_circle_class(@variant)}
        class={[
          animated_status_circle_class(@variant),
          "animate-ping absolute inline-flex h-full w-full rounded-full opacity-75"
        ]}
      >
      </span>
      <span class={[status_circle_class(@variant), "relative inline-flex rounded-full h-3 w-3"]}>
      </span>
    </span>
    """
  end

  @doc """
  Returns background class based on the given variant.

  See `status_indicator/1` for available variants.
  """
  def status_circle_class(variant)

  def status_circle_class(:success), do: "bg-green-bright-400"
  def status_circle_class(:warning), do: "bg-yellow-bright-200"
  def status_circle_class(:error), do: "bg-red-400"
  def status_circle_class(:inactive), do: "bg-gray-500"
  def status_circle_class(:waiting), do: "bg-gray-400"
  def status_circle_class(:progressing), do: "bg-blue-500"

  defp animated_status_circle_class(:waiting), do: "bg-gray-300"
  defp animated_status_circle_class(:progressing), do: "bg-blue-400"
  defp animated_status_circle_class(_other), do: nil

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

  @doc """
  Renders a circular spinner.
  """

  attr :class, :string, default: nil
  attr :rest, :global

  def spinner(assigns) do
    ~H"""
    <svg
      aria-hidden="true"
      class={["inline w-4 h-4 text-gray-200 animate-spin fill-blue-600", @class]}
      viewBox="0 0 100 101"
      fill="none"
      xmlns="http://www.w3.org/2000/svg"
      {@rest}
    >
      <path
        d="M100 50.5908C100 78.2051 77.6142 100.591 50 100.591C22.3858 100.591 0 78.2051 0 50.5908C0 22.9766 22.3858 0.59082 50 0.59082C77.6142 0.59082 100 22.9766 100 50.5908ZM9.08144 50.5908C9.08144 73.1895 27.4013 91.5094 50 91.5094C72.5987 91.5094 90.9186 73.1895 90.9186 50.5908C90.9186 27.9921 72.5987 9.67226 50 9.67226C27.4013 9.67226 9.08144 27.9921 9.08144 50.5908Z"
        fill="currentColor"
      />
      <path
        d="M93.9676 39.0409C96.393 38.4038 97.8624 35.9116 97.0079 33.5539C95.2932 28.8227 92.871 24.3692 89.8167 20.348C85.8452 15.1192 80.8826 10.7238 75.2124 7.41289C69.5422 4.10194 63.2754 1.94025 56.7698 1.05124C51.7666 0.367541 46.6976 0.446843 41.7345 1.27873C39.2613 1.69328 37.813 4.19778 38.4501 6.62326C39.0873 9.04874 41.5694 10.4717 44.0505 10.1071C47.8511 9.54855 51.7191 9.52689 55.5402 10.0491C60.8642 10.7766 65.9928 12.5457 70.6331 15.2552C75.2735 17.9648 79.3347 21.5619 82.5849 25.841C84.9175 28.9121 86.7997 32.2913 88.1811 35.8758C89.083 38.2158 91.5421 39.6781 93.9676 39.0409Z"
        fill="currentFill"
      />
    </svg>
    """
  end

  @doc """
  Returns the text in singular or plural depending on the quantity.

  ## Examples

    <.listing items={@packages}>
      <:item :let={package}><code><%= package.name %></code></:item>
      <:singular_suffix>package</:singular_suffix>
      <:plural_suffix>packages</:plural_suffix>
    </.listing>

  """

  attr :items, :list, required: true

  slot :item, required: true
  slot :plural_suffix
  slot :singular_suffix

  def listing(%{items: [_]} = assigns) do
    ~H"""
    <%= render_slot(@item, hd(@items)) %>
    <%= render_slot(@singular_suffix) %>
    """
  end

  def listing(%{items: [_, _ | _]} = assigns) do
    {items, assigns} = Map.pop!(assigns, :items)
    {leading, [second_to_last, last]} = Enum.split(items, -2)
    assigns = assign(assigns, leading: leading, second_to_last: second_to_last, last: last)

    ~H"""
    <%= for item <- @leading do %>
      <%= render_slot(@item, item) %>,
    <% end %>
    <%= render_slot(@item, @second_to_last) %> and <%= render_slot(@item, @last) %>
    <%= render_slot(@plural_suffix) %>
    """
  end

  @doc """
  Renders a table with generic styling.

  ## Examples

      <.table id="users" rows={@users}>
        <:col :let={user} label="id"><%= user.id %></:col>
        <:col :let={user} label="username"><%= user.username %></:col>
      </.table>

  """
  attr :id, :string, required: true
  attr :rows, :list, required: true
  attr :row_id, :any, default: nil, doc: "the function for generating the row id"
  attr :row_click, :any, default: nil, doc: "the function for handling phx-click on each row"

  attr :row_item, :any,
    default: &Function.identity/1,
    doc: "the function for mapping each row before calling the :col and :action slots"

  slot :col, required: true do
    attr :label, :string
  end

  slot :action, doc: "the slot for showing user actions in the last table column"

  def table(assigns) do
    assigns =
      with %{rows: %Phoenix.LiveView.LiveStream{}} <- assigns do
        assign(assigns, row_id: assigns.row_id || fn {id, _item} -> id end)
      end

    ~H"""
    <table class="overflow-y-auto px-4 sm:overflow-visible sm:px-0 min-w-full divide-y divide-gray-300">
      <thead class="text-sm text-left font-semibold leading-6 text-gray-900">
        <tr>
          <th :for={col <- @col} class="py-3.5 pl-4 pr-3 sm:pl-6">
            <%= col[:label] %>
          </th>
          <th :if={@action != []} class="py-3.5 pl-3 pr-5 text-right sm:pr-7">
            <span>Actions</span>
          </th>
        </tr>
      </thead>
      <tbody
        id={@id}
        phx-update={match?(%Phoenix.LiveView.LiveStream{}, @rows) && "stream"}
        class="relative divide-y divide-gray-200 bg-white text-sm font-medium leading-6 text-gray-900"
      >
        <tr :for={row <- @rows} id={@row_id && @row_id.(row)} class="hover:bg-gray-50">
          <td
            :for={col <- @col}
            phx-click={@row_click && @row_click.(row)}
            class={["relative p-0", @row_click && "hover:cursor-pointer"]}
          >
            <div class="relative block p-4 sm:px-6">
              <%= render_slot(col, @row_item.(row)) %>
            </div>
          </td>
          <td :if={@action != []} class="relative p-0">
            <div class="relative whitespace-nowrap py-4 pl-3 pr-4 sm:pr-6 flex justify-end items-center">
              <span :for={action <- @action} class="ml-4">
                <%= render_slot(action, @row_item.(row)) %>
              </span>
            </div>
          </td>
        </tr>
      </tbody>
    </table>
    """
  end

  @doc """
  Renders a button.

  ## Examples

      <.button>Click</.button>

      <.button color="gray" outlined>Click</.button>

      <.button color="gray" small>Click</.button>

  """
  attr :disabled, :boolean, default: false
  attr :color, :string, default: "blue", values: ~w(blue gray red)
  attr :outlined, :boolean, default: false
  attr :small, :boolean, default: false
  attr :class, :string, default: nil

  attr :rest, :global, include: ~w(href patch navigate download name)

  slot :inner_block

  def button(assigns)
      when is_map_key(assigns.rest, :href) or is_map_key(assigns.rest, :patch) or
             is_map_key(assigns.rest, :navigate) do
    ~H"""
    <.link class={[button_classes(@small, @disabled, @color, @outlined), @class]} {@rest}>
      <%= render_slot(@inner_block) %>
    </.link>
    """
  end

  def button(assigns) do
    ~H"""
    <button
      class={[button_classes(@small, @disabled, @color, @outlined), @class]}
      disabled={@disabled}
      {@rest}
    >
      <%= render_slot(@inner_block) %>
    </button>
    """
  end

  defp button_classes(small, disabled, color, outlined) do
    [
      if small do
        "px-2 py-1 font-normal text-xs"
      else
        "px-5 py-2 font-medium text-sm"
      end,
      "inline-flex rounded-lg border whitespace-nowrap items-center justify-center gap-1.5",
      if disabled do
        "cursor-default pointer-events-none border-transparent bg-gray-100 text-gray-400"
      else
        case {color, outlined} do
          {"blue", false} ->
            "border-transparent bg-blue-600 text-white hover:bg-blue-700 focus:bg-blue-700"

          {"red", false} ->
            "border-transparent bg-red-600 text-white hover:bg-red-700 focus:bg-red-700"

          {"gray", false} ->
            "border-gray-200 bg-gray-100 text-gray-600 hover:bg-gray-200 focus:bg-gray-200"

          {"blue", true} ->
            "bg-blue-50 border-blue-600 text-blue-600 hover:bg-blue-100 focus:bg-blue-100"

          {"red", true} ->
            "bg-red-50 border-red-600 text-red-600 hover:bg-red-100 focus:bg-red-100"

          {"gray", true} ->
            "bg-transparent border-gray-300 text-gray-600 hover:bg-gray-100 focus:bg-gray-100"
        end
      end
    ]
  end

  @doc """
  Renders an icon button.

  ## Examples

      <.icon_button>
        <.remix_icon icon="refresh-line" />
      </.icon_button>

  """
  attr :disabled, :boolean, default: false
  attr :small, :boolean, default: false
  attr :class, :string, default: nil

  attr :rest, :global, include: ~w(href patch navigate download name)

  slot :inner_block

  def icon_button(assigns)
      when is_map_key(assigns.rest, :href) or is_map_key(assigns.rest, :patch) or
             is_map_key(assigns.rest, :navigate) do
    ~H"""
    <.link class={[icon_button_classes(@small, @disabled), @class]} {@rest}>
      <%= render_slot(@inner_block) %>
    </.link>
    """
  end

  def icon_button(assigns) do
    ~H"""
    <button class={[icon_button_classes(@small, @disabled), @class]} disabled={@disabled} {@rest}>
      <%= render_slot(@inner_block) %>
    </button>
    """
  end

  defp icon_button_classes(small, disabled) do
    [
      unless small do
        "text-xl"
      end,
      "p-1 flex items-center justify-center rounded-full leading-none",
      if disabled do
        "cursor-default text-gray-300"
      else
        "text-gray-500 hover:text-gray-900 focus:bg-gray-100"
      end
    ]
  end

  @doc """
  Renders stateful tabs with content.

  ## Examples

      <.tabs id="animals" default="cat">
        <:tab id="cat" label="Cat">
          This is a cat.
        </:tab>
        <:tab id="dog" label="Dog">
          This is a dog.
        </:tab>
      </.tabs>

  """

  attr :id, :string, required: true
  attr :default, :string, required: true

  slot :tab do
    attr :id, :string, required: true
    attr :label, :string, required: true
  end

  def tabs(assigns) do
    ~H"""
    <div id={@id} class="flex flex-col gap-4">
      <div class="tabs">
        <button
          :for={tab <- @tab}
          class={["tab", @default == tab.id && "active"]}
          phx-click={
            JS.remove_class("active", to: "##{@id} .tab.active")
            |> JS.add_class("active")
            |> JS.add_class("hidden", to: "##{@id} [data-tab]")
            |> JS.remove_class("hidden", to: "##{@id} [data-tab='#{tab.id}']")
          }
        >
          <span class="font-medium">
            <%= tab.label %>
          </span>
        </button>
      </div>

      <div :for={tab <- @tab} data-tab={tab.id} class={@default == tab.id || "hidden"}>
        <%= render_slot(tab) %>
      </div>
    </div>
    """
  end

  # JS commands

  @doc """
  Pushes and executes the given `%Phoenix.LiveView.JS{}` on the client.

  ## Options

    * `:to` - selector for elements to execute against. Defaults to
      document body

  """
  def exec_js(socket, js, opts \\ []) do
    opts = Keyword.validate!(opts, [:to])

    Phoenix.LiveView.push_event(socket, "lb:exec_js", %{js: Jason.encode!(js.ops), to: opts[:to]})
  end

  @doc """
  Encodes value for hook prop attribute.

  ## Examples

      <div id="hook" phx-hook={MyHook} data-p-value={hook_prop(@value)}>
      </div>

  """
  def hook_prop(value)

  def hook_prop(%Phoenix.LiveComponent.CID{} = value) do
    hook_prop(to_string(value))
  end

  def hook_prop(value) do
    Jason.encode!(value)
  end
end
