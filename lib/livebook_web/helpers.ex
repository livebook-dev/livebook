defmodule LivebookWeb.Helpers do
  use Phoenix.Component

  alias Phoenix.LiveView.JS

  alias LivebookWeb.Router.Helpers, as: Routes

  alias Livebook.FileSystem

  @doc """
  Wraps the given content in a modal dialog.

  ## Example

      <.modal id="edit-modal" patch={...}>
        <.live_component module={MyComponent}  />
      </.modal>
  """
  def modal(assigns) do
    assigns =
      assigns
      |> assign_new(:show, fn -> false end)
      |> assign_new(:patch, fn -> nil end)
      |> assign_new(:navigate, fn -> nil end)
      |> assign_new(:class, fn -> "" end)
      |> assign(:attrs, assigns_to_attributes(assigns, [:id, :show, :patch, :navigate, :class]))

    ~H"""
    <div id={@id} class={"fixed z-[10000] inset-0 #{if @show, do: "fade-in", else: "hidden"}"} phx-remove={JS.transition("fade-out")} {@attrs}>
      <!-- Modal container -->
      <div class="h-screen flex items-center justify-center p-4">
        <!-- Overlay -->
        <div class="absolute z-0 inset-0 bg-gray-500 opacity-75" aria-hidden="true"></div>
        <!-- Modal box -->
        <div class={"relative max-h-full overflow-y-auto bg-white rounded-lg shadow-xl #{@class}"}
          role="dialog"
          aria-modal="true"
          tabindex="0"
          autofocus
          phx-window-keydown={hide_modal(@id)}
          phx-click-away={hide_modal(@id)}
          phx-key="escape">
          <%= if @patch do %>
            <%= live_patch "", to: @patch, class: "hidden", id: "#{@id}-return" %>
            <% end %>
          <%= if @navigate do %>
            <%= live_redirect "", to: @navigate, class: "hidden", id: "#{@id}-return" %>
          <% end %>
          <button class="absolute top-6 right-6 text-gray-400 flex space-x-1 items-center"
            aria_label="close modal"
            phx-click={hide_modal(@id)}>
            <span class="text-sm">(esc)</span>
            <.remix_icon icon="close-line" class="text-2xl" />
          </button>
          <%= render_slot(@inner_block) %>
        </div>
      </div>
    </div>
    """
  end

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
  def confirm_modal(assigns) do
    # TODO: this ensures unique ids when navigating across LVs.
    # Remove once https://github.com/phoenixframework/phoenix_live_view/issues/1903
    # is resolved
    lv_id = self() |> :erlang.term_to_binary() |> Base.encode32()
    assigns = assign_new(assigns, :id, fn -> "confirm-modal-#{lv_id}" end)

    ~H"""
    <.modal id={@id} class="w-full max-w-xl" phx-hook="ConfirmModal" data-js-show={show_modal(@id)}>
      <div id={"#{@id}-content"} class="p-6 pb-4 flex flex-col" phx-update="ignore">
        <h3 class="text-2xl font-semibold text-gray-800" data-title></h3>
        <p class="mt-8 text-gray-700" data-description></p>
        <label class="mt-6 text-gray-700 flex items-center" data-opt-out>
          <input class="checkbox-base mr-3" type="checkbox" />
          <span class="text-sm">
            Don't show this message again
          </span>
        </label>
        <div class="mt-8 flex justify-end space-x-2">
          <button class="button-base button-red"
            phx-click={hide_modal(@id) |> JS.dispatch("lb:confirm", to: "##{@id}")}>
            <i aria-hidden="true" data-confirm-icon></i>
            <span data-confirm-text></span>
          </button>
          <button class="button-base button-outlined-gray"
            phx-click={hide_modal(@id)}>
            Cancel
          </button>
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
        [:confirm_icon, :description, :opt_out_id, title: "Are you sure?", confirm_text: "Yes"]
      )

    JS.dispatch(js, "lb:confirm_request",
      detail: %{
        on_confirm: Jason.encode!(on_confirm.ops),
        title: opts[:title],
        description: Keyword.fetch!(opts, :description),
        confirm_text: opts[:confirm_text],
        confirm_icon: opts[:confirm_icon],
        opt_out_id: opts[:opt_out_id]
      }
    )
  end

  @doc """
  Determines user platform based on the given *User-Agent* header.
  """
  @spec platform_from_user_agent(String.t()) :: :linux | :mac | :windows | :other
  def platform_from_user_agent(user_agent) when is_binary(user_agent) do
    cond do
      linux?(user_agent) -> :linux
      mac?(user_agent) -> :mac
      windows?(user_agent) -> :windows
      true -> :other
    end
  end

  defp linux?(user_agent), do: String.match?(user_agent, ~r/Linux/)
  defp mac?(user_agent), do: String.match?(user_agent, ~r/Mac OS X/)
  defp windows?(user_agent), do: String.match?(user_agent, ~r/Windows/)

  @doc """
  Returns path to specific process dialog within LiveDashboard.
  """
  def live_dashboard_process_path(socket, pid) do
    pid_str = Phoenix.LiveDashboard.PageBuilder.encode_pid(pid)
    Routes.live_dashboard_path(socket, :page, node(), "processes", info: pid_str)
  end

  @doc """
  Converts human-readable strings to strings which can be used
  as HTML element IDs (compatible with HTML5)

  At the same time duplicate IDs are enumerated to avoid duplicates
  """
  @spec names_to_html_ids(list(String.t())) :: list(String.t())
  def names_to_html_ids(names) do
    names
    |> Enum.map(&name_to_html_id/1)
    |> Enum.map_reduce(%{}, fn html_id, counts ->
      counts = Map.update(counts, html_id, 1, &(&1 + 1))

      case counts[html_id] do
        1 -> {html_id, counts}
        count -> {"#{html_id}-#{count}", counts}
      end
    end)
    |> elem(0)
  end

  defp name_to_html_id(name) do
    name
    |> String.trim()
    |> String.downcase()
    |> String.replace(~r/\s+/u, "-")
  end

  @doc """
  Renders [Remix](https://remixicon.com) icon.

  ## Examples

      <.remix_icon icon="cpu-line" />

      <.remix_icon icon="cpu-line" class="align-middle mr-1" />
  """
  def remix_icon(assigns) do
    assigns =
      assigns
      |> assign_new(:class, fn -> "" end)
      |> assign(:attrs, assigns_to_attributes(assigns, [:icon, :class]))

    ~H"""
    <i class={"ri-#{@icon} #{@class}"} aria-hidden="true" {@attrs}></i>
    """
  end

  @doc """
  Renders a list of select input options with the given one selected.

  ## Examples

      <.select
        name="language"
        selected={@language}
        options={[en: "English", pl: "Polski", fr: "Français"]} />
  """
  def select(assigns) do
    ~H"""
    <select class="input" name={@name}>
      <%= for {value, label} <- @options do %>
        <option value={value} selected={value == @selected}>
          <%= label %>
        </option>
      <% end %>
    </select>
    """
  end

  @doc """
  Renders a checkbox input styled as a switch.

  Also, a hidden input with the same name is rendered
  alongside the checkbox, so the submitted value is
  always either `"true"` or `"false"`.

  ## Examples

      <.switch_checkbox
        name="likes_cats"
        label="I very much like cats"
        checked={@likes_cats} />
  """
  def switch_checkbox(assigns) do
    assigns =
      assigns
      |> assign_new(:label, fn -> nil end)
      |> assign_new(:disabled, fn -> false end)
      |> assign_new(:class, fn -> "" end)
      |> assign(
        :attrs,
        assigns_to_attributes(assigns, [:label, :name, :checked, :disabled, :class])
      )

    ~H"""
    <div class="flex space-x-3 items-center justify-between">
      <%= if @label do %>
        <span class="text-gray-700"><%= @label %></span>
      <% end %>
      <label class={"switch-button #{if(@disabled, do: "switch-button--disabled")}"}>
        <input type="hidden" value="false" name={@name} />
        <input
          type="checkbox"
          value="true"
          class={"switch-button__checkbox #{@class}"}
          name={@name}
          checked={@checked}
          {@attrs} />
        <div class="switch-button__bg"></div>
      </label>
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
  def choice_button(assigns) do
    assigns =
      assigns
      |> assign_new(:class, fn -> "" end)
      |> assign_new(:disabled, fn -> assigns.active end)
      |> assign(:attrs, assigns_to_attributes(assigns, [:active, :class, :disabled]))

    ~H"""
    <button class={"choice-button #{if(@active, do: "active")} #{@class}"} disabled={@disabled} {@attrs}>
      <%= render_slot(@inner_block) %>
    </button>
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
  def code_preview(assigns) do
    ~H"""
    <div class="markdown">
      <pre><code
        class="tiny-scrollbar"
        id={"#{@source_id}-highlight"}
        phx-hook="Highlight"
        data-language={@language}><div id={@source_id} data-source><%= @source %></div><div data-target></div></code></pre>
    </div>
    """
  end

  @doc """
  Renders text with a tiny label.

  ## Examples

      <.labeled_text label="Name" text="Sherlock Holmes" />
  """
  def labeled_text(assigns) do
    assigns = assign_new(assigns, :one_line, fn -> false end)

    ~H"""
    <div class="flex flex-col space-y-1">
      <span class="text-sm text-gray-500">
        <%= @label %>
      </span>
      <span class={"text-gray-800 text-sm font-semibold #{if @one_line, do: "whitespace-nowrap overflow-auto tiny-scrollbar"}"}>
        <%= @text %>
      </span>
    </div>
    """
  end

  @doc """
  Renders a wrapper around password input
  with an added visibility toggle button.

  The toggle switches the input's type between `password`
  and `text`.

  ## Examples

      <.with_password_toggle id="input-id">
        <input type="password" ...>
      </.with_password_toggle>
  """
  def with_password_toggle(assigns) do
    ~H"""
    <div id={"password-toggle-#{@id}"} class="relative inline w-min" phx-hook="PasswordToggle">
      <!-- render password input -->
      <%= render_slot(@inner_block) %>
      <button
        class="bg-gray-50 p-1 icon-button absolute inset-y-0 right-1"
        type="button"
        aria-label="toggle password visibility"
        phx-change="ignore">
        <.remix_icon icon="eye-line" class="text-xl" />
      </button>
    </div>
    """
  end

  @doc """
  Renders a popup menu that shows up on toggle click.

  ## Assigns

    * `:id` - unique HTML id

    * `:disabled` - whether the menu is active. Defaults to `false`

    * `:position` - which side of the clickable the menu menu should
      be attached to, either `"left"` or `"right"`. Defaults to `"right"`

    * `:secondary_click` - whether secondary click (usually right mouse click)
      should open the menu. Defaults to `false`

  ## Examples

      <.menu id="my-menu">
        <:toggle>
          <button>Open</button>
        </:toggle>
        <:content>
          <button class"menu-item" role="menuitem">Option 1</button>
        </:content>
      </.menu>
  """
  def menu(assigns) do
    assigns =
      assigns
      |> assign_new(:disabled, fn -> false end)
      |> assign_new(:position, fn -> "right" end)
      |> assign_new(:secondary_click, fn -> false end)

    ~H"""
    <div class="relative"
      id={@id}>
      <div
        phx-click={not @disabled && JS.toggle(to: "##{@id}-content")}
        phx-click-away={JS.hide(to: "##{@id}-content")}
        data-contextmenu-trigger-click={@secondary_click}
        phx-window-keydown={JS.hide(to: "##{@id}-content")}
        phx-key="escape">
        <%= render_slot(@toggle) %>
      </div>
      <menu id={"#{@id}-content"} class={"hidden menu #{@position}"} role="menu">
        <%= render_slot(@content) %>
      </menu>
    </div>
    """
  end

  defdelegate ansi_string_to_html(string), to: LivebookWeb.Helpers.ANSI
  defdelegate ansi_string_to_html_lines(string), to: LivebookWeb.Helpers.ANSI

  @doc """
  Renders an icon representing the given file system.
  """
  def file_system_icon(assigns)

  def file_system_icon(%{file_system: %FileSystem.Local{}} = assigns) do
    ~H"""
    <.remix_icon icon="hard-drive-2-line leading-none" />
    """
  end

  def file_system_icon(%{file_system: %FileSystem.S3{}} = assigns) do
    ~H"""
    <i class="not-italic">
      <span class="text-[0.75em] font-semibold align-middle">S3</span>
    </i>
    """
  end

  @doc """
  Formats the given file system into a descriptive label.
  """
  def file_system_label(file_system)

  def file_system_label(%FileSystem.Local{}), do: "Local disk"
  def file_system_label(%FileSystem.S3{} = fs), do: fs.bucket_url

  @doc """
  Returns the text in singular or plural depending on the quantity

  ## Examples

      iex> LivebookWeb.Helpers.pluralize(1, "notebook is not persisted", "notebooks are not persisted")
      "1 notebook is not persisted"

      iex> LivebookWeb.Helpers.pluralize(3, "notebook is not persisted", "notebooks are not persisted")
      "3 notebooks are not persisted"
  """
  def pluralize(1, singular, _plural), do: "1 #{singular}"
  def pluralize(count, _singular, plural), do: "#{count} #{plural}"

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
end
