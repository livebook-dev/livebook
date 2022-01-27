defmodule LivebookWeb.Helpers do
  use Phoenix.Component

  alias Phoenix.LiveView.JS

  alias LivebookWeb.Router.Helpers, as: Routes

  alias Livebook.FileSystem

  @doc """
  Wraps the given content in a modal dialog.

  When closed, the modal redirects to the given `:return_to` URL.

  ## Example

      <.modal return_to={...}>
        <.live_component module={MyComponent}  />
      </.modal>
  """
  def modal(assigns) do
    assigns =
      assigns
      |> assign_new(:class, fn -> "" end)

    ~H"""
    <div class="fixed z-[10000] inset-0 fade-in" phx-remove={JS.transition("fade-out")}>
      <!-- Modal container -->
      <div class="h-screen flex items-center justify-center p-4">
        <!-- Overlay -->
        <div class="absolute inset-0 bg-gray-500 opacity-75 z-0" aria-hidden="true"></div>

        <!-- Modal box -->
        <div class={"relative max-h-full overflow-y-auto bg-white rounded-lg shadow-xl #{@class}"}
          role="dialog"
          aria-modal="true"
          tabindex="0"
          autofocus
          phx-window-keydown={click_modal_close()}
          phx-click-away={click_modal_close()}
          phx-key="escape">

          <%= live_patch to: @return_to,
                class: "absolute top-6 right-6 text-gray-400 flex space-x-1 items-center",
                aria_label: "close modal",
                id: "close-modal-button" do %>
            <span class="text-sm">(esc)</span>
            <.remix_icon icon="close-line" class="text-2xl" />
          <% end %>

          <%= render_slot(@inner_block) %>
        </div>
      </div>
    </div>
    """
  end

  defp click_modal_close(js \\ %JS{}) do
    JS.dispatch(js, "click", to: "#close-modal-button")
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
end
