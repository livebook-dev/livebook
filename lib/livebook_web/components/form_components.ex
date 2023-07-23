defmodule LivebookWeb.FormComponents do
  use Phoenix.Component

  import LivebookWeb.CoreComponents

  alias Phoenix.LiveView.JS

  @doc """
  Renders a text input with label and error messages.
  """
  attr :id, :any, default: nil
  attr :name, :any
  attr :label, :string, default: nil
  attr :value, :any
  attr :errors, :list, default: []
  attr :field, Phoenix.HTML.FormField, doc: "a form field struct retrieved from the form"
  attr :help, :string, default: nil
  attr :class, :string, default: nil

  attr :rest, :global, include: ~w(autocomplete readonly disabled)

  def text_field(assigns) do
    assigns = assigns_from_field(assigns)

    ~H"""
    <.field_wrapper id={@id} name={@name} label={@label} errors={@errors} help={@help}>
      <input
        type="text"
        name={@name}
        id={@id || @name}
        value={Phoenix.HTML.Form.normalize_value("text", @value)}
        class={["input", @class]}
        {@rest}
      />
    </.field_wrapper>
    """
  end

  @doc """
  Renders a textarea input with label and error messages.
  """
  attr :id, :any, default: nil
  attr :name, :any
  attr :label, :string, default: nil
  attr :value, :any
  attr :errors, :list, default: []
  attr :field, Phoenix.HTML.FormField, doc: "a form field struct retrieved from the form"
  attr :help, :string, default: nil

  attr :resizable, :boolean, default: false

  attr :rest, :global, include: ~w(autocomplete readonly disabled rows cols)

  def textarea_field(assigns) do
    assigns = assigns_from_field(assigns)

    ~H"""
    <.field_wrapper id={@id} name={@name} label={@label} errors={@errors} help={@help}>
      <textarea
        id={@id || @name}
        name={@name}
        class={["input", not @resizable && "resize-none"]}
        {@rest}
      ><%= Phoenix.HTML.Form.normalize_value("textarea", @value) %></textarea>
    </.field_wrapper>
    """
  end

  @doc """
  Renders a hidden input.
  """
  attr :id, :any, default: nil
  attr :name, :any
  attr :value, :any
  attr :field, Phoenix.HTML.FormField, doc: "a form field struct retrieved from the form"

  attr :rest, :global, include: ~w(autocomplete readonly disabled)

  def hidden_field(assigns) do
    assigns = assigns_from_field(assigns)

    ~H"""
    <input type="hidden" name={@name} id={@id || @name} value={@value} {@rest} />
    """
  end

  @doc """
  Renders a password input with label and error messages.
  """
  attr :id, :any, default: nil
  attr :name, :any
  attr :label, :string, default: nil
  attr :value, :any
  attr :errors, :list, default: []
  attr :field, Phoenix.HTML.FormField, doc: "a form field struct retrieved from the form"
  attr :help, :string, default: nil
  attr :class, :string, default: nil

  attr :rest, :global, include: ~w(autocomplete readonly disabled)

  def password_field(assigns) do
    assigns = assigns_from_field(assigns)

    ~H"""
    <.field_wrapper id={@id} name={@name} label={@label} errors={@errors} help={@help}>
      <.with_password_toggle id={@id <> "-toggle"}>
        <input
          type="password"
          name={@name}
          id={@id || @name}
          value={Phoenix.HTML.Form.normalize_value("text", @value)}
          class={["input pr-8", @class]}
          {@rest}
        />
      </.with_password_toggle>
    </.field_wrapper>
    """
  end

  @doc """
  Renders a hex color input with label and error messages.
  """
  attr :id, :any, default: nil
  attr :name, :any
  attr :label, :string, default: nil
  attr :value, :any
  attr :errors, :list, default: []
  attr :field, Phoenix.HTML.FormField, doc: "a form field struct retrieved from the form"
  attr :help, :string, default: nil

  attr :randomize, JS, default: %JS{}
  attr :rest, :global

  def hex_color_field(assigns) do
    assigns = assigns_from_field(assigns)

    ~H"""
    <.field_wrapper id={@id} name={@name} label={@label} errors={@errors} help={@help}>
      <div class="flex space-x-4 items-center">
        <div
          class="border-[3px] rounded-lg p-1 flex justify-center items-center"
          style={"border-color: #{@value}"}
        >
          <div class="rounded h-5 w-5" style={"background-color: #{@value}"}></div>
        </div>
        <div class="relative grow">
          <input
            type="text"
            name={@name}
            id={@id || @name}
            value={@value}
            class="input"
            spellcheck="false"
            maxlength="7"
            {@rest}
          />
          <button class="icon-button absolute right-2 top-1" type="button" phx-click={@randomize}>
            <.remix_icon icon="refresh-line" class="text-xl" />
          </button>
        </div>
      </div>
    </.field_wrapper>
    """
  end

  @doc """
  Renders a switch input with label and error messages.
  """
  attr :id, :any, default: nil
  attr :name, :any
  attr :label, :string, default: nil
  attr :value, :any
  attr :errors, :list, default: []
  attr :field, Phoenix.HTML.FormField, doc: "a form field struct retrieved from the form"
  attr :help, :string, default: nil

  attr :disabled, :boolean, default: false
  attr :checked_value, :string, default: "true"
  attr :unchecked_value, :string, default: "false"

  attr :rest, :global

  def switch_field(assigns) do
    assigns = assigns_from_field(assigns)

    ~H"""
    <div phx-feedback-for={@name} class={[@errors != [] && "show-errors"]}>
      <div class="flex items-center gap-1 sm:gap-3 justify-between">
        <span :if={@label} class="text-gray-700 flex gap-1 items-center">
          <%= @label %>
          <.help :if={@help} text={@help} />
        </span>
        <label class={[
          "relative inline-block w-14 h-7 select-none",
          @disabled && "pointer-events-none opacity-50"
        ]}>
          <input type="hidden" value={@unchecked_value} name={@name} />
          <input
            type="checkbox"
            value={@checked_value}
            class={[
              "appearance-none absolute block w-7 h-7 rounded-full bg-white border-[5px] border-gray-200 cursor-pointer transition-all duration-300",
              "peer checked:bg-white checked:border-blue-600 checked:translate-x-full"
            ]}
            name={@name}
            id={@id || @name}
            checked={to_string(@value) == @checked_value}
            {@rest}
          />
          <div class={[
            "block h-full w-full rounded-full bg-gray-200 cursor-pointer transition-all duration-300",
            "peer-checked:bg-blue-600"
          ]}>
          </div>
        </label>
      </div>
      <.error :for={msg <- @errors}><%= msg %></.error>
    </div>
    """
  end

  @doc """
  Renders checkbox input with label and error messages.
  """
  attr :id, :any, default: nil
  attr :name, :any
  attr :label, :string, default: nil
  attr :value, :any
  attr :errors, :list, default: []
  attr :field, Phoenix.HTML.FormField, doc: "a form field struct retrieved from the form"
  attr :help, :string, default: nil

  attr :checked_value, :string, default: "true"

  attr :unchecked_value, :any,
    default: "false",
    doc: "when set to `nil`, unchecked value is not sent"

  attr :rest, :global

  def checkbox_field(assigns) do
    assigns = assigns_from_field(assigns)

    ~H"""
    <div phx-feedback-for={@name} class={[@errors != [] && "show-errors"]}>
      <label class="flex items-center gap-2 cursor-pointer">
        <input :if={@unchecked_value} type="hidden" value={@unchecked_value} name={@name} />
        <input
          type="checkbox"
          class="checkbox shrink-0"
          value={@checked_value}
          name={@name}
          id={@id || @name}
          checked={to_string(@value) == @checked_value}
          {@rest}
        />
        <span :if={@label} class="text-gray-700 flex gap-1 items-center">
          <%= @label %>
          <.help :if={@help} text={@help} />
        </span>
      </label>
      <.error :for={msg <- @errors}><%= msg %></.error>
    </div>
    """
  end

  @doc """
  Renders radio inputs with label and error messages.
  """
  attr :id, :any, default: nil
  attr :name, :any
  attr :label, :string, default: nil
  attr :value, :any
  attr :errors, :list, default: []
  attr :field, Phoenix.HTML.FormField, doc: "a form field struct retrieved from the form"
  attr :help, :string, default: nil

  attr :options, :list, default: [], doc: "a list of `{value, description}` tuples"

  attr :rest, :global

  def radio_field(assigns) do
    assigns = assigns_from_field(assigns)

    ~H"""
    <div phx-feedback-for={@name} class={[@errors != [] && "show-errors"]}>
      <.label :if={@label} for={@id} help={@help}><%= @label %></.label>
      <div class="flex gap-4 text-gray-600">
        <label :for={{value, description} <- @options} class="flex items-center gap-2 cursor-pointer">
          <input
            type="radio"
            class="radio"
            name={@name}
            value={value}
            checked={to_string(@value) == value}
            {@rest}
          />
          <span><%= description %></span>
        </label>
      </div>
      <.error :for={msg <- @errors}><%= msg %></.error>
    </div>
    """
  end

  @doc """
  Renders radio inputs presented with label and error messages presented
  as button group.
  """
  attr :id, :any, default: nil
  attr :name, :any
  attr :label, :string, default: nil
  attr :value, :any
  attr :errors, :list, default: []
  attr :field, Phoenix.HTML.FormField, doc: "a form field struct retrieved from the form"
  attr :help, :string, default: nil

  attr :options, :list, default: [], doc: "a list of `{value, description}` tuples"
  attr :full_width, :boolean, default: false

  attr :rest, :global

  def radio_button_group_field(assigns) do
    assigns = assigns_from_field(assigns)

    ~H"""
    <div phx-feedback-for={@name} class={[@errors != [] && "show-errors"]}>
      <.label :if={@label} for={@id} help={@help}><%= @label %></.label>
      <div class="flex">
        <label
          :for={{value, description} <- @options}
          class={[
            @full_width && "flex-grow text-center",
            "px-3 py-2 first:rounded-l-lg last:rounded-r-lg font-medium text-sm whitespace-nowrap cursor-pointer",
            "border border-r-0 last:border-r border-gray-500",
            if(to_string(@value) == value,
              do: "text-gray-50 bg-gray-500",
              else: "text-gray-500 hover:bg-gray-100 focus:bg-gray-100"
            )
          ]}
        >
          <input
            type="radio"
            class="hidden"
            name={@name}
            value={value}
            checked={to_string(@value) == value}
            {@rest}
          />
          <span><%= description %></span>
        </label>
      </div>
      <.error :for={msg <- @errors}><%= msg %></.error>
    </div>
    """
  end

  @doc """
  Renders emoji input with label and error messages.
  """
  attr :id, :any, default: nil
  attr :name, :any
  attr :label, :string, default: nil
  attr :value, :any
  attr :errors, :list, default: []
  attr :field, Phoenix.HTML.FormField, doc: "a form field struct retrieved from the form"
  attr :help, :string, default: nil

  attr :rest, :global

  def emoji_field(assigns) do
    assigns = assigns_from_field(assigns)

    ~H"""
    <.field_wrapper id={@id} name={@name} label={@label} errors={@errors} help={@help}>
      <div class="flex border bg-gray-50 rounded-lg space-x-4 items-center">
        <div id={"#{@id}-picker"} class="flex w-full" phx-hook="EmojiPicker">
          <div class="grow p-1 pl-3">
            <span id={"#{@id}-preview"} data-emoji-preview><%= @value %></span>
          </div>
          <button
            id={"#{@id}-button"}
            type="button"
            data-emoji-button
            class="p-1 pl-3 pr-3 rounded-tr-lg rounded-br-lg bg-gray-50 hover:bg-gray-100 active:bg-gray-200 border-l-[1px] bg-white flex justify-center items-center cursor-pointer"
          >
            <.remix_icon icon="emotion-line" class="text-xl" />
          </button>
          <input
            type="hidden"
            name={@name}
            id={@id || @name}
            value={@value}
            class="hidden emoji-picker-input"
            data-emoji-input
          />
        </div>
      </div>
    </.field_wrapper>
    """
  end

  @doc """
  Renders select input with label and error messages.
  """
  attr :id, :any, default: nil
  attr :name, :any
  attr :label, :string, default: nil
  attr :value, :any
  attr :errors, :list, default: []
  attr :field, Phoenix.HTML.FormField, doc: "a form field struct retrieved from the form"
  attr :help, :string, default: nil

  attr :options, :list, default: []
  attr :prompt, :string, default: nil

  attr :rest, :global

  def select_field(assigns) do
    assigns = assigns_from_field(assigns)

    ~H"""
    <.field_wrapper id={@id} name={@name} label={@label} errors={@errors} help={@help}>
      <select id={@id} name={@name} class="input" {@rest}>
        <option :if={@prompt} value="" disabled selected><%= @prompt %></option>
        <%= Phoenix.HTML.Form.options_for_select(@options, @value) %>
      </select>
    </.field_wrapper>
    """
  end

  defp assigns_from_field(%{field: %Phoenix.HTML.FormField{} = field} = assigns) do
    assigns
    |> assign(field: nil, id: assigns.id || field.id)
    |> assign(:errors, Enum.map(field.errors, &translate_error/1))
    |> assign_new(:name, fn -> field.name end)
    |> assign_new(:value, fn -> field.value end)
  end

  defp assigns_from_field(assigns), do: assigns

  @doc """
  Translates an error message using gettext.
  """
  def translate_error({msg, opts}) do
    # Because the error messages we show in our forms and APIs
    # are defined inside Ecto, we need to translate them dynamically.
    Enum.reduce(opts, msg, fn {key, value}, acc ->
      String.replace(acc, "%{#{key}}", fn _ -> to_string(value) end)
    end)
  end

  attr :id, :any, required: true
  attr :name, :any, required: true
  attr :label, :string, required: true
  attr :errors, :list, required: true
  attr :help, :string, required: true
  slot :inner_block, required: true

  defp field_wrapper(assigns) do
    ~H"""
    <div phx-feedback-for={@name} class={[@errors != [] && "show-errors"]}>
      <.label :if={@label} for={@id} help={@help}><%= @label %></.label>
      <%= render_slot(@inner_block) %>
      <.error :for={msg <- @errors}><%= msg %></.error>
    </div>
    """
  end

  @doc """
  Renders a label.
  """
  attr :for, :string, default: nil
  attr :help, :string, default: nil
  slot :inner_block, required: true

  def label(assigns) do
    ~H"""
    <label for={@for} class="mb-1 block text-sm text-gray-800 font-medium flex items-center gap-1">
      <%= render_slot(@inner_block) %>
      <.help :if={@help} text={@help} />
    </label>
    """
  end

  @doc """
  Generates a generic error message.
  """
  slot :inner_block, required: true

  def error(assigns) do
    ~H"""
    <p class="mt-0.5 text-red-600 text-sm hidden phx-form-error:block">
      <%= render_slot(@inner_block) %>
    </p>
    """
  end

  defp help(assigns) do
    ~H"""
    <span class="cursor-pointer tooltip top" data-tooltip={@text}>
      <.remix_icon icon="question-line" class="text-sm leading-none" />
    </span>
    """
  end

  @doc """
  Renders a wrapper around password input with an added visibility
  toggle button.

  The toggle switches the input's type between `password` and `text`.

  ## Examples

      <.with_password_toggle id="secret-password-toggle">
        <input type="password" ...>
      </.with_password_toggle>

  """
  attr :id, :string, required: true

  slot :inner_block, required: true

  def with_password_toggle(assigns) do
    ~H"""
    <div id={@id} class="relative flex">
      <%= render_slot(@inner_block) %>
      <div class="flex items-center absolute inset-y-0 right-1">
        <button
          class="icon-button"
          data-show
          type="button"
          aria-label="show password"
          phx-click={
            JS.remove_attribute("type", to: "##{@id} input")
            |> JS.set_attribute({"type", "text"}, to: "##{@id} input")
            |> JS.add_class("hidden", to: "##{@id} [data-show]")
            |> JS.remove_class("hidden", to: "##{@id} [data-hide]")
          }
        >
          <.remix_icon icon="eye-line" class="text-xl" />
        </button>
        <button
          class="icon-button hidden"
          data-hide
          type="button"
          aria-label="hide password"
          phx-click={
            JS.remove_attribute("type", to: "##{@id} input")
            |> JS.set_attribute({"type", "password"}, to: "##{@id} input")
            |> JS.remove_class("hidden", to: "##{@id} [data-show]")
            |> JS.add_class("hidden", to: "##{@id} [data-hide]")
          }
        >
          <.remix_icon icon="eye-off-line" class="text-xl" />
        </button>
      </div>
    </div>
    """
  end

  @doc """
  Renders a drag-and-drop area for the given upload.

  Once a file is selected, renders the entry.

  ## Examples

      <.file_drop_input
        upload={@uploads.file}
        label="File"
        on_clear={JS.push("clear_file", target: @myself)}
      />

  """
  attr :upload, Phoenix.LiveView.UploadConfig, required: true
  attr :label, :string, required: true
  attr :on_clear, Phoenix.LiveView.JS, required: true

  def file_drop_input(%{upload: %{entries: []}} = assigns) do
    ~H"""
    <div>
      <.live_file_input upload={@upload} class="hidden" />
      <label
        class="flex flex-col justify-center items-center w-full rounded-xl border-2 border-dashed border-gray-400 h-48 cursor-pointer"
        phx-hook="Dropzone"
        id={"#{@upload.ref}-dropzone"}
        for={@upload.ref}
        phx-drop-target={@upload.ref}
      >
        <span class="font-medium text-gray-400">
          Click to select a file or drag a local file here
        </span>
      </label>
    </div>
    """
  end

  def file_drop_input(assigns) do
    ~H"""
    <div>
      <.live_file_input upload={@upload} class="hidden" />
      <.label><%= @label %></.label>
      <div :for={entry <- @upload.entries} class="flex flex-col gap-1">
        <div class="flex flex-col gap-0.5">
          <div class="flex items-center justify-between text-gray-700">
            <span><%= entry.client_name %></span>
            <button
              type="button"
              class="ml-1 text-gray-500 hover:text-gray-900"
              phx-click={@on_clear}
              phx-value-ref={entry.ref}
              tabindex="-1"
            >
              <.remix_icon icon="close-line" />
            </button>
            <span class="flex-grow"></span>
            <span :if={entry.preflighted?} class="text-sm font-medium">
              <%= entry.progress %>%
            </span>
          </div>
          <div :if={entry.preflighted?} class="w-full h-2 rounded-lg bg-blue-200">
            <div
              class="h-full rounded-lg bg-blue-600 transition-all ease-out duration-1000"
              style={"width: #{entry.progress}%"}
            >
            </div>
          </div>
        </div>
      </div>
    </div>
    """
  end

  @doc """
  Checks if the given upload makes the form disabled.
  """
  @spec upload_disabled?(Phoenix.LiveView.UploadConfig.t()) :: boolean()
  def upload_disabled?(upload) do
    upload.entries == [] or upload.errors != [] or Enum.any?(upload.entries, & &1.preflighted?)
  end
end
