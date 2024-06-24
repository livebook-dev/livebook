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
  attr :type, :string, default: "text"
  attr :class, :string, default: nil

  attr :rest, :global, include: ~w(autocomplete readonly disabled step min max)

  def text_field(assigns) do
    assigns = assigns_from_field(assigns)

    ~H"""
    <.field_wrapper id={@id} name={@name} label={@label} errors={@errors} help={@help}>
      <input
        type={@type}
        name={@name}
        id={@id || @name}
        value={Phoenix.HTML.Form.normalize_value("text", @value)}
        class={[input_classes(@errors), @class]}
        {@rest}
      />
    </.field_wrapper>
    """
  end

  defp input_classes(errors) do
    [
      "w-full px-3 py-2 text-sm font-normal border rounded-lg placeholder-gray-400 disabled:opacity-70 disabled:cursor-not-allowed",
      if errors == [] do
        "bg-gray-50 border-gray-200 text-gray-600"
      else
        "bg-red-50 border-red-600 text-red-600"
      end,
      "invalid:bg-red-50 invalid:border-red-600 invalid:text-red-600"
    ]
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
  attr :class, :string, default: nil

  attr :monospace, :boolean, default: false

  attr :rest, :global, include: ~w(autocomplete readonly disabled rows cols)

  def textarea_field(assigns) do
    assigns = assigns_from_field(assigns)

    ~H"""
    <.field_wrapper id={@id} name={@name} label={@label} errors={@errors} help={@help}>
      <textarea
        id={@id || @name}
        name={@name}
        class={[
          input_classes(@errors),
          "resize-none tiny-scrollbar",
          @monospace && "font-mono",
          @class
        ]}
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
      <div id={"#{@id}-toggle"} class="relative flex">
        <input
          type="password"
          name={@name}
          id={@id || @name}
          value={Phoenix.HTML.Form.normalize_value("text", @value)}
          class={[input_classes(@errors), "pr-8", @class]}
          {@rest}
        />
        <div class="flex items-center absolute inset-y-0 right-1">
          <.icon_button
            data-show
            type="button"
            aria-label="show password"
            tabindex="-1"
            phx-click={
              JS.set_attribute({"type", "text"}, to: "##{@id}-toggle input")
              |> JS.add_class("hidden", to: "##{@id}-toggle [data-show]")
              |> JS.remove_class("hidden", to: "##{@id}-toggle [data-hide]")
            }
          >
            <.remix_icon icon="eye-line" />
          </.icon_button>
          <.icon_button
            class="hidden"
            data-hide
            type="button"
            aria-label="hide password"
            tabindex="-1"
            phx-click={
              JS.set_attribute({"type", "password"}, to: "##{@id}-toggle input")
              |> JS.remove_class("hidden", to: "##{@id}-toggle [data-show]")
              |> JS.add_class("hidden", to: "##{@id}-toggle [data-hide]")
            }
          >
            <.remix_icon icon="eye-off-line" />
          </.icon_button>
        </div>
      </div>
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
            class={input_classes(@errors)}
            spellcheck="false"
            maxlength="7"
            {@rest}
          />
          <div class="absolute right-2 top-1">
            <.icon_button type="button" phx-click={@randomize}>
              <.remix_icon icon="refresh-line" />
            </.icon_button>
          </div>
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
    <div>
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

  attr :small, :boolean, default: false

  attr :rest, :global

  def checkbox_field(assigns) do
    assigns = assigns_from_field(assigns)

    ~H"""
    <div>
      <label class="flex items-center gap-2 cursor-pointer">
        <input :if={@unchecked_value} type="hidden" value={@unchecked_value} name={@name} />
        <input
          type="checkbox"
          class="peer hidden"
          value={@checked_value}
          name={@name}
          id={@id || @name}
          checked={to_string(@value) == @checked_value}
          {@rest}
        />
        <div class="w-5 h-5 flex items-center justify-center border border-gray-300 peer-checked:border-transparent bg-white peer-checked:bg-blue-600 rounded">
          <svg viewBox="0 0 16 16" fill="white" xmlns="http://www.w3.org/2000/svg">
            <path d="M12.207 4.793a1 1 0 010 1.414l-5 5a1 1 0 01-1.414 0l-2-2a1 1 0 011.414-1.414L6.5 9.086l4.293-4.293a1 1 0 011.414 0z" />
          </svg>
        </div>
        <span :if={@label} class={["text-gray-700 flex gap-1 items-center", @small && "text-sm"]}>
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
    <div>
      <.label :if={@label} for={@id} help={@help}><%= @label %></.label>
      <div class="flex gap-4 text-gray-600">
        <label :for={{value, description} <- @options} class="flex items-center gap-2 cursor-pointer">
          <input
            type="radio"
            class="peer hidden"
            name={@name}
            value={value}
            checked={to_string(@value) == value}
            {@rest}
          />
          <div class="w-5 h-5 flex items-center justify-center border border-gray-300 peer-checked:border-blue-600 text-white peer-checked:text-blue-600 bg-white rounded-full">
            <div class="w-3 h-3 rounded-full bg-current"></div>
          </div>
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
    <div>
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
            class="p-1 pl-3 pr-3 rounded-tr-lg rounded-br-lg bg-gray-50 hover:bg-gray-100 active:bg-gray-200 border-l-[1px] flex justify-center items-center cursor-pointer"
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
  attr :class, :string, default: ""
  attr :field, Phoenix.HTML.FormField, doc: "a form field struct retrieved from the form"
  attr :help, :string, default: nil

  attr :options, :list, default: []
  attr :prompt, :string, default: nil

  attr :rest, :global, include: ~w(disabled)

  def select_field(assigns) do
    assigns = assigns_from_field(assigns)

    ~H"""
    <.field_wrapper id={@id} name={@name} label={@label} errors={@errors} help={@help}>
      <div class="block relative">
        <select
          id={@id}
          name={@name}
          class={[
            "w-full px-3 py-2 pr-7 appearance-none bg-gray-50 text-sm border rounded-lg placeholder-gray-400 text-gray-600 disabled:opacity-70 disabled:cursor-not-allowed",
            if(@errors == [], do: "border-gray-200", else: "border-red-300"),
            @class
          ]}
          {@rest}
        >
          <option :if={@prompt} value=""><%= @prompt %></option>
          <%!-- TODO: we use to_string to normalize nil and "", remove
                this once fixed upstream https://github.com/phoenixframework/phoenix_html/issues/444 --%>
          <%= Phoenix.HTML.Form.options_for_select(@options, to_string(@value)) %>
        </select>
        <div class="pointer-events-none absolute inset-y-0 right-0 flex items-center px-2 text-gray-500">
          <.remix_icon icon="arrow-down-s-line" />
        </div>
      </div>
    </.field_wrapper>
    """
  end

  defp assigns_from_field(%{field: %Phoenix.HTML.FormField{} = field} = assigns) do
    errors = if Phoenix.Component.used_input?(field), do: field.errors, else: []

    assigns
    |> assign(field: nil, id: assigns.id || field.id)
    |> assign(:errors, Enum.map(errors, &translate_error(&1)))
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
    <div>
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
    <label for={@for} class="mb-1 flex items-center gap-1 text-sm text-gray-800 font-medium">
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
    <p class="mt-0.5 text-red-600 text-sm">
      <%= render_slot(@inner_block) %>
    </p>
    """
  end

  defp help(assigns) do
    ~H"""
    <span class="cursor-pointer tooltip right" data-tooltip={@text}>
      <.remix_icon icon="question-line" class="text-sm leading-none" />
    </span>
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
        <.file_entry entry={entry} on_clear={@on_clear} />
      </div>
    </div>
    """
  end

  @doc """
  Renders a file entry with progress.

  ## Examples

      <.file_entry
        entry={entry}
        on_clear={JS.push("clear_file", target: @myself)}
      />

  """
  attr :entry, Phoenix.LiveView.UploadEntry, required: true
  attr :on_clear, Phoenix.LiveView.JS, required: true
  attr :name, :string, default: nil

  def file_entry(assigns) do
    ~H"""
    <div class="flex flex-col gap-0.5">
      <div class="flex items-center justify-between gap-1 text-gray-700">
        <span><%= @name || @entry.client_name %></span>
        <button
          type="button"
          class="text-gray-500 hover:text-gray-900"
          phx-click={@on_clear}
          phx-value-ref={@entry.ref}
          tabindex="-1"
        >
          <.remix_icon icon="close-line" />
        </button>
        <span class="flex-grow"></span>
        <span :if={@entry.preflighted?} class="text-sm font-medium">
          <%= @entry.progress %>%
        </span>
      </div>
      <div :if={@entry.preflighted?} class="w-full h-2 rounded-lg bg-blue-200">
        <div
          class="h-full rounded-lg bg-blue-600 transition-all ease-out duration-1000"
          style={"width: #{@entry.progress}%"}
        >
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
