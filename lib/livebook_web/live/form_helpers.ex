defmodule LivebookWeb.FormHelpers do
  @moduledoc """
  Conveniences for translating and building error messages.
  """
  use Phoenix.Component

  import Phoenix.HTML.Form
  import LivebookWeb.LiveHelpers

  @doc """
  A wrapper for inputs with conveniences.
  """
  def input_wrapper(assigns) do
    assigns = assign_new(assigns, :class, fn -> [] end)

    ~H"""
    <div
      phx-feedback-for={input_name(@form, @field)}
      class={[@class, if(@form.errors[@field], do: "show-errors", else: "")]}
    >
      <%= render_slot(@inner_block) %>
      <%= for error <- Keyword.get_values(@form.errors, @field) do %>
        <span class="hidden text-red-600 text-sm phx-form-error:block">
          <%= translate_error(error) %>
        </span>
      <% end %>
    </div>
    """
  end

  @doc """
  Hex color input.
  """
  def hex_color_input(assigns) do
    ~H"""
    <div class="flex space-x-4 items-center">
      <div
        class="border-[3px] rounded-lg p-1 flex justify-center items-center"
        style={"border-color: #{input_value(@form, @field)}"}
      >
        <div class="rounded h-5 w-5" style={"background-color: #{input_value(@form, @field)}"}></div>
      </div>
      <div class="relative grow">
        <%= text_input(@form, @field,
          class: "input",
          spellcheck: "false",
          maxlength: 7
        ) %>
        <button class="icon-button absolute right-2 top-1" type="button" phx-click={@randomize}>
          <.remix_icon icon="refresh-line" class="text-xl" />
        </button>
      </div>
    </div>
    """
  end

  @doc """
  Emoji input.
  """
  def emoji_input(assigns) do
    ~H"""
    <div id={@id} class="flex border-[1px] bg-gray-50 rounded-lg space-x-4 items-center">
      <div id={"#{@id}-picker"} class="grid grid-cols-1 md:grid-cols-3 w-full" phx-hook="EmojiPicker">
        <div class="place-content-start">
          <div class="p-1 pl-3">
            <span id={"#{@id}-preview"} data-emoji-preview><%= input_value(@form, @field) %></span>
          </div>
        </div>

        <div />

        <div class="flex items-center place-content-end">
          <button
            id={"#{@id}-button"}
            type="button"
            data-emoji-button
            class="p-1 pl-3 pr-3 rounded-tr-lg rounded-br-lg bg-gray-50 hover:bg-gray-100 active:bg-gray-200 border-l-[1px] bg-white flex justify-center items-center cursor-pointer"
          >
            <.remix_icon icon="emotion-line" class="text-xl" />
          </button>
        </div>
        <div id={"#{@id}-container"} data-emoji-container class="absolute mt-10 hidden" />
        <%= hidden_input(@form, @field, class: "hidden emoji-picker-input", "data-emoji-input": true) %>
      </div>
    </div>
    """
  end

  @doc """
  Translates an error message.
  """
  def translate_error({msg, opts}) do
    # Because the error messages we show in our forms and APIs
    # are defined inside Ecto, we need to translate them dynamically.
    Enum.reduce(opts, msg, fn {key, value}, acc ->
      String.replace(acc, "%{#{key}}", fn _ -> to_string(value) end)
    end)
  end
end
