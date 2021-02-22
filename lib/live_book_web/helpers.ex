defmodule LiveBookWeb.Helpers do
  import Phoenix.LiveView.Helpers

  @doc """
  Renders a component inside the `LiveBook.ModalComponent` component.

  The rendered modal receives a `:return_to` option to properly update
  the URL when the modal is closed.
  """
  def live_modal(socket, component, opts) do
    path = Keyword.fetch!(opts, :return_to)
    modal_opts = [id: :modal, return_to: path, component: component, opts: opts]
    live_component(socket, LiveBookWeb.ModalComponent, modal_opts)
  end

  @doc """
  Determines user platform based on the given *User-Agent* header.
  """
  @spec platform_from_user_agent(Sting.t()) :: :linux | :mac | :windows | :other
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
  Takes a string with ANSI escape codes and build a HTML safe string
  with `span` tags having classes corresponding to the escape codes.

  Note that currently only one escape at a time is supported.
  Any HTML in the string is escaped.
  """
  @spec ansi_string_to_html(String.t()) :: Phoenix.HTML.safe()
  def ansi_string_to_html(string) do
    [head | tail] = String.split(string, "\e[")

    {:safe, head_html} = Phoenix.HTML.html_escape(head)

    tail_html =
      Enum.map(tail, fn string ->
        {class, rest} = ansi_prefix_to_class(string)
        {:safe, content} = Phoenix.HTML.html_escape(rest)

        if class do
          [~s{<span class="ansi #{class}">}, content, ~s{</span>}]
        else
          content
        end
      end)

    Phoenix.HTML.raw([head_html, tail_html])
  end

  @ansi_code_with_class [
    {"0m", nil},
    {"1A", "cursor-up"},
    {"1B", "cursor-down"},
    {"1C", "cursor-right"},
    {"1D", "cursor-left"},
    {"1m", "bright"},
    {"2J", "clear"},
    {"2K", "clear-line"},
    {"2m", "faint"},
    {"3m", "italic"},
    {"4m", "underline"},
    {"5m", "blink-slow"},
    {"6m", "blink-rapid"},
    {"7m", "inverse"},
    {"8m", "conceal"},
    {"9m", "crossed-out"},
    {"10m", "primary-font"},
    {"11m", "font-1"},
    {"12m", "font-2"},
    {"13m", "font-3"},
    {"14m", "font-4"},
    {"15m", "font-5"},
    {"16m", "font-6"},
    {"17m", "font-7"},
    {"18m", "font-8"},
    {"19m", "font-9"},
    {"22m", "normal"},
    {"23m", "not-italic"},
    {"24m", "no-underline"},
    {"25m", "blink-off"},
    {"27m", "inverse-off"},
    {"30m", "black"},
    {"31m", "red"},
    {"32m", "green"},
    {"33m", "yellow"},
    {"34m", "blue"},
    {"35m", "magenta"},
    {"36m", "cyan"},
    {"37m", "white"},
    {"39m", "default-color"},
    {"40m", "black-background"},
    {"41m", "red-background"},
    {"42m", "green-background"},
    {"43m", "yellow-background"},
    {"44m", "blue-background"},
    {"45m", "magenta-background"},
    {"46m", "cyan-background"},
    {"47m", "white-background"},
    {"49m", "default-background"},
    {"51m", "framed"},
    {"52m", "encircled"},
    {"53m", "overlined"},
    {"54m", "not-framed-encircled"},
    {"55m", "not-overlined"},
    {"90m", "light-black"},
    {"91m", "light-red"},
    {"92m", "light-green"},
    {"93m", "light-yellow"},
    {"94m", "light-blue"},
    {"95m", "light-magenta"},
    {"96m", "light-cyan"},
    {"97m", "light-white"},
    {"100m", "light-black-background"},
    {"101m", "light-red-background"},
    {"102m", "light-green-background"},
    {"103m", "light-yellow-background"},
    {"104m", "light-blue-background"},
    {"105m", "light-magenta-background"},
    {"106m", "light-cyan-background"},
    {"107m", "light-white-background"},
    {"H", "home"}
  ]

  for {code, class} <- @ansi_code_with_class do
    defp ansi_prefix_to_class(unquote(code) <> rest) do
      {unquote(class), rest}
    end
  end

  defp ansi_prefix_to_class(string), do: {nil, string}
end
