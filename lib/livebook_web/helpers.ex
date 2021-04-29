defmodule LivebookWeb.Helpers do
  import Phoenix.LiveView.Helpers
  import Phoenix.HTML.Tag
  alias LivebookWeb.Router.Helpers, as: Routes

  @doc """
  Renders a component inside the `Livebook.ModalComponent` component.

  The rendered modal receives a `:return_to` option to properly update
  the URL when the modal is closed.
  """
  def live_modal(socket, component, opts) do
    path = Keyword.fetch!(opts, :return_to)
    modal_class = Keyword.get(opts, :modal_class)

    modal_opts = [
      id: :modal,
      return_to: path,
      modal_class: modal_class,
      component: component,
      opts: opts
    ]

    live_component(socket, LivebookWeb.ModalComponent, modal_opts)
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
  Returns [Remix](https://remixicon.com) icon tag.
  """
  def remix_icon(name, attrs \\ []) do
    icon_class = "ri-#{name}"
    attrs = Keyword.update(attrs, :class, icon_class, fn class -> "#{icon_class} #{class}" end)
    content_tag(:i, "", attrs)
  end

  defdelegate ansi_string_to_html(string, opts \\ []), to: LivebookWeb.ANSI

  @doc """
  Converts a string with ANSI escape codes into HTML lines.

  This method is similar to `ansi_string_to_html/2`,
  but makes sure each line is itself a valid HTML
  (as opposed to just splitting HTML into lines).
  """
  @spec ansi_to_html_lines(String.t()) :: list(Phoenix.HTML.safe())
  def ansi_to_html_lines(string) do
    string
    |> ansi_string_to_html(
      # Make sure every line is styled separately,
      # so that later we can safely split the whole HTML
      # into valid HTML lines.
      renderer: fn style, content ->
        content
        |> IO.iodata_to_binary()
        |> String.split("\n")
        |> Enum.map(&LivebookWeb.ANSI.default_renderer(style, &1))
        |> Enum.intersperse("\n")
      end
    )
    |> Phoenix.HTML.safe_to_string()
    |> String.split("\n")
    |> Enum.map(&Phoenix.HTML.raw/1)
  end

  @doc """
  Returns path to specific process dialog within LiveDashboard.
  """
  def live_dashboard_process_path(socket, pid) do
    pid_str = Phoenix.LiveDashboard.Helpers.encode_pid(pid)
    Routes.live_dashboard_path(socket, :page, node(), "processes", info: pid_str)
  end
end
