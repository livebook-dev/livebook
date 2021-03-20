defmodule LivebookWeb.Helpers do
  import Phoenix.LiveView.Helpers
  import Phoenix.HTML.Tag

  @doc """
  Renders a component inside the `Livebook.ModalComponent` component.

  The rendered modal receives a `:return_to` option to properly update
  the URL when the modal is closed.
  """
  def live_modal(socket, component, opts) do
    path = Keyword.fetch!(opts, :return_to)
    modal_opts = [id: :modal, return_to: path, component: component, opts: opts]
    live_component(socket, LivebookWeb.ModalComponent, modal_opts)
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

  defdelegate ansi_string_to_html(string, opts \\ []), to: LivebookWeb.ANSI

  @doc """
  Returns [Remix](https://remixicon.com) icon tag.
  """
  def remix_icon(name, attrs \\ []) do
    icon_class = "ri-#{name}"
    attrs = Keyword.update(attrs, :class, icon_class, fn class -> "#{icon_class} #{class}" end)
    content_tag(:i, "", attrs)
  end
end
