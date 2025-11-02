defmodule LivebookWeb.Output.MarkdownStaticComponent do
  use LivebookWeb, :live_component

  @impl true
  def render(assigns) do
    ~H"""
    <div>{to_html(@output.text)}</div>
    """
  end

  defp to_html(markdown), do: Earmark.as_html!(markdown) |> Phoenix.HTML.raw()
end
