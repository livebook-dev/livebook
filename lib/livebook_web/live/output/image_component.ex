defmodule LivebookWeb.Output.ImageComponent do
  use LivebookWeb, :live_component

  @impl true
  def render(assigns) do
    ~L"""
    <%= tag :img, src: data_url(@content, @mime_type), alt: "output image" %>
    """
  end

  defp data_url(content, mime_type) do
    image_base64 = Base.encode64(content)
    ["data:", mime_type, ";base64,", image_base64]
  end
end
