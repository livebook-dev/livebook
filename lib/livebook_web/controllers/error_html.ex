defmodule LivebookWeb.ErrorHTML do
  use LivebookWeb, :html

  embed_templates "error_html/*"

  def render(_template, assigns) do
    render("500.html", assigns)
  end
end
