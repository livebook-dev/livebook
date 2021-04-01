defmodule LivebookWeb.ErrorView do
  use LivebookWeb, :view

  def template_not_found(_template, assigns) do
    render("500.html", assigns)
  end
end
