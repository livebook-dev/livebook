defmodule LivebookWeb.ErrorView do
  use LivebookWeb, :view

  def render("401.html", %{reason: %LivebookWeb.InvalidTokenError{}} = assigns) do
    render("401_token.html", assigns)
  end

  def template_not_found(_template, assigns) do
    render("500.html", assigns)
  end
end
