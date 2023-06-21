defmodule LivebookWeb.ErrorHTML do
  use LivebookWeb, :html

  def render("404.html", assigns) do
    ~H"""
    <.error_page status={@status} title="No Numbats here!" />
    """
  end

  def render("403.html", assigns) do
    ~H"""
    <.error_page status={@status} title="No Numbats allowed here!" />
    """
  end

  def render(_template, assigns) do
    ~H"""
    <.error_page
      status={@status}
      title="Something went wrong."
      details="Check out the console for logs for more information."
    />
    """
  end

  attr :status, :integer, required: true
  attr :title, :string, required: true
  attr :details, :string, default: nil

  defp error_page(assigns) do
    ~H"""
    <!DOCTYPE html>
    <html lang="en">
      <head>
        <meta charset="utf-8" />
        <meta http-equiv="X-UA-Compatible" content="IE=edge" />
        <meta name="viewport" content="width=device-width, initial-scale=1.0" />
        <link rel="icon" type="image/svg+xml" href={~p"/favicon.svg"} />
        <link rel="alternate icon" type="image/png" href={~p"/favicon.png"} />
        <title><%= @status %> - Livebook</title>
        <link rel="stylesheet" href={~p"/assets/app.css"} />
      </head>
      <body>
        <div class="h-screen flex items-center justify-center bg-gray-900">
          <div class="flex flex-col space-y-4 items-center">
            <a href={~p"/"}>
              <img src={~p"/images/logo.png"} height="128" width="128" alt="livebook" />
            </a>
            <div class="text-2xl text-gray-50">
              <%= @title %>
            </div>
            <div :if={@details} class="text-lg text-gray-50">
              <%= @details %>
            </div>
          </div>
        </div>
      </body>
    </html>
    """
  end
end
