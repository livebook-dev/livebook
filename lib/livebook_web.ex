defmodule LivebookWeb do
  @moduledoc false

  def static_paths, do: ~w(assets images favicon.svg favicon.png robots.txt)

  def controller do
    quote do
      use Phoenix.Controller,
        formats: [:html, :json],
        layouts: [html: LivebookWeb.Layouts]

      import Plug.Conn

      unquote(verified_routes())
    end
  end

  def live_view do
    quote do
      use Phoenix.LiveView, layout: {LivebookWeb.Layouts, :live}

      unquote(html_helpers())
    end
  end

  def live_component do
    quote do
      use Phoenix.LiveComponent

      unquote(html_helpers())
    end
  end

  def router do
    quote do
      use Phoenix.Router, helpers: false

      import Plug.Conn
      import Phoenix.Controller
      import Phoenix.LiveView.Router
    end
  end

  def html do
    quote do
      use Phoenix.Component

      # Import convenience functions from controllers
      import Phoenix.Controller,
        only: [get_csrf_token: 0, view_module: 1, view_template: 1]

      # Include general helpers for rendering HTML
      unquote(html_helpers())
    end
  end

  defp html_helpers do
    quote do
      # HTML escaping functionality
      import Phoenix.HTML

      # Core UI components
      import LivebookWeb.CoreComponents
      import LivebookWeb.FormComponents
      import LivebookWeb.Confirm

      # Shortcut for generating JS commands
      alias Phoenix.LiveView.JS

      # Custom helpers
      import LivebookWeb.Helpers

      # Routes generation with the ~p sigil
      unquote(verified_routes())
    end
  end

  def verified_routes do
    quote do
      use Phoenix.VerifiedRoutes,
        endpoint: LivebookWeb.Endpoint,
        router: LivebookWeb.Router,
        statics: LivebookWeb.static_paths()

      # We don't know the hostname Livebook runs on, so we don't use
      # absolute URL helpers
      import Phoenix.VerifiedRoutes, only: :sigils
    end
  end

  @doc """
  When used, dispatch to the appropriate controller/view/etc.
  """
  defmacro __using__(which) when is_atom(which) do
    apply(__MODULE__, which, [])
  end
end
