defmodule LivebookWeb do
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

      # Routes generation with the ~p sigil
      unquote(verified_routes())
    end
  end

  def public_path(path) do
    base_url_path = Livebook.Config.base_url_path()
    public_base_url_path = Livebook.Config.public_base_url_path()
    ^base_url_path <> rest = path
    public_base_url_path <> rest
  end

  def verified_routes do
    quote do
      use Phoenix.VerifiedRoutes,
        endpoint: LivebookWeb.Endpoint,
        router: LivebookWeb.Router,
        statics: LivebookWeb.static_paths()

      # We don't know the hostname Livebook runs on, so we don't use
      # absolute URL helpers
      import Phoenix.VerifiedRoutes, only: []
      import LivebookWeb, only: [sigil_p: 2]
    end
  end

  @doc """
  When used, dispatch to the appropriate controller/view/etc.
  """
  defmacro __using__(which) when is_atom(which) do
    apply(__MODULE__, which, [])
  end

  # Overrides
  require Phoenix.VerifiedRoutes

  defmacro sigil_p({:<<>>, _meta, ["/public/" <> _ | _]} = route, extra) do
    # We allow configuring a base path for all routes and we configure
    # Phoenix to use it. However, we have an additional configuration
    # for base path applying only to /public. We use a custom sigil_p
    # to insert this base path if needed.
    quote do
      path = Phoenix.VerifiedRoutes.sigil_p(unquote(route), unquote(extra))
      LivebookWeb.__rewrite_public_base_path__(path)
    end
  end

  defmacro sigil_p(route, extra) do
    quote do
      Phoenix.VerifiedRoutes.sigil_p(unquote(route), unquote(extra))
    end
  end

  def __rewrite_public_base_path__(path) do
    base_url_path = Livebook.Config.base_url_path()
    public_base_url_path = Livebook.Config.public_base_url_path()
    ^base_url_path <> rest = path
    public_base_url_path <> rest
  end
end
