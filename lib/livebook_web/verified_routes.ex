defmodule LivebookWeb.VerifiedRoutes do
  require Phoenix.VerifiedRoutes

  defmacro sigil_p({:<<>>, _meta, ["/public/" <> _ | _]} = route, extra) do
    # We allow configuring a base path for all routes and we configure
    # Phoenix to use it. However, we have an additional configuration
    # for base path applying only to /public. We use a custom sigil_p
    # to insert this base path if needed.
    quote do
      path = Phoenix.VerifiedRoutes.sigil_p(unquote(route), unquote(extra))
      LivebookWeb.VerifiedRoutes.__rewrite_public_base_path__(path)
    end
  end

  defmacro sigil_p(route, extra) do
    quote do
      Phoenix.VerifiedRoutes.sigil_p(unquote(route), unquote(extra))
    end
  end

  @doc """
  Returns the base url path for the Livebook endpoint.
  """
  @spec base_url_path() :: String.t()
  def base_url_path() do
    # Use fully qualified module name to avoid compile time dependencies
    path = Application.get_env(:livebook, :"Elixir.LivebookWeb.Endpoint")[:url][:path]
    String.trim_trailing(path, "/")
  end

  @doc """
  Returns the base url path for Livebook public endpoints (health & assets)
  """
  @spec public_base_url_path() :: String.t()
  def public_base_url_path() do
    case Application.get_env(:livebook, :public_base_url_path) do
      nil -> base_url_path()
      path -> String.trim_trailing(path, "/")
    end
  end

  @doc false
  def __rewrite_public_base_path__(path) do
    base_url_path = base_url_path()
    public_base_url_path = public_base_url_path()
    ^base_url_path <> rest = path
    public_base_url_path <> rest
  end
end
