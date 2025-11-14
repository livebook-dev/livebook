defmodule Livebook.Intellisense do
  # Language-specific intellisense that is used by the code editor.
  #
  # This module defines a behaviour and dispatches the intellisense
  # implementation to the appropriate language-specific module.

  alias Livebook.Intellisense
  alias Livebook.Runtime

  @typedoc """
  Evaluation state to consider for intellisense.

  The `:map_binding` is only called when a value needs to be extracted
  from binding.
  """
  @type context :: %{
          env: Macro.Env.t(),
          ebin_path: String.t() | nil,
          map_binding: (Code.binding() -> any())
        }

  @doc """
  Language-specific implementation of `t:Runtime.intellisense_request/1`.
  """
  @callback handle_request(
              request :: Runtime.intellisense_request(),
              context :: context(),
              node :: node()
            ) :: Runtime.intellisense_response()

  @doc """
  Resolves an intellisense request as defined in
  `t:Runtime.intellisense_request/1`.
  """
  @spec handle_request(
          Runtime.language(),
          Runtime.intellisense_request(),
          context(),
          node()
        ) :: Runtime.intellisense_response()
  def handle_request(language, request, context, node) do
    if impl = impl_for_language(language) do
      impl.handle_request(request, context, node)
    end
  end

  defp impl_for_language(:elixir), do: Intellisense.Elixir
  defp impl_for_language(:erlang), do: Intellisense.Erlang
  defp impl_for_language(_other), do: nil

  @doc """
  Adjusts the system for more accurate intellisense.
  """
  @spec load() :: :ok
  def load() do
    # Completion looks for modules in loaded applications, so we ensure
    # that the most relevant built-in applications are loaded
    apps = [:erts, :crypto, :inets, :public_key, :runtime_tools, :ex_unit, :iex]

    for app <- apps do
      Application.load(app)
    end

    :ok
  end

  @doc """
  Clears all cache stored by the intellisense modules.
  """
  @spec clear_cache() :: :ok
  def clear_cache() do
    for node <- Node.list() do
      clear_cache(node)
    end

    :ok
  end

  @doc """
  Clear any cache stored related to the given node.
  """
  @spec clear_cache(node()) :: :ok
  def clear_cache(node) do
    Intellisense.Elixir.IdentifierMatcher.clear_all_loaded(node)
  end
end
