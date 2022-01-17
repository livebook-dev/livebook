defmodule Livebook.Intellisense.Docs do
  @moduledoc false

  # This module is responsible for extracting and normalizing
  # information like documentation, signatures and specs.

  @type member_info :: %{
          kind: member_kind(),
          name: atom(),
          arity: non_neg_integer(),
          documentation: documentation(),
          signatures: list(signature()),
          specs: list(spec()),
          meta: meta()
        }

  @type member_kind :: :function | :macro | :type

  @type documentation :: {format :: String.t(), content :: String.t()} | :hidden | nil

  @type signature :: String.t()

  @type meta :: map()

  @typedoc """
  A single spec annotation in the Erlang Abstract Format.
  """
  @type spec :: term()

  @doc """
  Fetches documentation for the given module if available.
  """
  @spec get_module_documentation(module()) :: documentation()
  def get_module_documentation(module) do
    case Code.fetch_docs(module) do
      {:docs_v1, _, _, format, %{"en" => docstring}, _, _} ->
        {format, docstring}

      {:docs_v1, _, _, _, :hidden, _, _} ->
        :hidden

      _ ->
        nil
    end
  end

  @doc """
  Fetches information about the given module members if available.

  The given `members` are used to limit the result to the relevant
  entries. Arity may be given as `:any`, in which case all entries
  matching the name are returned.

  Functions with default arguments are normalized, such that each
  arity is treated as a separate member, sourcing documentation
  from the original one.

  ## Options

    * `:kinds` - a list of member kinds to limit the lookup to.
      Valid kinds are `:function`, `:macro` and `:type`. Defaults
      to all kinds
  """
  @spec lookup_module_members(
          module(),
          list({name :: atom(), arity :: non_neg_integer() | :any}),
          keyword()
        ) :: list(member_info())
  def lookup_module_members(module, members, opts \\ []) do
    members = MapSet.new(members)
    kinds = opts[:kinds] || [:function, :macro, :type]

    specs =
      with true <- :function in kinds or :macro in kinds,
           {:ok, specs} <- Code.Typespec.fetch_specs(module) do
        Map.new(specs)
      else
        _ -> %{}
      end

    case Code.fetch_docs(module) do
      {:docs_v1, _, _, format, _, _, docs} ->
        for {{kind, name, base_arity}, _line, signatures, doc, meta} <- docs,
            kind in kinds,
            defaults = Map.get(meta, :defaults, 0),
            arity <- (base_arity - defaults)..base_arity,
            MapSet.member?(members, {name, arity}) or MapSet.member?(members, {name, :any}),
            do: %{
              kind: kind,
              name: name,
              arity: arity,
              documentation: documentation(doc, format),
              signatures: signatures,
              specs: Map.get(specs, {name, base_arity}, []),
              meta: meta
            }

      _ ->
        []
    end
  end

  defp documentation(%{"en" => docstr}, format), do: {format, docstr}
  defp documentation(:hidden, _format), do: :hidden
  defp documentation(_doc, _format), do: nil

  @doc """
  Determines a more specific module type if any.
  """
  @spec get_module_subtype(module) ::
          :protocol | :implementation | :exception | :struct | :behaviour | nil
  def get_module_subtype(module) do
    cond do
      not ensure_loaded?(module) ->
        nil

      function_exported?(module, :__protocol__, 1) ->
        :protocol

      function_exported?(module, :__impl__, 1) ->
        :implementation

      function_exported?(module, :__struct__, 0) ->
        if function_exported?(module, :exception, 1) do
          :exception
        else
          :struct
        end

      function_exported?(module, :behaviour_info, 1) ->
        :behaviour

      true ->
        nil
    end
  end

  # In case insensitive file systems, attempting to load
  # Elixir will log a warning in the terminal as it wrongly
  # loads elixir.beam, so we explicitly list it.
  defp ensure_loaded?(Elixir), do: false
  defp ensure_loaded?(module), do: Code.ensure_loaded?(module)
end
