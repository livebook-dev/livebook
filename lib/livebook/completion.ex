defmodule Livebook.Completion do
  @moduledoc false

  # This module provides basic intellisense completion
  # suitable for text editors.
  #
  # The implementation is based primarly on `IEx.Autocomplete`.
  # It also takes insights from `ElixirSense.Providers.Suggestion.Complete`,
  # which is a very extensive implementation used in the Elixir Language Server.

  @type completion_item :: Livebook.Runtime.completion_item()

  # Configures width used for inspect and specs formatting.
  @line_length 30

  @doc """
  Returns a list of completion suggestions for the given `hint`.

  Uses evaluation binding and environment to expand aliases,
  imports, nested maps, etc.

  `hint` may be a single token or line fragment like `if Enum.m`.
  """
  @spec get_completion_items(String.t(), Code.binding(), Macro.Env.t()) :: list(completion_item())
  def get_completion_items(hint, binding, env) do
    expr = hint |> String.to_charlist() |> Enum.reverse()

    expr
    |> completion_entrypoint(%{binding: binding, env: env})
    |> Enum.sort_by(&completion_item_priority/1)
  end

  defp completion_item_priority(completion_item) do
    {completion_item_kind_priority(completion_item.kind), completion_item.label}
  end

  @ordered_kinds [:variable, :module, :function, :type]

  defp completion_item_kind_priority(kind) when kind in @ordered_kinds do
    Enum.find_index(@ordered_kinds, &(&1 == kind))
  end

  # Takes hint as a reversed charlist
  defp completion_entrypoint([], ctx) do
    complete_variable_or_import("", ctx)
  end

  defp completion_entrypoint([h | t] = expr, ctx) do
    cond do
      h == ?. and t != [] ->
        complete_dot(reduce(t), ctx)

      h == ?: and t == [] ->
        complete_erlang_module("")

      identifier?(h) ->
        complete_expr(reduce(expr), ctx)

      h == ?/ and t != [] and identifier?(hd(t)) ->
        complete_expr(reduce(t), ctx)

      h in ' ([{,' ->
        completion_entrypoint([], ctx)

      true ->
        no()
    end
  end

  defp identifier?(h) do
    h in ?a..?z or h in ?A..?Z or h in ?0..?9 or h in '_?!'
  end

  # Given reversed expression, extracts the relevent token for completion.
  #
  #     iex> 'if(true, do: Enum.ma' |> Enum.reverse() |> reduce()
  #     "Enum.ma"
  #
  defp reduce(expr) do
    [token | _] = :string.lexemes(expr, ' ([{,')

    token
    |> Enum.reverse()
    |> trim_leading(?&)
    |> trim_leading(?%)
    |> trim_leading(?!)
    |> trim_leading(?^)
    |> to_string()
  end

  defp trim_leading([char | rest], char), do: rest
  defp trim_leading(expr, _char), do: expr

  defp no do
    []
  end

  defp complete_dot(expr, ctx) do
    case Code.string_to_quoted(expr) do
      {:ok, atom} when is_atom(atom) ->
        complete_call(atom, "", ctx)

      {:ok, {:__aliases__, _, list}} ->
        complete_elixir_module(list, "", ctx)

      {:ok, {_, _, _} = ast_node} ->
        complete_call(ast_node, "", ctx)

      _ ->
        no()
    end
  end

  defp complete_expr(expr, ctx) do
    case Code.string_to_quoted(expr) do
      {:ok, atom} when is_atom(atom) ->
        complete_erlang_module(Atom.to_string(atom))

      {:ok, {atom, _, nil}} when is_atom(atom) ->
        complete_variable_or_import(Atom.to_string(atom), ctx)

      {:ok, {:__aliases__, _, [root]}} ->
        complete_elixir_module([], Atom.to_string(root), ctx)

      {:ok, {:__aliases__, _, [h | _] = list}} when is_atom(h) ->
        hint = Atom.to_string(List.last(list))
        list = Enum.take(list, length(list) - 1)
        complete_elixir_module(list, hint, ctx)

      {:ok, {{:., _, [ast_node, fun]}, _, []}} when is_atom(fun) ->
        complete_call(ast_node, Atom.to_string(fun), ctx)

      _ ->
        no()
    end
  end

  ## Expand calls

  # `complete_call` takes ast node as the first argument
  # and `hint` being the actual call

  # :atom.fun
  defp complete_call(mod, hint, _ctx) when is_atom(mod) do
    complete_module_call(mod, hint)
  end

  # Elixir.fun
  defp complete_call({:__aliases__, _, list}, hint, ctx) do
    case expand_alias(list, ctx) do
      {:ok, alias} -> complete_module_call(alias, hint)
      :error -> no()
    end
  end

  # # variable.fun_or_key
  defp complete_call({_, _, _} = ast_node, hint, ctx) do
    case value_from_binding(ast_node, ctx.binding) do
      {:ok, mod} when is_atom(mod) -> complete_call(mod, hint, ctx)
      {:ok, map} when is_map(map) -> complete_map_field(map, hint)
      _otherwise -> no()
    end
  end

  defp complete_call(_, _, _) do
    no()
  end

  defp complete_module_call(mod, hint) do
    complete_module_function(mod, hint) ++ complete_module_type(mod, hint)
  end

  defp complete_variable_or_import(hint, ctx) do
    variables = complete_variable(hint, ctx)

    imports =
      imports_from_env(ctx)
      |> Enum.flat_map(fn {mod, funs} ->
        complete_module_function(mod, hint, funs)
      end)

    special_forms_funs = complete_module_function(Kernel.SpecialForms, hint)

    variables ++ imports ++ special_forms_funs
  end

  defp complete_variable(hint, ctx) do
    complete_key_value(ctx.binding, hint)
  end

  defp complete_map_field(map, hint) do
    # Note: we need Map.to_list/1 in case this is a struct
    complete_key_value(Map.to_list(map), hint)
  end

  defp complete_key_value(list, hint) do
    for {key, value} <- list,
        is_atom(key),
        name = Atom.to_string(key),
        String.starts_with?(name, hint),
        do: %{
          label: name,
          kind: :variable,
          detail: "variable",
          documentation: value_docstr(value),
          insert_text: name
        }
  end

  defp value_docstr(value) do
    """
    ```
    #{inspect(value, pretty: true, width: @line_length)}
    ```\
    """
  end

  ## Erlang modules

  defp complete_erlang_module(hint) do
    for mod <- get_matching_modules(hint),
        usable_as_unquoted_module?(mod),
        name = Atom.to_string(mod) do
      %{
        label: name,
        kind: :module,
        detail: "module",
        # TODO: support Erlang docs
        documentation: nil,
        insert_text: name
      }
    end
  end

  ## Elixir modules

  defp complete_elixir_module([], hint, ctx) do
    complete_alias(hint, ctx) ++ complete_module_submodule(nil, hint)
  end

  defp complete_elixir_module(list, hint, ctx) do
    case expand_alias(list, ctx) do
      {:ok, mod} -> complete_module_dot(mod, hint)
      :error -> no()
    end
  end

  defp complete_module_dot(mod, hint) do
    complete_module_submodule(mod, hint) ++ complete_module_call(mod, hint)
  end

  defp expand_alias([name | rest], ctx) when is_atom(name) do
    case Keyword.fetch(aliases_from_env(ctx), Module.concat(Elixir, name)) do
      {:ok, name} when rest == [] -> {:ok, name}
      {:ok, name} -> {:ok, Module.concat([name | rest])}
      :error -> {:ok, Module.concat([name | rest])}
    end
  end

  defp expand_alias([_ | _], _) do
    :error
  end

  defp complete_alias(hint, ctx) do
    for {alias, mod} <- aliases_from_env(ctx),
        [name] = Module.split(alias),
        String.starts_with?(name, hint) do
      %{
        label: name,
        kind: :module,
        detail: "module",
        documentation: mod |> get_module_doc_content() |> format_doc_content(),
        insert_text: name
      }
    end
  end

  defp complete_module_submodule(nil, hint) do
    items = complete_module_submodule(Elixir, hint)

    # `Elixir` is not a existing module name, but `Elixir.Enum`,
    # so if the user type `Eli` the completion should include `Elixir`.
    if String.starts_with?("Elixir", hint) do
      [
        %{
          label: "Elixir",
          kind: :module,
          detail: "module",
          documentation: nil,
          insert_text: "Elixir"
        }
        | items
      ]
    else
      items
    end
  end

  defp complete_module_submodule(mod, hint) do
    # Note `mod` may be `Elixir`, even though it's not a valid module

    match_prefix = "#{mod}.#{hint}"
    depth = match_prefix |> Module.split() |> length()

    for mod <- get_matching_modules(match_prefix),
        parts = Module.split(mod),
        length(parts) >= depth,
        name = Enum.at(parts, depth - 1),
        valid_alias_piece?("." <> name),
        mod = parts |> Enum.take(depth) |> Module.concat(),
        uniq: true,
        do: %{
          label: name,
          kind: :module,
          detail: "module",
          documentation: mod |> get_module_doc_content() |> format_doc_content(),
          insert_text: name
        }
  end

  defp valid_alias_piece?(<<?., char, rest::binary>>) when char in ?A..?Z,
    do: valid_alias_rest?(rest)

  defp valid_alias_piece?(_), do: false

  defp valid_alias_rest?(<<char, rest::binary>>)
       when char in ?A..?Z
       when char in ?a..?z
       when char in ?0..?9
       when char == ?_,
       do: valid_alias_rest?(rest)

  defp valid_alias_rest?(<<>>), do: true
  defp valid_alias_rest?(rest), do: valid_alias_piece?(rest)

  defp usable_as_unquoted_module?(mod) do
    Code.Identifier.classify(mod) != :other
  end

  defp get_matching_modules(hint) do
    get_modules()
    |> Enum.filter(&String.starts_with?(Atom.to_string(&1), hint))
    |> Enum.uniq()
  end

  defp get_modules() do
    modules = Enum.map(:code.all_loaded(), &elem(&1, 0))

    case :code.get_mode() do
      :interactive -> modules ++ get_modules_from_applications()
      _otherwise -> modules
    end
  end

  defp get_modules_from_applications do
    for [app] <- loaded_applications(),
        {:ok, modules} = :application.get_key(app, :modules),
        module <- modules,
        do: module
  end

  defp loaded_applications do
    # If we invoke :application.loaded_applications/0,
    # it can error if we don't call safe_fixtable before.
    # Since in both cases we are reaching over the
    # application controller internals, we choose to match
    # for performance.
    :ets.match(:ac_tab, {{:loaded, :"$1"}, :_})
  end

  defp complete_module_function(mod, hint, funs \\ nil) do
    if ensure_loaded?(mod) do
      # TODO: support Erlang docs (and consequently signatures)
      docs = get_docs(mod, [:function, :macro])
      specs = get_specs(mod)
      funs = funs || exports(mod)
      funs_with_base_arity = funs_with_base_arity(docs)

      funs
      |> Enum.filter(fn {name, _arity} ->
        name = Atom.to_string(name)
        String.starts_with?(name, hint)
      end)
      |> Enum.map(fn {name, arity} ->
        base_arity = Map.get(funs_with_base_arity, {name, arity}, arity)
        doc = find_doc(docs, {name, base_arity})
        spec = find_spec(specs, {name, base_arity})

        docstr = doc |> doc_content() |> format_doc_content()
        signatures = doc |> doc_signatures() |> format_signatures(mod)
        spec = format_spec(spec)

        %{
          label: "#{name}/#{arity}",
          kind: :function,
          detail: signatures,
          documentation: doc_join([docstr, spec]),
          insert_text: Atom.to_string(name)
        }
      end)
    else
      []
    end
  end

  # If a function has default arguments it generates less-arity functions,
  # but they have the same docs/specs as the original function.
  # Here we build a map that given function {name, arity} returns its base arity.
  defp funs_with_base_arity(docs) do
    for {{_, fun_name, arity}, _, _, _, metadata} <- docs,
        count = Map.get(metadata, :defaults, 0),
        count > 0,
        new_arity <- (arity - count)..(arity - 1),
        into: %{},
        do: {{fun_name, new_arity}, arity}
  end

  defp get_docs(mod, kinds) do
    case Code.fetch_docs(mod) do
      {:docs_v1, _, _, _, _, _, docs} ->
        for {{kind, _, _}, _, _, _, _} = doc <- docs, kind in kinds, do: doc

      {:error, _} ->
        []
    end
  end

  defp get_module_doc_content(mod) do
    case Code.fetch_docs(mod) do
      {:docs_v1, _, _, _, %{"en" => docstring}, _, _} ->
        docstring

      _ ->
        nil
    end
  end

  defp find_doc(docs, {name, arity}) do
    Enum.find(docs, &match?({{_, ^name, ^arity}, _, _, _, _}, &1))
  end

  defp get_specs(mod) do
    case Code.Typespec.fetch_specs(mod) do
      {:ok, specs} -> specs
      :error -> []
    end
  end

  defp find_spec(specs, {name, arity}) do
    Enum.find(specs, &match?({{^name, ^arity}, _}, &1))
  end

  defp doc_signatures({_, _, signatures, _, _}), do: signatures
  defp doc_signatures(_), do: []

  defp doc_content({_, _, _, %{"en" => docstr}, _}), do: docstr
  defp doc_content(_), do: nil

  defp format_signatures([], _mod), do: nil

  defp format_signatures(signatures, mod) do
    prefix = mod_to_prefix(mod)
    Enum.map_join(signatures, "\n", &(prefix <> &1))
  end

  defp mod_to_prefix(mod) do
    case Atom.to_string(mod) do
      "Elixir." <> name -> name <> "."
      name -> name <> "."
    end
  end

  defp format_doc_content(nil), do: nil

  defp format_doc_content(docstr) do
    # Extract just the first paragraph
    docstr
    |> String.split("\n\n")
    |> hd()
    |> String.trim()
  end

  defp doc_join(list) do
    case Enum.reject(list, &is_nil/1) do
      [] -> nil
      parts -> Enum.join(parts, "\n\n")
    end
  end

  defp format_spec(nil), do: nil

  defp format_spec({{name, _arity}, spec_ast_list}) do
    spec_lines =
      Enum.map(spec_ast_list, fn spec_ast ->
        spec =
          Code.Typespec.spec_to_quoted(name, spec_ast)
          |> Macro.to_string()
          |> Code.format_string!(line_length: @line_length)

        ["@spec ", spec]
      end)

    ["```", spec_lines, "```"]
    |> Enum.intersperse("\n")
    |> IO.iodata_to_binary()
  end

  defp exports(mod) do
    if Code.ensure_loaded?(mod) and function_exported?(mod, :__info__, 1) do
      mod.__info__(:macros) ++ (mod.__info__(:functions) -- [__info__: 1])
    else
      mod.module_info(:exports) -- [module_info: 0, module_info: 1]
    end
  end

  defp complete_module_type(mod, hint) do
    docs = get_docs(mod, [:type])
    types = get_module_types(mod)

    types
    |> Enum.filter(fn {name, _arity} ->
      name = Atom.to_string(name)
      String.starts_with?(name, hint)
    end)
    |> Enum.map(fn {name, arity} ->
      doc = find_doc(docs, {name, arity})
      docstr = doc |> doc_content() |> format_doc_content()

      %{
        label: "#{name}/#{arity}",
        kind: :type,
        detail: "typespec",
        documentation: docstr,
        insert_text: Atom.to_string(name)
      }
    end)
  end

  defp get_module_types(mod) do
    if ensure_loaded?(mod) do
      case Code.Typespec.fetch_types(mod) do
        {:ok, types} ->
          for {kind, {name, _, args}} <- types,
              kind in [:type, :opaque] do
            {name, length(args)}
          end

        :error ->
          []
      end
    else
      []
    end
  end

  defp ensure_loaded?(Elixir), do: false
  defp ensure_loaded?(mod), do: Code.ensure_loaded?(mod)

  ## Context helpers

  defp imports_from_env(ctx) do
    ctx.env.functions ++ ctx.env.macros
  end

  defp aliases_from_env(ctx) do
    ctx.env.aliases
  end

  defp value_from_binding(ast_node, binding) do
    with {var, map_key_path} <- extract_from_ast(ast_node, []) do
      traverse_binding(binding, var, map_key_path)
    else
      _ -> :error
    end
  end

  defp extract_from_ast(var_name, acc) when is_atom(var_name) do
    {var_name, acc}
  end

  defp extract_from_ast({var_name, _, nil}, acc) when is_atom(var_name) do
    {var_name, acc}
  end

  defp extract_from_ast({{:., _, [ast_node, fun]}, _, []}, acc) when is_atom(fun) do
    extract_from_ast(ast_node, [fun | acc])
  end

  defp extract_from_ast(_ast_node, _acc) do
    :error
  end

  defp traverse_binding(binding, var_name, map_key_path) do
    accumulator = Keyword.fetch(binding, var_name)

    Enum.reduce(map_key_path, accumulator, fn
      key, {:ok, map} when is_map(map) -> Map.fetch(map, key)
      _key, _acc -> :error
    end)
  end
end
