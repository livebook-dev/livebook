defmodule Livebook.Intellisense.IdentifierMatcher do
  # This module allows for extracting information about identifiers
  # based on code and runtime information (binding, environment).
  #
  # This functionality is a basic building block to be used for code
  # completion and information extraction.
  #
  # The implementation is based primarily on `IEx.Autocomplete`. It
  # also takes insights from `ElixirSense.Providers.Suggestion.Complete`,
  # which is a very extensive implementation used in the Elixir Language
  # Server.

  alias Livebook.Intellisense
  alias Livebook.Intellisense.Docs

  @typedoc """
  A single identifier together with relevant information.
  """
  @type identifier_item ::
          %{
            kind: :variable,
            name: name()
          }
          | %{
              kind: :map_field,
              name: name()
            }
          | %{
              kind: :in_map_field,
              name: name()
            }
          | %{
              kind: :in_struct_field,
              module: module(),
              name: name(),
              default: term()
            }
          | %{
              kind: :module,
              module: module(),
              display_name: display_name(),
              documentation: Docs.documentation()
            }
          | %{
              kind: :function,
              module: module(),
              name: name(),
              arity: arity(),
              type: :function | :macro,
              display_name: display_name(),
              from_default: boolean(),
              documentation: Docs.documentation(),
              signatures: list(Docs.signature()),
              specs: list(Docs.spec()),
              meta: Docs.meta()
            }
          | %{
              kind: :type,
              module: module(),
              name: name(),
              arity: arity(),
              documentation: Docs.documentation(),
              type_spec: Docs.type_spec()
            }
          | %{
              kind: :module_attribute,
              name: name(),
              documentation: Docs.documentation()
            }
          | %{
              kind: :bitstring_modifier,
              name: name(),
              arity: integer()
            }

  @type name :: atom()
  @type display_name :: String.t()

  @exact_matcher &Kernel.==/2
  @prefix_matcher &String.starts_with?/2

  @bitstring_modifiers [
    {:big, 0},
    {:binary, 0},
    {:bitstring, 0},
    {:integer, 0},
    {:float, 0},
    {:little, 0},
    {:native, 0},
    {:signed, 0},
    {:size, 1},
    {:unit, 1},
    {:unsigned, 0},
    {:utf8, 0},
    {:utf16, 0},
    {:utf32, 0}
  ]

  @alias_only_atoms ~w(alias import require)a
  @alias_only_charlists ~w(alias import require)c

  @doc """
  Clears all loaded entries stored for node.
  """
  def clear_all_loaded(node) do
    :persistent_term.erase({__MODULE__, node})
  end

  defp cached_all_loaded(node) do
    case :persistent_term.get({__MODULE__, node}, :error) do
      :error ->
        modules = Enum.map(:erpc.call(node, :code, :all_loaded, []), &elem(&1, 0))
        :persistent_term.put({__MODULE__, node}, modules)
        modules

      [_ | _] = modules ->
        modules
    end
  end

  @doc """
  Returns a list of identifiers matching the given `hint` together
  with relevant information.

  Evaluation binding and environment is used to expand aliases,
  imports, nested maps, etc.

  `hint` may be a single token or line fragment like `if Enum.m`.
  """
  @spec completion_identifiers(String.t(), Intellisense.context(), node()) ::
          list(identifier_item())
  def completion_identifiers(hint, intellisense_context, node) do
    context = Code.Fragment.cursor_context(hint)

    ctx = %{
      fragment: hint,
      intellisense_context: intellisense_context,
      matcher: @prefix_matcher,
      type: :completion,
      node: node
    }

    context_to_matches(context, ctx)
  end

  @doc """
  Extracts information about an identifier found in `column` in
  `line`.

  The function returns range of columns where the identifier
  is located and a list of matching identifier items.
  """
  @spec locate_identifier(String.t(), pos_integer(), Intellisense.context(), node()) ::
          %{
            matches: list(identifier_item()),
            range: nil | %{from: pos_integer(), to: pos_integer()}
          }
  def locate_identifier(line, column, intellisense_context, node) do
    case Code.Fragment.surround_context(line, {1, column}) do
      %{context: context, begin: {_, from}, end: {_, to}} ->
        fragment = String.slice(line, 0, to - 1)

        ctx = %{
          fragment: fragment,
          intellisense_context: intellisense_context,
          matcher: @exact_matcher,
          type: :locate,
          node: node
        }

        matches = context_to_matches(context, ctx)
        %{matches: matches, range: %{from: from, to: to}}

      :none ->
        %{matches: [], range: nil}
    end
  end

  # Takes a context returned from Code.Fragment.cursor_context
  # or Code.Fragment.surround_context and looks up matching
  # identifier items
  defp context_to_matches(context, ctx) do
    case context do
      {:alias, alias} ->
        match_alias(List.to_string(alias), ctx, false)

      {:unquoted_atom, unquoted_atom} ->
        match_erlang_module(List.to_string(unquoted_atom), ctx)

      {:dot, path, hint} ->
        if alias = dot_alias_only(path, hint, ctx.fragment, ctx) do
          match_alias(List.to_string(alias), ctx, false)
        else
          match_dot(path, List.to_string(hint), ctx)
        end

      {:dot_arity, path, hint} ->
        match_dot(path, List.to_string(hint), %{ctx | matcher: @exact_matcher})

      {:dot_call, _path, _hint} ->
        match_default(ctx)

      :expr ->
        match_container_context(ctx.fragment, :expr, "", ctx) || match_default(ctx)

      {:local_or_var, local_or_var} ->
        hint = List.to_string(local_or_var)
        match_container_context(ctx.fragment, :expr, hint, ctx) || match_local_or_var(hint, ctx)

      {:local_arity, local} ->
        match_local(List.to_string(local), %{ctx | matcher: @exact_matcher})

      {:local_call, local} when local in @alias_only_charlists ->
        match_alias("", ctx, false)

      {:local_call, local} ->
        case ctx.type do
          :completion -> match_default(ctx)
          :locate -> match_local(List.to_string(local), %{ctx | matcher: @exact_matcher})
        end

      {:operator, operator} when operator in ~w(:: -)c ->
        match_container_context(ctx.fragment, :operator, "", ctx) ||
          match_local_or_var(List.to_string(operator), ctx)

      {:operator, operator} ->
        match_local_or_var(List.to_string(operator), ctx)

      {:operator_arity, operator} ->
        match_local(List.to_string(operator), %{ctx | matcher: @exact_matcher})

      {:operator_call, operator} when operator in ~w(|)c ->
        match_container_context(ctx.fragment, :expr, "", ctx) || match_default(ctx)

      {:operator_call, _operator} ->
        match_default(ctx)

      {:module_attribute, attribute} ->
        match_module_attribute(List.to_string(attribute), ctx)

      {:sigil, []} ->
        match_sigil("", ctx) ++ match_local("~", ctx)

      {:sigil, sigil} ->
        match_sigil(List.to_string(sigil), ctx)

      {:struct, struct} ->
        match_struct(List.to_string(struct), ctx)

      # :none
      _ ->
        []
    end
  end

  defp match_dot(path, hint, ctx) do
    case expand_dot_path(path, ctx) do
      {:ok, mod} when is_atom(mod) and hint == "" ->
        match_module_member(mod, hint, ctx) ++ match_module(mod, hint, false, ctx)

      {:ok, mod} when is_atom(mod) ->
        match_module_member(mod, hint, ctx)

      {:ok, map} when is_map(map) ->
        match_map_field(map, hint, ctx)

      _ ->
        []
    end
  end

  defp expand_dot_path({:alias, alias}, ctx) do
    {:ok, expand_alias(List.to_string(alias), ctx)}
  end

  defp expand_dot_path({:unquoted_atom, var}, _ctx) do
    {:ok, List.to_atom(var)}
  end

  defp expand_dot_path({:module_attribute, _attribute}, _ctx) do
    :error
  end

  defp expand_dot_path(path, ctx) do
    with {:ok, path} <- recur_expand_dot_path(path, []) do
      value_from_binding(path, ctx)
    end
  end

  defp recur_expand_dot_path({:var, var}, path) do
    {:ok, [List.to_atom(var) | path]}
  end

  defp recur_expand_dot_path({:dot, parent, call}, path) do
    recur_expand_dot_path(parent, [List.to_atom(call) | path])
  end

  defp recur_expand_dot_path(_, _path) do
    :error
  end

  defp value_from_binding([var | map_path], ctx) do
    if Macro.Env.has_var?(ctx.intellisense_context.env, {var, nil}) do
      ctx.intellisense_context.map_binding.(fn binding ->
        value = Keyword.fetch(binding, var)

        Enum.reduce(map_path, value, fn
          key, {:ok, map} when is_map(map) -> Map.fetch(map, key)
          _key, _acc -> :error
        end)
      end)
    else
      :error
    end
  end

  defp match_default(ctx) do
    match_local_or_var("", ctx)
  end

  defp match_alias(hint, ctx, nested?) do
    case split_at_last_occurrence(hint, ".") do
      :error ->
        match_elixir_root_module(hint, nested?, ctx) ++ match_env_alias(hint, ctx)

      {:ok, alias, hint} ->
        mod = expand_alias(alias, ctx)
        match_module(mod, hint, nested?, ctx)
    end
  end

  defp dot_alias_only(path, hint, code, ctx) do
    with {:alias, alias} <- path,
         [] <- hint,
         :alias_only <- container_context(code, ctx) do
      alias ++ [?.]
    else
      _ -> nil
    end
  end

  # This is ignoring information from remote nodes
  # and only listing structs that are also structs
  # in the current node. Doing this check remotely
  # would unfortunately be too expensive. Alternatively
  # we list all modules.
  defp match_struct(hint, ctx) do
    for %{kind: :module, module: module} = item <- match_alias(hint, ctx, true),
        has_struct?(module),
        not is_exception?(module),
        do: item
  end

  defp has_struct?(mod) do
    Code.ensure_loaded?(mod) and function_exported?(mod, :__struct__, 1)
  end

  defp is_exception?(mod) do
    Code.ensure_loaded?(mod) and function_exported?(mod, :exception, 1)
  end

  defp match_module_member(mod, hint, ctx) do
    match_module_function(mod, hint, ctx) ++ match_module_type(mod, hint, ctx)
  end

  defp match_container_context(code, context, hint, ctx) do
    case container_context(code, ctx) do
      {:map, map, pairs} when context == :expr ->
        container_context_map_fields(pairs, map, hint, ctx)

      {:struct, alias, pairs} when context == :expr ->
        container_context_struct_fields(pairs, alias, hint, ctx)

      :bitstring_modifier ->
        existing = code |> String.split("::") |> List.last() |> String.split("-")

        for {modifier, arity} <- @bitstring_modifiers,
            name = Atom.to_string(modifier),
            String.starts_with?(name, hint) and name not in existing,
            do: %{kind: :bitstring_modifier, name: modifier, arity: arity}

      _ ->
        nil
    end
  end

  defp container_context(code, ctx) do
    case Code.Fragment.container_cursor_to_quoted(code) do
      {:ok, quoted} ->
        case Macro.path(quoted, &match?({:__cursor__, _, []}, &1)) do
          [cursor, {:%{}, _, pairs}, {:%, _, [{:__aliases__, _, aliases}, _map]} | _] ->
            container_context_struct(cursor, pairs, aliases, ctx)

          [
            cursor,
            pairs,
            {:|, _, _},
            {:%{}, _, _},
            {:%, _, [{:__aliases__, _, aliases}, _map]} | _
          ] ->
            container_context_struct(cursor, pairs, aliases, ctx)

          [cursor, pairs, {:|, _, [{variable, _, nil} | _]}, {:%{}, _, _} | _] ->
            container_context_map(cursor, pairs, variable, ctx)

          [cursor, {special_form, _, [cursor]} | _] when special_form in @alias_only_atoms ->
            :alias_only

          [cursor | tail] ->
            case remove_operators(tail, cursor) do
              [{:"::", _, [_, _]}, {:<<>>, _, [_ | _]} | _] -> :bitstring_modifier
              _ -> nil
            end

          _ ->
            nil
        end

      {:error, _} ->
        nil
    end
  end

  defp remove_operators([{op, _, [_, previous]} = head | tail], previous) when op in [:-],
    do: remove_operators(tail, head)

  defp remove_operators(tail, _previous),
    do: tail

  defp container_context_struct(cursor, pairs, aliases, ctx) do
    with {pairs, [^cursor]} <- Enum.split(pairs, -1),
         alias = expand_alias(aliases, ctx),
         true <- Keyword.keyword?(pairs) and has_struct?(alias) do
      {:struct, alias, pairs}
    else
      _ -> nil
    end
  end

  defp container_context_map(cursor, pairs, variable, ctx) do
    with {pairs, [^cursor]} <- Enum.split(pairs, -1),
         {:ok, map} when is_map(map) <- value_from_binding([variable], ctx),
         true <- Keyword.keyword?(pairs) do
      {:map, map, pairs}
    else
      _ -> nil
    end
  end

  defp container_context_map_fields(pairs, map, hint, ctx) do
    map = filter_out_fields(map, pairs)

    for {key, _value} <- map,
        name = Atom.to_string(key),
        ctx.matcher.(name, hint),
        do: %{kind: :in_map_field, name: key}
  end

  defp container_context_struct_fields(pairs, mod, hint, ctx) do
    map = Map.from_struct(mod.__struct__())
    map = filter_out_fields(map, pairs)

    for {field, default} <- map,
        name = Atom.to_string(field),
        ctx.matcher.(name, hint),
        do: %{kind: :in_struct_field, struct: mod, name: field, default: default}
  end

  defp filter_out_fields(map, pairs) do
    # Remove the keys that have already been filled, and internal keys
    map
    |> Map.drop(Keyword.keys(pairs))
    |> Map.reject(fn {key, _} ->
      key
      |> Atom.to_string()
      |> String.starts_with?("_")
    end)
  end

  defp match_local_or_var(hint, ctx) do
    match_local(hint, ctx) ++ match_variable(hint, ctx)
  end

  defp match_local(hint, ctx) do
    imports =
      ctx.intellisense_context.env
      |> imports_from_env()
      |> Enum.flat_map(fn {mod, funs} ->
        match_module_function(mod, hint, ctx, funs)
      end)

    special_forms = match_module_function(Kernel.SpecialForms, hint, ctx)

    imports ++ special_forms
  end

  defp match_variable(hint, ctx) do
    for {var, nil} <- Macro.Env.vars(ctx.intellisense_context.env),
        name = Atom.to_string(var),
        ctx.matcher.(name, hint),
        do: %{kind: :variable, name: var}
  end

  defp match_map_field(map, hint, ctx) do
    # Note: we need Map.to_list/1 in case this is a struct
    for {key, _value} <- Map.to_list(map),
        is_atom(key),
        name = Atom.to_string(key),
        ctx.matcher.(name, hint),
        do: %{kind: :map_field, name: key}
  end

  defp match_sigil(hint, ctx) do
    for %{kind: :function, display_name: "sigil_" <> sigil_name} = item <-
          match_local("sigil_", %{ctx | matcher: @prefix_matcher}),
        ctx.matcher.(sigil_name, hint),
        do: %{item | display_name: "~" <> sigil_name}
  end

  defp match_erlang_module(hint, ctx) do
    for mod <- get_matching_modules(hint, ctx),
        usable_as_unquoted_module?(mod),
        name = ":" <> Atom.to_string(mod),
        do: %{
          kind: :module,
          module: mod,
          display_name: name,
          documentation: Intellisense.Docs.get_module_documentation(mod, ctx.node)
        }
  end

  # Converts alias string to module atom with regard to the given env
  defp expand_alias(alias, ctx) when is_binary(alias) do
    alias
    |> String.split(".")
    |> Enum.map(&String.to_atom/1)
    |> expand_alias(ctx)
  end

  defp expand_alias([_ | _] = parts, ctx) do
    Macro.expand({:__aliases__, [], parts}, ctx.intellisense_context.env)
  end

  defp match_env_alias(hint, ctx) do
    for {alias, mod} <- ctx.intellisense_context.env.aliases,
        [name] = Module.split(alias),
        ctx.matcher.(name, hint),
        do: %{
          kind: :module,
          module: mod,
          display_name: name,
          documentation: Intellisense.Docs.get_module_documentation(mod, ctx.node)
        }
  end

  defp match_module(base_mod, hint, nested?, ctx) do
    # Note: we specifically don't want further completion
    # if `base_mod` is an Erlang module.

    if base_mod == Elixir or elixir_module?(base_mod) do
      match_elixir_module(base_mod, hint, nested?, ctx)
    else
      []
    end
  end

  defp elixir_module?(mod) do
    mod |> Atom.to_string() |> String.starts_with?("Elixir.")
  end

  defp match_elixir_root_module(hint, nested?, ctx) do
    items = match_elixir_module(Elixir, hint, nested?, ctx)

    # `Elixir` is not a existing module name, but `Elixir.Enum` is,
    # so if the user types `Eli` the completion should include `Elixir`.
    if ctx.matcher.("Elixir", hint) do
      [%{kind: :module, module: Elixir, display_name: "Elixir", documentation: nil} | items]
    else
      items
    end
  end

  defp match_elixir_module(base_mod, hint, nested?, ctx) do
    # Note: `base_mod` may be `Elixir`, even though it's not a valid module

    match_prefix = "#{base_mod}.#{hint}"
    depth = match_prefix |> Module.split() |> length()

    for mod <- get_matching_modules(match_prefix, ctx),
        parts = Module.split(mod),
        length(parts) >= depth,
        {parent_mod_parts, name_parts} = Enum.split(parts, depth - 1),
        name_parts = if(nested?, do: name_parts, else: [hd(name_parts)]),
        name = Enum.join(name_parts, "."),
        # Note: module can be defined dynamically and its name
        # may not be a valid alias (e.g. :"Elixir.My.module").
        # That's why we explicitly check if the name part makes
        # for a valid alias piece.
        valid_alias_piece?("." <> name),
        mod = Module.concat(parent_mod_parts ++ name_parts),
        uniq: true,
        do: %{
          kind: :module,
          module: mod,
          display_name: name,
          documentation: Intellisense.Docs.get_module_documentation(mod, ctx.node)
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
    Macro.classify_atom(mod) in [:identifier, :unquoted]
  end

  defp get_matching_modules(hint, ctx) do
    ctx
    |> get_modules()
    |> Enum.filter(&ctx.matcher.(Atom.to_string(&1), hint))
    |> Enum.uniq()
  end

  defp get_modules(%{node: node} = ctx) do
    # On interactive mode, we load modules from the application
    # and then the ones from runtime. For a remote node, ideally
    # we would get the applications one, but there is no cheap
    # way to do such, so we get :code.all_loaded and cache it
    # instead (which includes all modules anyway on embedded mode).
    if node == node() and :code.get_mode() == :interactive do
      runtime_modules(ctx.intellisense_context.ebin_path) ++ get_modules_from_applications()
    else
      cached_all_loaded(node)
    end
  end

  defp runtime_modules(path) do
    with true <- is_binary(path),
         {:ok, beams} <- File.ls(path) do
      for beam <- beams, String.ends_with?(beam, ".beam") do
        beam
        |> binary_slice(0..-6//1)
        |> String.to_atom()
      end
    else
      _ -> []
    end
  end

  defp get_modules_from_applications() do
    # If we invoke :application.loaded_applications/0,
    # it can error if we don't call safe_fixtable before.
    # Since in both cases we are reaching over the
    # application controller internals, we choose to match
    # for performance.
    for [app] <- :ets.match(:ac_tab, {{:loaded, :"$1"}, :_}),
        {:ok, modules} = :application.get_key(app, :modules),
        module <- modules,
        do: module
  end

  defp match_module_function(mod, hint, ctx, funs \\ nil) do
    if ensure_loaded?(mod, ctx.node) do
      funs = funs || exports(mod, ctx.node)

      matching_funs =
        Enum.filter(funs, fn {name, _arity, _type} ->
          name = Atom.to_string(name)
          ctx.matcher.(name, hint)
        end)

      doc_items =
        Intellisense.Docs.lookup_module_members(
          mod,
          Enum.map(matching_funs, &Tuple.delete_at(&1, 2)),
          ctx.node,
          kinds: [:function, :macro]
        )

      Enum.map(matching_funs, fn {name, arity, type} ->
        doc_item =
          Enum.find(
            doc_items,
            %{from_default: false, documentation: nil, signatures: [], specs: [], meta: %{}},
            fn doc_item ->
              doc_item.name == name && doc_item.arity == arity
            end
          )

        %{
          kind: :function,
          module: mod,
          name: name,
          arity: arity,
          type: type,
          display_name: Atom.to_string(name),
          from_default: doc_item.from_default,
          documentation: doc_item.documentation,
          signatures: doc_item.signatures,
          specs: doc_item.specs,
          meta: doc_item.meta
        }
      end)
    else
      []
    end
  end

  defp exports(mod, node) do
    try do
      :erpc.call(node, mod, :module_info, [:exports])
    rescue
      _ -> []
    else
      exports ->
        for {fun, arity} <- exports,
            not reflection?(fun, arity),
            do: function_or_macro(Atom.to_string(fun), fun, arity)
    end
  end

  defp reflection?(:module_info, 0), do: true
  defp reflection?(:module_info, 1), do: true
  defp reflection?(:__info__, 1), do: true
  defp reflection?(_, _), do: false

  defp function_or_macro("MACRO-" <> name, _, arity),
    do: {String.to_atom(name), arity - 1, :macro}

  defp function_or_macro(_, fun, arity), do: {fun, arity, :function}

  defp append_funs_type(funs, type) do
    Enum.map(funs, fn {name, arity} -> {name, arity, type} end)
  end

  defp match_module_type(mod, hint, ctx) do
    types = get_module_types(mod, ctx.node)

    matching_types =
      Enum.filter(types, fn {name, _arity} ->
        name = Atom.to_string(name)
        ctx.matcher.(name, hint)
      end)

    doc_items =
      Intellisense.Docs.lookup_module_members(mod, matching_types, ctx.node, kinds: [:type])

    Enum.map(matching_types, fn {name, arity} ->
      doc_item =
        Enum.find(doc_items, %{documentation: nil, type_spec: nil}, fn doc_item ->
          doc_item.name == name && doc_item.arity == arity
        end)

      %{
        kind: :type,
        module: mod,
        name: name,
        arity: arity,
        documentation: doc_item.documentation,
        type_spec: doc_item.type_spec
      }
    end)
  end

  defp get_module_types(mod, node) do
    with true <- ensure_loaded?(mod, node),
         {:ok, types} <- :erpc.call(node, Code.Typespec, :fetch_types, [mod]) do
      for {kind, {name, _, args}} <- types, kind in [:type, :opaque] do
        {name, length(args)}
      end
    else
      _ -> []
    end
  end

  # Skip Elixir to avoid warnings
  defp ensure_loaded?(Elixir, _node), do: false
  # Remote nodes only have loaded modules
  defp ensure_loaded?(_mod, node) when node != node(), do: true
  defp ensure_loaded?(mod, _node), do: Code.ensure_loaded?(mod)

  defp imports_from_env(env) do
    Enum.map(env.functions, fn {mod, funs} ->
      {mod, append_funs_type(funs, :function)}
    end) ++
      Enum.map(env.macros, fn {mod, funs} ->
        {mod, append_funs_type(funs, :macro)}
      end)
  end

  defp split_at_last_occurrence(string, pattern) do
    case :binary.matches(string, pattern) do
      [] ->
        :error

      parts ->
        {start, length} = List.last(parts)
        <<left::binary-size(start), _::binary-size(length), right::binary>> = string
        {:ok, left, right}
    end
  end

  defp match_module_attribute(hint, ctx) do
    for {attribute, info} <- Module.reserved_attributes(),
        name = Atom.to_string(attribute),
        ctx.matcher.(name, hint),
        do: %{
          kind: :module_attribute,
          name: attribute,
          documentation: {"text/markdown", info.doc}
        }
  end
end
