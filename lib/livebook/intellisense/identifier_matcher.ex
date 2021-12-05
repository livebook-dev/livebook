defmodule Livebook.Intellisense.IdentifierMatcher do
  @moduledoc false

  # This module allows for extracting information about
  # identifiers based on code and runtime information
  # (binding, environment).
  #
  # This functionality is a basic building block to be
  # used for code completion and information extraction.
  #
  # The implementation is based primarily on `IEx.Autocomplete`.
  # It also takes insights from `ElixirSense.Providers.Suggestion.Complete`,
  # which is a very extensive implementation used in the
  # Elixir Language Server.

  alias Livebook.Intellisense.Docs

  @typedoc """
  A single identifier together with relevant information.
  """
  @type identifier_item ::
          {:variable, name(), value()}
          | {:map_field, name(), value()}
          | {:module, module(), display_name(), Docs.documentation()}
          | {:function, module(), name(), arity(), function_type(), display_name(),
             Docs.documentation(), list(Docs.signature()), list(Docs.spec())}
          | {:type, module(), name(), arity(), Docs.documentation()}
          | {:module_attribute, name(), Docs.documentation()}

  @type name :: atom()
  @type display_name :: String.t()
  @type value :: term()
  @type function_type :: :function | :macro

  @exact_matcher &Kernel.==/2
  @prefix_matcher &String.starts_with?/2

  @doc """
  Returns a list of identifiers matching the given `hint`
  together with relevant information.

  Evaluation binding and environment is used to expand aliases,
  imports, nested maps, etc.

  `hint` may be a single token or line fragment like `if Enum.m`.
  """
  @spec completion_identifiers(String.t(), Code.binding(), Macro.Env.t()) ::
          list(identifier_item())
  def completion_identifiers(hint, binding, env) do
    context = Code.Fragment.cursor_context(hint)
    ctx = %{binding: binding, env: env, matcher: @prefix_matcher}
    context_to_matches(context, ctx, :completion)
  end

  @doc """
  Extracts information about an identifier found in `column`
  in `line`.

  The function returns range of columns where the identifier
  is located and a list of matching identifier items.
  """
  @spec locate_identifier(String.t(), pos_integer(), Code.binding(), Macro.Env.t()) ::
          %{
            matches: list(identifier_item()),
            range: nil | %{from: pos_integer(), to: pos_integer()}
          }
  def locate_identifier(line, column, binding, env) do
    case Code.Fragment.surround_context(line, {1, column}) do
      %{context: context, begin: {_, from}, end: {_, to}} ->
        ctx = %{binding: binding, env: env, matcher: @exact_matcher}
        matches = context_to_matches(context, ctx, :locate)
        %{matches: matches, range: %{from: from, to: to}}

      :none ->
        %{matches: [], range: nil}
    end
  end

  # Takes a context returned from Code.Fragment.cursor_context
  # or Code.Fragment.surround_context and looks up matching
  # identifier items
  defp context_to_matches(context, ctx, type) do
    case context do
      {:alias, alias} ->
        match_alias(List.to_string(alias), ctx, false)

      {:unquoted_atom, unquoted_atom} ->
        match_erlang_module(List.to_string(unquoted_atom), ctx)

      {:dot, path, hint} ->
        match_dot(path, List.to_string(hint), ctx)

      {:dot_arity, path, hint} ->
        match_dot(path, List.to_string(hint), %{ctx | matcher: @exact_matcher})

      {:dot_call, _path, _hint} ->
        match_default(ctx)

      :expr ->
        match_default(ctx)

      {:local_or_var, local_or_var} ->
        match_local_or_var(List.to_string(local_or_var), ctx)

      {:local_arity, local} ->
        match_local(List.to_string(local), %{ctx | matcher: @exact_matcher})

      {:local_call, local} ->
        case type do
          :completion -> match_default(ctx)
          :locate -> match_local(List.to_string(local), %{ctx | matcher: @exact_matcher})
        end

      {:operator, operator} ->
        match_local_or_var(List.to_string(operator), ctx)

      {:operator_arity, operator} ->
        match_local(List.to_string(operator), %{ctx | matcher: @exact_matcher})

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

  defp expand_dot_path({:var, var}, ctx) do
    Keyword.fetch(ctx.binding, List.to_atom(var))
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

  defp expand_dot_path({:dot, parent, call}, ctx) do
    case expand_dot_path(parent, ctx) do
      {:ok, %{} = map} -> Map.fetch(map, List.to_atom(call))
      _ -> :error
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

  defp match_struct(hint, ctx) do
    for {:module, module, name, documentation} <- match_alias(hint, ctx, true),
        has_struct?(module),
        do: {:module, module, name, documentation}
  end

  defp has_struct?(mod) do
    Code.ensure_loaded?(mod) and function_exported?(mod, :__struct__, 1) and
      not function_exported?(mod, :exception, 1)
  end

  defp match_module_member(mod, hint, ctx) do
    match_module_function(mod, hint, ctx) ++ match_module_type(mod, hint, ctx)
  end

  defp match_local_or_var(hint, ctx) do
    match_local(hint, ctx) ++ match_variable(hint, ctx)
  end

  defp match_local(hint, ctx) do
    imports =
      ctx.env
      |> imports_from_env()
      |> Enum.flat_map(fn {mod, funs} ->
        match_module_function(mod, hint, ctx, funs)
      end)

    special_forms = match_module_function(Kernel.SpecialForms, hint, ctx)

    imports ++ special_forms
  end

  defp match_variable(hint, ctx) do
    for {key, value} <- ctx.binding,
        is_atom(key),
        name = Atom.to_string(key),
        ctx.matcher.(name, hint),
        do: {:variable, name, value}
  end

  defp match_map_field(map, hint, ctx) do
    # Note: we need Map.to_list/1 in case this is a struct
    for {key, value} <- Map.to_list(map),
        is_atom(key),
        name = Atom.to_string(key),
        ctx.matcher.(name, hint),
        do: {:map_field, name, value}
  end

  defp match_sigil(hint, ctx) do
    for {:function, module, name, arity, type, "sigil_" <> sigil_name, documentation, signatures,
         specs} <-
          match_local("sigil_", %{ctx | matcher: @prefix_matcher}),
        ctx.matcher.(sigil_name, hint),
        do:
          {:function, module, name, arity, type, "~" <> sigil_name, documentation, signatures,
           specs}
  end

  defp match_erlang_module(hint, ctx) do
    for mod <- get_matching_modules(hint, ctx),
        usable_as_unquoted_module?(mod),
        name = ":" <> Atom.to_string(mod),
        do: {:module, mod, name, Livebook.Intellisense.Docs.get_module_documentation(mod)}
  end

  # Converts alias string to module atom with regard to the given env
  defp expand_alias(alias, ctx) do
    [name | rest] = alias |> String.split(".") |> Enum.map(&String.to_atom/1)

    case Keyword.fetch(ctx.env.aliases, Module.concat(Elixir, name)) do
      {:ok, name} when rest == [] -> name
      {:ok, name} -> Module.concat([name | rest])
      :error -> Module.concat([name | rest])
    end
  end

  defp match_env_alias(hint, ctx) do
    for {alias, mod} <- ctx.env.aliases,
        [name] = Module.split(alias),
        ctx.matcher.(name, hint),
        do: {:module, mod, name, Livebook.Intellisense.Docs.get_module_documentation(mod)}
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
      [{:module, Elixir, "Elixir", nil} | items]
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
        do: {:module, mod, name, Livebook.Intellisense.Docs.get_module_documentation(mod)}
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

  defp get_matching_modules(hint, ctx) do
    get_modules()
    |> Enum.filter(&ctx.matcher.(Atom.to_string(&1), hint))
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

  defp match_module_function(mod, hint, ctx, funs \\ nil) do
    if ensure_loaded?(mod) do
      funs = funs || exports(mod)

      matching_funs =
        Enum.filter(funs, fn {name, _arity, _type} ->
          name = Atom.to_string(name)
          ctx.matcher.(name, hint)
        end)

      doc_items =
        Livebook.Intellisense.Docs.lookup_module_members(
          mod,
          Enum.map(matching_funs, &Tuple.delete_at(&1, 2)),
          kinds: [:function, :macro]
        )

      Enum.map(matching_funs, fn {name, arity, type} ->
        doc_item =
          Enum.find(doc_items, %{documentation: nil, signatures: [], specs: []}, fn doc_item ->
            doc_item.name == name && doc_item.arity == arity
          end)

        {:function, mod, name, arity, type, Atom.to_string(name),
         doc_item && doc_item.documentation, doc_item.signatures, doc_item.specs}
      end)
    else
      []
    end
  end

  defp exports(mod) do
    if Code.ensure_loaded?(mod) and function_exported?(mod, :__info__, 1) do
      macros = mod.__info__(:macros)
      functions = mod.__info__(:functions) -- [__info__: 1]
      append_funs_type(macros, :macro) ++ append_funs_type(functions, :function)
    else
      functions = mod.module_info(:exports) -- [module_info: 0, module_info: 1]
      append_funs_type(functions, :function)
    end
  end

  defp append_funs_type(funs, type) do
    Enum.map(funs, &Tuple.append(&1, type))
  end

  defp match_module_type(mod, hint, ctx) do
    types = get_module_types(mod)

    matching_types =
      Enum.filter(types, fn {name, _arity} ->
        name = Atom.to_string(name)
        ctx.matcher.(name, hint)
      end)

    doc_items =
      Livebook.Intellisense.Docs.lookup_module_members(mod, matching_types, kinds: [:type])

    Enum.map(matching_types, fn {name, arity} ->
      doc_item =
        Enum.find(doc_items, %{documentation: nil}, fn doc_item ->
          doc_item.name == name && doc_item.arity == arity
        end)

      {:type, mod, Atom.to_string(name), arity, doc_item.documentation}
    end)
  end

  defp get_module_types(mod) do
    with true <- ensure_loaded?(mod),
         {:ok, types} <- Code.Typespec.fetch_types(mod) do
      for {kind, {name, _, args}} <- types, kind in [:type, :opaque] do
        {name, length(args)}
      end
    else
      _ -> []
    end
  end

  defp ensure_loaded?(Elixir), do: false
  defp ensure_loaded?(mod), do: Code.ensure_loaded?(mod)

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
        {start, _} = List.last(parts)
        size = byte_size(string)
        {:ok, binary_part(string, 0, start), binary_part(string, start + 1, size - start - 1)}
    end
  end

  defp match_module_attribute(hint, ctx) do
    for {attribute, info} <- Module.reserved_attributes(),
        name = Atom.to_string(attribute),
        ctx.matcher.(name, hint),
        do: {:module_attribute, name, {"text/markdown", info.doc}}
  end
end
