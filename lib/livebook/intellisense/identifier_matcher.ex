defmodule Livebook.Intellisense.IdentifierMatcher do
  @moduledoc false

  # This module allows for extracting information about
  # identifiers based on code and runtime information
  # (binding, environment).
  #
  # This functionality is a basic building block to be
  # used for code completion and information extraction.
  #
  # The implementation is based primarly on `IEx.Autocomplete`.
  # It also takes insights from `ElixirSense.Providers.Suggestion.Complete`,
  # which is a very extensive implementation used in the
  # Elixir Language Server.

  @typedoc """
  A single identifier together with relevant information.
  """
  @type identifier_item ::
          {:variable, name(), value()}
          | {:map_field, name(), value()}
          | {:module, name(), doc_content()}
          | {:function, module(), name(), arity(), doc_content(), list(signature()), spec()}
          | {:type, module(), name(), arity(), doc_content()}
          | {:module_attribute, name(), doc_content()}

  @type name :: String.t()
  @type value :: term()
  @type doc_content :: {format :: String.t(), content :: String.t()} | nil
  @type signature :: String.t()
  @type spec :: tuple() | nil

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
    context = cursor_context(hint)
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
    case surround_context(line, {1, column}) do
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
        match_alias(List.to_string(alias), ctx)

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

      # :none
      _ ->
        []
    end
  end

  defp match_dot(path, hint, ctx) do
    case expand_dot_path(path, ctx) do
      {:ok, mod} when is_atom(mod) and hint == "" ->
        match_module_member(mod, hint, ctx) ++ match_module(mod, hint, ctx)

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

  defp match_alias(hint, ctx) do
    case split_at_last_occurrence(hint, ".") do
      {hint, ""} ->
        match_elixir_root_module(hint, ctx) ++ match_env_alias(hint, ctx)

      {alias, hint} ->
        mod = expand_alias(alias, ctx)
        match_module(mod, hint, ctx)
    end
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

  defp match_erlang_module(hint, ctx) do
    for mod <- get_matching_modules(hint, ctx),
        usable_as_unquoted_module?(mod),
        name = ":" <> Atom.to_string(mod),
        do: {:module, name, get_module_doc_content(mod)}
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
        do: {:module, name, get_module_doc_content(mod)}
  end

  defp match_module(base_mod, hint, ctx) do
    # Note: we specifically don't want further completion
    # if `base_mod` is an Erlang module.

    if base_mod == Elixir or elixir_module?(base_mod) do
      match_elixir_module(base_mod, hint, ctx)
    else
      []
    end
  end

  defp elixir_module?(mod) do
    mod |> Atom.to_string() |> String.starts_with?("Elixir.")
  end

  defp match_elixir_root_module(hint, ctx) do
    items = match_elixir_module(Elixir, hint, ctx)

    # `Elixir` is not a existing module name, but `Elixir.Enum` is,
    # so if the user types `Eli` the completion should include `Elixir`.
    if ctx.matcher.("Elixir", hint) do
      [{:module, "Elixir", nil} | items]
    else
      items
    end
  end

  defp match_elixir_module(base_mod, hint, ctx) do
    # Note: `base_mod` may be `Elixir`, even though it's not a valid module

    match_prefix = "#{base_mod}.#{hint}"
    depth = match_prefix |> Module.split() |> length()

    for mod <- get_matching_modules(match_prefix, ctx),
        parts = Module.split(mod),
        length(parts) >= depth,
        name = Enum.at(parts, depth - 1),
        # Note: module can be defined dynamically and its name
        # may not be a valid alias (e.g. :"Elixir.My.module").
        # That's why we explicitly check if the name part makes
        # for a alias piece.
        valid_alias_piece?("." <> name),
        mod = parts |> Enum.take(depth) |> Module.concat(),
        uniq: true,
        do: {:module, name, get_module_doc_content(mod)}
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
      {format, docs} = get_docs(mod, [:function, :macro])
      specs = get_specs(mod)
      funs = funs || exports(mod)
      funs_with_base_arity = funs_with_base_arity(docs)

      funs
      |> Enum.filter(fn {name, _arity} ->
        name = Atom.to_string(name)
        ctx.matcher.(name, hint)
      end)
      |> Enum.map(fn {name, arity} ->
        base_arity = Map.get(funs_with_base_arity, {name, arity}, arity)
        doc = find_doc(docs, {name, base_arity})
        spec = find_spec(specs, {name, base_arity})

        doc_content = doc_content(doc, format)
        signatures = doc_signatures(doc)

        {:function, mod, Atom.to_string(name), arity, doc_content, signatures, spec}
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
      {:docs_v1, _, _, format, _, _, docs} ->
        docs = for {{kind, _, _}, _, _, _, _} = doc <- docs, kind in kinds, do: doc
        {format, docs}

      _ ->
        {nil, []}
    end
  end

  defp get_module_doc_content(mod) do
    case Code.fetch_docs(mod) do
      {:docs_v1, _, _, format, %{"en" => docstring}, _, _} ->
        {format, docstring}

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

  defp doc_content({_, _, _, %{"en" => docstr}, _}, format), do: {format, docstr}
  defp doc_content(_doc, _format), do: nil

  defp exports(mod) do
    if Code.ensure_loaded?(mod) and function_exported?(mod, :__info__, 1) do
      mod.__info__(:macros) ++ (mod.__info__(:functions) -- [__info__: 1])
    else
      mod.module_info(:exports) -- [module_info: 0, module_info: 1]
    end
  end

  defp match_module_type(mod, hint, ctx) do
    {format, docs} = get_docs(mod, [:type])
    types = get_module_types(mod)

    types
    |> Enum.filter(fn {name, _arity} ->
      name = Atom.to_string(name)
      ctx.matcher.(name, hint)
    end)
    |> Enum.map(fn {name, arity} ->
      doc = find_doc(docs, {name, arity})
      doc_content = doc_content(doc, format)

      {:type, mod, Atom.to_string(name), arity, doc_content}
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

  defp imports_from_env(env), do: env.functions ++ env.macros

  defp split_at_last_occurrence(string, pattern) do
    case :binary.matches(string, pattern) do
      [] ->
        {string, ""}

      parts ->
        {start, _} = List.last(parts)
        size = byte_size(string)
        {binary_part(string, 0, start), binary_part(string, start + 1, size - start - 1)}
    end
  end

  defp match_module_attribute(hint, ctx) do
    for {attribute, info} <- Module.reserved_attributes(),
        name = Atom.to_string(attribute),
        ctx.matcher.(name, hint),
        do: {:module_attribute, name, {"text/markdown", info.doc}}
  end

  # ---
  # Source of Code.Fragment

  # TODO: no longer required on Elixir v1.13

  # This module provides conveniences for analyzing fragments of
  # textual code and extract available information whenever possible.

  # Most of the functions in this module provide a best-effort
  # and may not be accurate under all circumstances. Read each
  # documentation for more information.

  # This module should be considered experimental.

  @type position :: {line :: pos_integer(), column :: pos_integer()}

  @doc """
  Receives a string and returns the cursor context.

  This function receives a string with an Elixir code fragment,
  representing a cursor position, and based on the string, it
  provides contextual information about said position. The
  return of this function can then be used to provide tips,
  suggestions, and autocompletion functionality.

  This function provides a best-effort detection and may not be
  accurate under all circumstances. See the "Limitations"
  section below.

  Consider adding a catch-all clause when handling the return
  type of this function as new cursor information may be added
  in future releases.

  ## Examples

      iex> Code.Fragment.cursor_context("")
      :expr

      iex> Code.Fragment.cursor_context("hello_wor")
      {:local_or_var, 'hello_wor'}

  ## Return values

    * `{:alias, charlist}` - the context is an alias, potentially
      a nested one, such as `Hello.Wor` or `HelloWor`

    * `{:dot, inside_dot, charlist}` - the context is a dot
      where `inside_dot` is either a `{:var, charlist}`, `{:alias, charlist}`,
      `{:module_attribute, charlist}`, `{:unquoted_atom, charlist}` or a `dot`
      itself. If a var is given, this may either be a remote call or a map
      field access. Examples are `Hello.wor`, `:hello.wor`, `hello.wor`,
      `Hello.nested.wor`, `hello.nested.wor`, and `@hello.world`

    * `{:dot_arity, inside_dot, charlist}` - the context is a dot arity
      where `inside_dot` is either a `{:var, charlist}`, `{:alias, charlist}`,
      `{:module_attribute, charlist}`, `{:unquoted_atom, charlist}` or a `dot`
      itself. If a var is given, it must be a remote arity. Examples are
      `Hello.world/`, `:hello.world/`, `hello.world/2`, and `@hello.world/2`

    * `{:dot_call, inside_dot, charlist}` - the context is a dot
      call. This means parentheses or space have been added after the expression.
      where `inside_dot` is either a `{:var, charlist}`, `{:alias, charlist}`,
      `{:module_attribute, charlist}`, `{:unquoted_atom, charlist}` or a `dot`
      itself. If a var is given, it must be a remote call. Examples are
      `Hello.world(`, `:hello.world(`, `Hello.world `, `hello.world(`, `hello.world `,
      and `@hello.world(`

    * `:expr` - may be any expression. Autocompletion may suggest an alias,
      local or var

    * `{:local_or_var, charlist}` - the context is a variable or a local
      (import or local) call, such as `hello_wor`

    * `{:local_arity, charlist}` - the context is a local (import or local)
      arity, such as `hello_world/`

    * `{:local_call, charlist}` - the context is a local (import or local)
      call, such as `hello_world(` and `hello_world `

    * `{:module_attribute, charlist}` - the context is a module attribute, such
      as `@hello_wor`

    * `{:operator, charlist}` (since v1.13.0) - the context is an operator,
      such as `+` or `==`. Note textual operators, such as `when` do not
      appear as operators but rather as `:local_or_var`. `@` is never an
      `:operator` and always a `:module_attribute`

    * `{:operator_arity, charlist}` (since v1.13.0)  - the context is an
      operator arity, which is an operator followed by /, such as `+/`,
      `not/` or `when/`

    * `{:operator_call, charlist}` (since v1.13.0)  - the context is an
      operator call, which is an operator followed by space, such as
      `left + `, `not ` or `x when `

    * `:none` - no context possible

    * `{:unquoted_atom, charlist}` - the context is an unquoted atom. This
      can be any atom or an atom representing a module

  ## Limitations

    * The current algorithm only considers the last line of the input
    * Context does not yet track strings and sigils
    * Arguments of functions calls are not currently recognized

  """
  @doc since: "1.13.0"
  @spec cursor_context(List.Chars.t(), keyword()) ::
          {:alias, charlist}
          | {:dot, inside_dot, charlist}
          | {:dot_arity, inside_dot, charlist}
          | {:dot_call, inside_dot, charlist}
          | :expr
          | {:local_or_var, charlist}
          | {:local_arity, charlist}
          | {:local_call, charlist}
          | {:module_attribute, charlist}
          | {:operator, charlist}
          | {:operator_arity, charlist}
          | {:operator_call, charlist}
          | :none
          | {:unquoted_atom, charlist}
        when inside_dot:
               {:alias, charlist}
               | {:dot, inside_dot, charlist}
               | {:module_attribute, charlist}
               | {:unquoted_atom, charlist}
               | {:var, charlist}
  def cursor_context(fragment, opts \\ [])

  def cursor_context(binary, opts) when is_binary(binary) and is_list(opts) do
    binary =
      case :binary.matches(binary, "\n") do
        [] ->
          binary

        matches ->
          {position, _} = List.last(matches)
          binary_part(binary, position + 1, byte_size(binary) - position - 1)
      end

    binary
    |> String.to_charlist()
    |> :lists.reverse()
    |> codepoint_cursor_context(opts)
    |> elem(0)
  end

  def cursor_context(charlist, opts) when is_list(charlist) and is_list(opts) do
    charlist =
      case charlist |> Enum.chunk_by(&(&1 == ?\n)) |> List.last([]) do
        [?\n | _] -> []
        rest -> rest
      end

    charlist
    |> :lists.reverse()
    |> codepoint_cursor_context(opts)
    |> elem(0)
  end

  def cursor_context(other, opts) when is_list(opts) do
    cursor_context(to_charlist(other), opts)
  end

  @operators '\\<>+-*/:=|&~^%!'
  @starter_punctuation ',([{;'
  @non_starter_punctuation ')]}"\'.$'
  @space '\t\s'
  @trailing_identifier '?!'

  @non_identifier @trailing_identifier ++
                    @operators ++ @starter_punctuation ++ @non_starter_punctuation ++ @space

  @textual_operators ~w(when not and or in)c

  defp codepoint_cursor_context(reverse, _opts) do
    {stripped, spaces} = strip_spaces(reverse, 0)

    case stripped do
      # It is empty
      [] -> {:expr, 0}
      # Token/AST only operators
      [?>, ?= | rest] when rest == [] or hd(rest) != ?: -> {:expr, 0}
      [?>, ?- | rest] when rest == [] or hd(rest) != ?: -> {:expr, 0}
      # Two-digit containers
      [?<, ?< | rest] when rest == [] or hd(rest) != ?< -> {:expr, 0}
      # Ambiguity around :
      [?: | rest] when rest == [] or hd(rest) != ?: -> unquoted_atom_or_expr(spaces)
      # Dots
      [?.] -> {:none, 0}
      [?. | rest] when hd(rest) not in '.:' -> dot(rest, spaces + 1, '')
      # It is a local or remote call with parens
      [?( | rest] -> call_to_cursor_context(strip_spaces(rest, spaces + 1))
      # A local arity definition
      [?/ | rest] -> arity_to_cursor_context(strip_spaces(rest, spaces + 1))
      # Starting a new expression
      [h | _] when h in @starter_punctuation -> {:expr, 0}
      # It is a local or remote call without parens
      rest when spaces > 0 -> call_to_cursor_context({rest, spaces})
      # It is an identifier
      _ -> identifier_to_cursor_context(reverse, 0, false)
    end
  end

  defp strip_spaces([h | rest], count) when h in @space, do: strip_spaces(rest, count + 1)
  defp strip_spaces(rest, count), do: {rest, count}

  defp unquoted_atom_or_expr(0), do: {{:unquoted_atom, ''}, 1}
  defp unquoted_atom_or_expr(_), do: {:expr, 0}

  defp arity_to_cursor_context({reverse, spaces}) do
    case identifier_to_cursor_context(reverse, spaces, true) do
      {{:local_or_var, acc}, count} -> {{:local_arity, acc}, count}
      {{:dot, base, acc}, count} -> {{:dot_arity, base, acc}, count}
      {{:operator, acc}, count} -> {{:operator_arity, acc}, count}
      {_, _} -> {:none, 0}
    end
  end

  defp call_to_cursor_context({reverse, spaces}) do
    case identifier_to_cursor_context(reverse, spaces, true) do
      {{:local_or_var, acc}, count} -> {{:local_call, acc}, count}
      {{:dot, base, acc}, count} -> {{:dot_call, base, acc}, count}
      {{:operator, acc}, count} -> {{:operator_call, acc}, count}
      {_, _} -> {:none, 0}
    end
  end

  defp identifier_to_cursor_context([?., ?., ?: | _], n, _), do: {{:unquoted_atom, '..'}, n + 3}
  defp identifier_to_cursor_context([?., ?., ?. | _], n, _), do: {{:local_or_var, '...'}, n + 3}
  defp identifier_to_cursor_context([?., ?: | _], n, _), do: {{:unquoted_atom, '.'}, n + 2}
  defp identifier_to_cursor_context([?., ?. | _], n, _), do: {{:operator, '..'}, n + 2}

  defp identifier_to_cursor_context(reverse, count, call_op?) do
    case identifier(reverse, count) do
      :none ->
        {:none, 0}

      :operator ->
        operator(reverse, count, [], call_op?)

      {:module_attribute, acc, count} ->
        {{:module_attribute, acc}, count}

      {:unquoted_atom, acc, count} ->
        {{:unquoted_atom, acc}, count}

      {:alias, rest, acc, count} ->
        case strip_spaces(rest, count) do
          {'.' ++ rest, count} when rest == [] or hd(rest) != ?. ->
            nested_alias(rest, count + 1, acc)

          _ ->
            {{:alias, acc}, count}
        end

      {:identifier, _, acc, count} when call_op? and acc in @textual_operators ->
        {{:operator, acc}, count}

      {:identifier, rest, acc, count} ->
        case strip_spaces(rest, count) do
          {'.' ++ rest, count} when rest == [] or hd(rest) != ?. ->
            dot(rest, count + 1, acc)

          _ ->
            {{:local_or_var, acc}, count}
        end
    end
  end

  defp identifier([?? | rest], count), do: check_identifier(rest, count + 1, [??])
  defp identifier([?! | rest], count), do: check_identifier(rest, count + 1, [?!])
  defp identifier(rest, count), do: check_identifier(rest, count, [])

  defp check_identifier([h | t], count, acc) when h not in @non_identifier,
    do: rest_identifier(t, count + 1, [h | acc])

  defp check_identifier(_, _, _), do: :operator

  defp rest_identifier([h | rest], count, acc) when h not in @non_identifier do
    rest_identifier(rest, count + 1, [h | acc])
  end

  defp rest_identifier(rest, count, [?@ | acc]) do
    case tokenize_identifier(rest, count, acc) do
      {:identifier, _rest, acc, count} -> {:module_attribute, acc, count}
      :none when acc == [] -> {:module_attribute, '', count}
      _ -> :none
    end
  end

  defp rest_identifier([?: | rest], count, acc) when rest == [] or hd(rest) != ?: do
    case String.Tokenizer.tokenize(acc) do
      {_, _, [], _, _, _} -> {:unquoted_atom, acc, count + 1}
      _ -> :none
    end
  end

  defp rest_identifier([?? | _], _count, _acc) do
    :none
  end

  defp rest_identifier(rest, count, acc) do
    tokenize_identifier(rest, count, acc)
  end

  defp tokenize_identifier(rest, count, acc) do
    case String.Tokenizer.tokenize(acc) do
      # Not actually an atom cause rest is not a :
      {:atom, _, _, _, _, _} ->
        :none

      # Aliases must be ascii only
      {:alias, _, _, _, false, _} ->
        :none

      {kind, _, [], _, _, extra} ->
        if ?@ in extra do
          :none
        else
          {kind, rest, acc, count}
        end

      _ ->
        :none
    end
  end

  defp nested_alias(rest, count, acc) do
    {rest, count} = strip_spaces(rest, count)

    case identifier_to_cursor_context(rest, count, true) do
      {{:alias, prev}, count} -> {{:alias, prev ++ '.' ++ acc}, count}
      _ -> {:none, 0}
    end
  end

  defp dot(rest, count, acc) do
    {rest, count} = strip_spaces(rest, count)

    case identifier_to_cursor_context(rest, count, true) do
      {{:local_or_var, var}, count} -> {{:dot, {:var, var}, acc}, count}
      {{:unquoted_atom, _} = prev, count} -> {{:dot, prev, acc}, count}
      {{:alias, _} = prev, count} -> {{:dot, prev, acc}, count}
      {{:dot, _, _} = prev, count} -> {{:dot, prev, acc}, count}
      {{:module_attribute, _} = prev, count} -> {{:dot, prev, acc}, count}
      {_, _} -> {:none, 0}
    end
  end

  defp operator([h | rest], count, acc, call_op?) when h in @operators do
    operator(rest, count + 1, [h | acc], call_op?)
  end

  defp operator(rest, count, acc, call_op?) when acc in ~w(^^ ~~ ~)c do
    {rest, dot_count} = strip_spaces(rest, count)

    cond do
      call_op? ->
        {:none, 0}

      match?([?. | rest] when rest == [] or hd(rest) != ?., rest) ->
        dot(tl(rest), dot_count + 1, acc)

      true ->
        {{:operator, acc}, count}
    end
  end

  defp operator(rest, count, acc, _call_op?) do
    case elixir_tokenizer_tokenize(acc, 1, 1, []) do
      {:ok, _, [{:atom, _, _}]} ->
        {{:unquoted_atom, tl(acc)}, count}

      {:ok, _, [{_, _, op}]} ->
        {rest, dot_count} = strip_spaces(rest, count)

        cond do
          Code.Identifier.unary_op(op) == :error and Code.Identifier.binary_op(op) == :error ->
            :none

          match?([?. | rest] when rest == [] or hd(rest) != ?., rest) ->
            dot(tl(rest), dot_count + 1, acc)

          true ->
            {{:operator, acc}, count}
        end

      _ ->
        {:none, 0}
    end
  end

  @doc """
  Receives a string and returns the surround context.

  This function receives a string with an Elixir code fragment
  and a `position`. It returns a map containing the beginning
  and ending of the expression alongside its context, or `:none`
  if there is nothing with a known context.

  The difference between `cursor_context/2` and `surround_context/3`
  is that the former assumes the expression in the code fragment
  is incomplete. For example, `do` in `cursor_context/2` may be
  a keyword or a variable or a local call, while `surround_context/3`
  assumes the expression in the code fragment is complete, therefore
  `do` would always be a keyword.

  The `position` contains both the `line` and `column`, both starting
  with the index of 1. The column must preceed the surrounding expression.
  For example, the expression `foo`, will return something for the columns
  1, 2, and 3, but not 4:

      foo
      ^ column 1

      foo
       ^ column 2

      foo
        ^ column 3

      foo
         ^ column 4

  The returned map contains the column the expression starts and the
  first column after the expression ends.

  This function builds on top of `cursor_context/2`. Therefore
  it also provides a best-effort detection and may not be accurate
  under all circumstances. See the "Return values" section for more
  information on the available contexts as well as the "Limitations"
  section.

  ## Examples

      iex> Code.Fragment.surround_context("foo", {1, 1})
      %{begin: {1, 1}, context: {:local_or_var, 'foo'}, end: {1, 4}}

  ## Differences to `cursor_context/2`

  In contrast to `cursor_context/2`, `surround_context/3` does not
  return `dot_call`/`dot_arity` nor `operator_call`/`operator_arity`
  contexts because they should behave the same as `dot` and `operator`
  respectively in complete expressions.

  On the other hand, it does make a distinction between `local_call`/
  `local_arity` to `local_or_var`, since the latter can be a local or
  variable.

  Also note that `@` when not followed by any identifier is returned
  as `{:operator, '@'}`, while it is a `{:module_attribute, ''}` in
  `cursor_context/3`. Once again, this happens because `surround_context/3`
  assumes the expression is complete, while `cursor_context/2` does not.
  """
  @doc since: "1.13.0"
  @spec surround_context(List.Chars.t(), position(), keyword()) ::
          %{begin: position, end: position, context: context} | :none
        when context:
               {:alias, charlist}
               | {:dot, inside_dot, charlist}
               | {:local_or_var, charlist}
               | {:local_arity, charlist}
               | {:local_call, charlist}
               | {:module_attribute, charlist}
               | {:operator, charlist}
               | {:unquoted_atom, charlist},
             inside_dot:
               {:alias, charlist}
               | {:dot, inside_dot, charlist}
               | {:module_attribute, charlist}
               | {:unquoted_atom, charlist}
               | {:var, charlist}
  def surround_context(fragment, position, options \\ [])

  def surround_context(binary, {line, column}, opts) when is_binary(binary) do
    binary
    |> String.split("\n")
    |> Enum.at(line - 1, '')
    |> String.to_charlist()
    |> position_surround_context(line, column, opts)
  end

  def surround_context(charlist, {line, column}, opts) when is_list(charlist) do
    charlist
    |> :string.split('\n', :all)
    |> Enum.at(line - 1, '')
    |> position_surround_context(line, column, opts)
  end

  def surround_context(other, position, opts) do
    surround_context(to_charlist(other), position, opts)
  end

  defp position_surround_context(charlist, line, column, opts)
       when is_integer(line) and line >= 1 and is_integer(column) and column >= 1 do
    {reversed_pre, post} = string_reverse_at(charlist, column - 1, [])
    {reversed_pre, post} = adjust_position(reversed_pre, post)

    case take_identifier(post, []) do
      {_, [], _} ->
        maybe_operator(reversed_pre, post, line, opts)

      {:identifier, reversed_post, rest} ->
        {rest, _} = strip_spaces(rest, 0)
        reversed = reversed_post ++ reversed_pre

        case codepoint_cursor_context(reversed, opts) do
          {{:alias, acc}, offset} ->
            build_surround({:alias, acc}, reversed, line, offset)

          {{:dot, _, [_ | _]} = dot, offset} ->
            build_surround(dot, reversed, line, offset)

          {{:local_or_var, acc}, offset} when hd(rest) == ?( ->
            build_surround({:local_call, acc}, reversed, line, offset)

          {{:local_or_var, acc}, offset} when hd(rest) == ?/ ->
            build_surround({:local_arity, acc}, reversed, line, offset)

          {{:local_or_var, acc}, offset} when acc in @textual_operators ->
            build_surround({:operator, acc}, reversed, line, offset)

          {{:local_or_var, acc}, offset} when acc not in ~w(do end after else catch rescue)c ->
            build_surround({:local_or_var, acc}, reversed, line, offset)

          {{:module_attribute, ''}, offset} ->
            build_surround({:operator, '@'}, reversed, line, offset)

          {{:module_attribute, acc}, offset} ->
            build_surround({:module_attribute, acc}, reversed, line, offset)

          {{:unquoted_atom, acc}, offset} ->
            build_surround({:unquoted_atom, acc}, reversed, line, offset)

          _ ->
            maybe_operator(reversed_pre, post, line, opts)
        end

      {:alias, reversed_post, _rest} ->
        reversed = reversed_post ++ reversed_pre

        case codepoint_cursor_context(reversed, opts) do
          {{:alias, acc}, offset} ->
            build_surround({:alias, acc}, reversed, line, offset)

          _ ->
            :none
        end
    end
  end

  defp maybe_operator(reversed_pre, post, line, opts) do
    case take_operator(post, []) do
      {[], _rest} ->
        :none

      {reversed_post, _rest} ->
        reversed = reversed_post ++ reversed_pre

        case codepoint_cursor_context(reversed, opts) do
          {{:operator, acc}, offset} ->
            build_surround({:operator, acc}, reversed, line, offset)

          {{:dot, _, [_ | _]} = dot, offset} ->
            build_surround(dot, reversed, line, offset)

          _ ->
            :none
        end
    end
  end

  defp build_surround(context, reversed, line, offset) do
    {post, reversed_pre} = enum_reverse_at(reversed, offset, [])
    pre = :lists.reverse(reversed_pre)
    pre_length = :string.length(pre) + 1

    %{
      context: context,
      begin: {line, pre_length},
      end: {line, pre_length + :string.length(post)}
    }
  end

  defp take_identifier([h | t], acc) when h in @trailing_identifier,
    do: {:identifier, [h | acc], t}

  defp take_identifier([h | t], acc) when h not in @non_identifier,
    do: take_identifier(t, [h | acc])

  defp take_identifier(rest, acc) do
    with {[?. | t], _} <- strip_spaces(rest, 0),
         {[h | _], _} when h in ?A..?Z <- strip_spaces(t, 0) do
      take_alias(rest, acc)
    else
      _ -> {:identifier, acc, rest}
    end
  end

  defp take_alias([h | t], acc) when h not in @non_identifier,
    do: take_alias(t, [h | acc])

  defp take_alias(rest, acc) do
    with {[?. | t], acc} <- move_spaces(rest, acc),
         {[h | t], acc} when h in ?A..?Z <- move_spaces(t, [?. | acc]) do
      take_alias(t, [h | acc])
    else
      _ -> {:alias, acc, rest}
    end
  end

  defp take_operator([h | t], acc) when h in @operators, do: take_operator(t, [h | acc])
  defp take_operator([h | t], acc) when h == ?., do: take_operator(t, [h | acc])
  defp take_operator(rest, acc), do: {acc, rest}

  # Unquoted atom handling
  defp adjust_position(reversed_pre, [?: | post])
       when hd(post) != ?: and (reversed_pre == [] or hd(reversed_pre) != ?:) do
    {[?: | reversed_pre], post}
  end

  # Dot handling
  defp adjust_position(reversed_pre, post) do
    case move_spaces(post, reversed_pre) do
      # If we are between spaces and a dot, move past the dot
      {[?. | post], reversed_pre} when hd(post) != ?. and hd(reversed_pre) != ?. ->
        {post, reversed_pre} = move_spaces(post, [?. | reversed_pre])
        {reversed_pre, post}

      _ ->
        case strip_spaces(reversed_pre, 0) do
          # If there is a dot to our left, make sure to move to the first character
          {[?. | rest], _} when rest == [] or hd(rest) not in '.:' ->
            {post, reversed_pre} = move_spaces(post, reversed_pre)
            {reversed_pre, post}

          _ ->
            {reversed_pre, post}
        end
    end
  end

  defp move_spaces([h | t], acc) when h in @space, do: move_spaces(t, [h | acc])
  defp move_spaces(t, acc), do: {t, acc}

  defp string_reverse_at(charlist, 0, acc), do: {acc, charlist}

  defp string_reverse_at(charlist, n, acc) do
    case :unicode_util.gc(charlist) do
      [gc | cont] when is_integer(gc) -> string_reverse_at(cont, n - 1, [gc | acc])
      [gc | cont] when is_list(gc) -> string_reverse_at(cont, n - 1, :lists.reverse(gc, acc))
      [] -> {acc, []}
    end
  end

  defp enum_reverse_at([h | t], n, acc) when n > 0, do: enum_reverse_at(t, n - 1, [h | acc])
  defp enum_reverse_at(rest, _, acc), do: {acc, rest}

  # ---

  # See: https://github.com/elixir-lang/elixir/pull/11143/files#r676519050
  def elixir_tokenizer_tokenize(string, line, columns, opts) do
    with {:ok, tokens} <- :elixir_tokenizer.tokenize(string, line, columns, opts) do
      {:ok, [], tokens}
    end
  end
end
