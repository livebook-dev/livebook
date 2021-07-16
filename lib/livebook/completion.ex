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
    hint
    |> complete(%{binding: binding, env: env})
    |> Enum.sort_by(&completion_item_priority/1)
  end

  defp completion_item_priority(completion_item) do
    {completion_item_kind_priority(completion_item.kind), completion_item.label}
  end

  @ordered_kinds [:field, :variable, :module, :function, :type]

  defp completion_item_kind_priority(kind) when kind in @ordered_kinds do
    Enum.find_index(@ordered_kinds, &(&1 == kind))
  end

  defp complete(hint, ctx) do
    case Code.cursor_context(hint) do
      {:alias, alias} ->
        complete_alias(List.to_string(alias), ctx)

      {:unquoted_atom, unquoted_atom} ->
        complete_erlang_module(List.to_string(unquoted_atom))

      {:dot, path, hint} ->
        complete_dot(path, List.to_string(hint), ctx)

      {:dot_arity, path, hint} ->
        complete_dot(path, List.to_string(hint), ctx)

      {:dot_call, _path, _hint} ->
        complete_default(ctx)

      :expr ->
        complete_default(ctx)

      {:local_or_var, local_or_var} ->
        complete_local_or_var(List.to_string(local_or_var), ctx)

      {:local_arity, local} ->
        complete_local(List.to_string(local), ctx)

      {:local_call, _local} ->
        complete_default(ctx)

      {:module_attribute, attribute} ->
        complete_module_attribute(List.to_string(attribute))

      # :none
      _ ->
        []
    end
  end

  ## Complete dot

  defp complete_dot(path, hint, ctx) do
    case expand_dot_path(path, ctx) do
      {:ok, mod} when is_atom(mod) and hint == "" ->
        complete_module_member(mod, hint) ++ complete_module(mod, hint)

      {:ok, mod} when is_atom(mod) ->
        complete_module_member(mod, hint)

      {:ok, map} when is_map(map) ->
        complete_map_field(map, hint)

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

  defp complete_default(ctx) do
    complete_local_or_var("", ctx)
  end

  defp complete_alias(hint, ctx) do
    case split_at_last_occurrence(hint, ".") do
      {hint, ""} ->
        complete_elixir_root_module(hint) ++ complete_env_alias(hint, ctx)

      {alias, hint} ->
        mod = expand_alias(alias, ctx)
        complete_module(mod, hint)
    end
  end

  defp complete_module_member(mod, hint) do
    complete_module_function(mod, hint) ++ complete_module_type(mod, hint)
  end

  defp complete_local_or_var(hint, ctx) do
    complete_local(hint, ctx) ++ complete_variable(hint, ctx)
  end

  defp complete_local(hint, ctx) do
    imports =
      ctx.env
      |> imports_from_env()
      |> Enum.flat_map(fn {mod, funs} ->
        complete_module_function(mod, hint, funs)
      end)

    special_forms = complete_module_function(Kernel.SpecialForms, hint)

    imports ++ special_forms
  end

  defp complete_variable(hint, ctx) do
    for {key, value} <- ctx.binding,
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

  defp complete_map_field(map, hint) do
    # Note: we need Map.to_list/1 in case this is a struct
    for {key, value} <- Map.to_list(map),
        is_atom(key),
        name = Atom.to_string(key),
        String.starts_with?(name, hint),
        do: %{
          label: name,
          kind: :field,
          detail: "field",
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

  defp complete_erlang_module(hint) do
    for mod <- get_matching_modules(hint),
        usable_as_unquoted_module?(mod),
        name = Atom.to_string(mod) do
      %{
        label: name,
        kind: :module,
        detail: "module",
        documentation: mod |> get_module_doc_content() |> format_doc_content(),
        insert_text: name
      }
    end
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

  defp complete_env_alias(hint, ctx) do
    for {alias, mod} <- ctx.env.aliases,
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

  defp complete_module(base_mod, hint) do
    # Note: we specifically don't want further completion
    # if `base_mod` is an Erlang module.

    if base_mod == Elixir or elixir_module?(base_mod) do
      complete_elixir_module(base_mod, hint)
    else
      []
    end
  end

  defp elixir_module?(mod) do
    mod |> Atom.to_string() |> String.starts_with?("Elixir.")
  end

  defp complete_elixir_root_module(hint) do
    items = complete_elixir_module(Elixir, hint)

    # `Elixir` is not a existing module name, but `Elixir.Enum` is,
    # so if the user types `Eli` the completion should include `Elixir`.
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

  defp complete_elixir_module(base_mod, hint) do
    # Note: `base_mod` may be `Elixir`, even though it's not a valid module

    match_prefix = "#{base_mod}.#{hint}"
    depth = match_prefix |> Module.split() |> length()

    for mod <- get_matching_modules(match_prefix),
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
      {format, docs} = get_docs(mod, [:function, :macro])
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

        docstr = doc |> doc_content(format) |> format_doc_content()
        signatures = doc |> doc_signatures() |> format_signatures(mod)
        spec = format_spec(spec)

        %{
          label: "#{name}/#{arity}",
          kind: :function,
          detail: signatures,
          documentation: documentation_join([docstr, spec]),
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

  defp documentation_join(list) do
    case Enum.reject(list, &is_nil/1) do
      [] -> nil
      parts -> Enum.join(parts, "\n\n")
    end
  end

  defp format_signatures([], _mod), do: nil

  defp format_signatures(signatures, mod) do
    mod_to_prefix(mod) <> Enum.join(signatures, "\n")
  end

  defp mod_to_prefix(mod) do
    case Atom.to_string(mod) do
      "Elixir." <> name -> name <> "."
      name -> name <> "."
    end
  end

  defp format_doc_content(nil), do: nil

  defp format_doc_content({"text/markdown", markdown}) do
    # Extract just the first paragraph
    markdown
    |> String.split("\n\n")
    |> hd()
    |> String.trim()
  end

  defp format_doc_content({"application/erlang+html", erlang_html}) do
    case erlang_html do
      # Extract just the first paragraph
      [{:p, _, inner} | _] ->
        inner
        |> text_from_erlang_html()
        |> String.trim()

      _ ->
        nil
    end
  end

  defp format_doc_content(_), do: nil

  def text_from_erlang_html(ast) when is_list(ast) do
    ast
    |> Enum.map(&text_from_erlang_html/1)
    |> Enum.join("")
  end

  def text_from_erlang_html(ast) when is_binary(ast), do: ast
  def text_from_erlang_html({_, _, ast}), do: text_from_erlang_html(ast)

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
    {format, docs} = get_docs(mod, [:type])
    types = get_module_types(mod)

    types
    |> Enum.filter(fn {name, _arity} ->
      name = Atom.to_string(name)
      String.starts_with?(name, hint)
    end)
    |> Enum.map(fn {name, arity} ->
      doc = find_doc(docs, {name, arity})
      docstr = doc |> doc_content(format) |> format_doc_content()

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

  defp complete_module_attribute(hint) do
    for {attribute, info} <- Module.reserved_attributes(),
        name = Atom.to_string(attribute),
        String.starts_with?(name, hint) do
      %{
        label: name,
        kind: :variable,
        detail: "module attribute",
        documentation: info.doc,
        insert_text: name
      }
    end
  end
end
