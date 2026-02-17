defmodule Livebook.Intellisense.Python do
  require Logger

  alias Livebook.Intellisense

  @behaviour Intellisense

  @impl true
  def handle_request({:format, _code}, _context, _node) do
    # Not supported.
    nil
  end

  def handle_request({:completion, hint}, context, _node) do
    handle_completion(hint, context)
  end

  def handle_request({:details, line, column}, context, _node) do
    handle_details(line, column, context)
  end

  def handle_request({:signature, hint}, context, _node) do
    handle_signature(hint, context)
  end

  @compile {:no_warn_undefined, {Pythonx, :eval, 2}}
  @compile {:no_warn_undefined, {Pythonx, :decode, 1}}

  defp handle_completion(hint, context) do
    # Note that we pre-process hint with Elixir, so that we can grab
    # only matching values from binding and pass that for further
    # completion to Python, instead of passing all Pythonx objects
    # in binding to Python.

    ctx = %{matcher: :prefix}

    {completion_items, include_extra_items?} =
      case Livebook.Intellisense.Python.Code.completion_context(hint) do
        {:import, name} ->
          items = import_matches(name, ctx)
          items = Enum.map(items, fn item -> Map.put(item, "context", "import") end)
          {items, false}

        {:import_member, module_name, name} ->
          items = import_member_matches(module_name, name, ctx)
          items = Enum.map(items, fn item -> Map.put(item, "context", "import") end)
          {items, false}

        {:name, name} ->
          items = variable_and_dot_matches(name, context, ctx)
          {items, true}

        :default ->
          items = variable_and_dot_matches("", context, ctx)
          {items, true}

        :none ->
          {[], false}
      end

    items = Enum.map(completion_items, &format_completion_item/1)

    items =
      if include_extra_items? do
        items ++ extra_completion_items(hint)
      else
        items
      end

    items = Enum.sort_by(items, &completion_item_priority/1)

    %{items: items}
  end

  defp variable_and_dot_matches(hint, context, ctx) do
    case String.split(hint, ".") do
      [hint] ->
        {matching_names, matching_python_vars} = matching_vars(hint, context, ctx.matcher)

        python_infos =
          maybe_eval(
            """
            hint = hint.decode("utf-8")

            intellisense.variable_matches(hint, matching_python_vars, matcher)
            """,
            %{
              "hint" => hint,
              "matching_python_vars" => matching_python_vars,
              "matcher" => ctx.matcher
            }
          ) || %{}

        infos =
          for name <- matching_names, reduce: python_infos do
            infos when is_map_key(infos, name) -> infos
            infos -> Map.put(infos, name, %{"kind" => "variable", "name" => name})
          end

        Map.values(infos)

      [target_part | parts] ->
        {_matching_names, matching_python_vars} = matching_vars(target_part, context, :exact)

        target_object =
          case matching_python_vars do
            %{^target_part => object} -> object
            %{} -> nil
          end

        maybe_eval(
          """
          target_part = target_part.decode("utf-8")
          parts = [part.decode("utf-8") for part in parts]

          intellisense.dot_matches(target_object, target_part, parts, matcher)
          """,
          %{
            "target_object" => target_object,
            "target_part" => target_part,
            "parts" => parts,
            "matcher" => ctx.matcher
          }
        ) || []
    end
  end

  defp matching_vars(hint, context, matcher) do
    matching_names =
      for {var, nil} <- Macro.Env.vars(context.env),
          name = Atom.to_string(var),
          matches?(matcher, name, hint),
          do: name

    matching_python_vars =
      if matching_names == [] do
        %{}
      else
        context.map_binding.(fn binding ->
          for name <- matching_names,
              value = Keyword.get(binding, String.to_atom(name)),
              is_struct(value, Pythonx.Object),
              into: %{},
              do: {name, value}
        end)
      end

    {matching_names, matching_python_vars}
  end

  defp matches?(:exact, string, hint), do: string == hint
  defp matches?(:prefix, string, hint), do: String.starts_with?(string, hint)

  defp import_matches(hint, ctx) do
    maybe_eval(
      """
      hint = hint.decode("utf-8")

      intellisense.import_matches(hint, matcher)
      """,
      %{"hint" => hint, "matcher" => ctx.matcher}
    ) || []
  end

  defp import_member_matches(module_name, hint, ctx) do
    maybe_eval(
      """
      module_name = module_name.decode("utf-8")
      hint = hint.decode("utf-8")

      intellisense.import_member_matches(module_name, hint, matcher)
      """,
      %{"module_name" => module_name, "hint" => hint, "matcher" => ctx.matcher}
    ) || []
  end

  defp format_completion_item(%{"kind" => "variable", "name" => name}),
    do: %{
      label: name,
      kind: :variable,
      documentation: "(variable)",
      insert_text: name
    }

  defp format_completion_item(%{
         "kind" => "module",
         "name" => name,
         "documentation" => documentation
       }),
       do: %{
         label: name,
         kind: :module,
         documentation: shorten_documentation(documentation),
         insert_text: name
       }

  defp format_completion_item(
         %{
           "kind" => kind,
           "name" => name,
           "documentation" => documentation,
           "signature" => signature,
           "documentation_signature" => documentation_signature,
           "has_parameters" => has_parameters
         } = item
       )
       when kind in ["class", "function", "method"],
       do: %{
         label: name,
         kind: if(kind == "class", do: :struct, else: :function),
         documentation:
           join_with_newlines([
             shorten_documentation(documentation),
             code(documentation_signature || signature)
           ]),
         insert_text:
           cond do
             item["context"] == "import" ->
               name

             has_parameters == false ->
               "#{name}()"

             true ->
               "#{name}(${})"
           end
       }

  @ordered_kinds [
    :keyword,
    :field,
    :variable,
    :module,
    :struct,
    :interface,
    :function,
    :type,
    :bitstring_option
  ]

  defp completion_item_priority(completion_item) do
    {completion_item_kind_priority(completion_item.kind), completion_item.label}
  end

  defp completion_item_kind_priority(kind) when kind in @ordered_kinds do
    Enum.find_index(@ordered_kinds, &(&1 == kind))
  end

  defp handle_details(line, column, context) do
    case Livebook.Intellisense.Python.Code.details_context(line, column) do
      nil ->
        nil

      %{context: details_context, range: range} ->
        ctx = %{matcher: :exact}

        items =
          case details_context do
            {:import, name} ->
              import_matches(name, ctx)

            {:import_member, module_name, name} ->
              import_member_matches(module_name, name, ctx)

            {:name, name} ->
              variable_and_dot_matches(name, context, ctx)

            _ ->
              []
          end

        case items do
          [] ->
            nil

          [item] ->
            contents = [format_details_item(item)]
            %{range: range, contents: contents, definition: nil}
        end
    end
  end

  defp format_details_item(%{"kind" => "variable", "name" => name}) do
    code(name)
  end

  defp format_details_item(%{
         "kind" => "module",
         "intrinsic_name" => name,
         "documentation" => documentation
       }) do
    join_with_divider([code("(module) " <> name), documentation])
  end

  defp format_details_item(%{
         "kind" => "class",
         "intrinsic_name" => name,
         "documentation" => documentation,
         "signature" => signature,
         "documentation_signature" => documentation_signature
       }) do
    join_with_divider([
      code("(class) " <> name),
      code(documentation_signature || signature),
      documentation
    ])
  end

  defp format_details_item(%{
         "kind" => kind,
         "documentation" => documentation,
         "signature" => signature,
         "documentation_signature" => documentation_signature
       })
       when kind in ["function", "method"] do
    join_with_divider([code(documentation_signature || signature), documentation])
  end

  defp handle_signature(hint, context) do
    case Livebook.Intellisense.Python.Code.call_context(hint) do
      nil ->
        nil

      %{name: name, args: args} ->
        ctx = %{matcher: :exact}

        case variable_and_dot_matches(name, context, ctx) do
          [%{"kind" => kind, "signature" => signature}]
          when kind in ["class", "function", "method"] ->
            signature_response(signature, args)

          _ ->
            nil
        end
    end
  end

  defp signature_response(signature, args) do
    signature_info = parse_signature(signature)

    num_args = length(args)
    num_positional = Enum.count(args, &(&1 == :unnamed))
    num_named = num_args - num_positional

    last_arg_name =
      case List.last(args) do
        {:named, name} -> name
        :unnamed -> nil
      end

    active_argument =
      cond do
        last_arg_name ->
          signature_info.keyword_indices[last_arg_name] || signature_info.arbitrary_keyword_idx

        # If there is a named argument, and it's not the last one,
        # then we don't know which argument should be highlighted,
        # so we return nil. The exception is if there is **kwargs,
        # in which case we always highlight that.
        num_named > 0 ->
          signature_info.arbitrary_keyword_idx

        signature_info.arbitrary_positional_idx &&
            num_args - 1 >= signature_info.arbitrary_positional_idx ->
          signature_info.arbitrary_positional_idx

        num_args <= signature_info.max_positional ->
          num_args - 1

        # If it's N + 1 positional, the user may only be typing the
        # keyword name, so we still want to show the signature. We
        # don't highlight anything, unless there is **kwargs, which
        # we can highlight.
        num_args == signature_info.max_positional + 1 and signature_info.has_keyword_only? ->
          signature_info.arbitrary_keyword_idx

        true ->
          :invalid
      end

    case active_argument do
      :invalid ->
        nil

      active_argument ->
        %{
          active_argument: active_argument,
          items: [%{signature: signature, arguments: signature_info.parameters}]
        }
    end
  end

  defp parse_signature(signature) do
    parts =
      signature
      |> String.split("(", parts: 2)
      |> Enum.at(1)
      |> String.trim_trailing(")")
      |> String.split(",")
      |> Enum.map(&String.trim/1)

    acc = %{
      parameters: [],
      max_positional: 0,
      arbitrary_positional_idx: nil,
      arbitrary_keyword_idx: nil,
      has_keyword_only?: false,
      keyword_indices: %{},
      # Internal tracking.
      num_parameters: 0,
      positional_finished?: false,
      positional_keyword_finished?: false
    }

    parts
    |> Enum.reduce(acc, fn
      "/", acc ->
        %{acc | positional_finished?: true}

      "*", acc ->
        %{acc | positional_finished?: true, positional_keyword_finished?: true}

      "**" <> _ = param, acc ->
        %{
          acc
          | arbitrary_keyword_idx: acc.num_parameters,
            positional_finished?: true,
            positional_keyword_finished?: true,
            has_keyword_only?: true,
            num_parameters: acc.num_parameters + 1,
            parameters: [param | acc.parameters]
        }

      "*" <> _ = param, acc ->
        %{
          acc
          | arbitrary_positional_idx: acc.num_parameters,
            positional_finished?: true,
            max_positional: :infinity,
            num_parameters: acc.num_parameters + 1,
            parameters: [param | acc.parameters]
        }

      param, acc ->
        acc =
          case String.split(param, "=", parts: 2) do
            [_] -> acc
            [name, _] -> put_in(acc.keyword_indices[name], acc.num_parameters)
          end

        acc =
          if acc.positional_keyword_finished? do
            %{acc | has_keyword_only?: true}
          else
            Map.update!(acc, :max_positional, fn
              num when is_number(num) -> num + 1
              :infinity -> :infinity
            end)
          end

        %{acc | num_parameters: acc.num_parameters + 1, parameters: [param | acc.parameters]}
    end)
    |> Map.update!(:parameters, &Enum.reverse/1)
  end

  @cache_key {__MODULE__, :intellisense_py}
  @intellisense_py "lib/livebook/intellisense/python/intellisense.py"
  @external_resource @intellisense_py
  @intellisense_py_code File.read!(@intellisense_py)

  defp maybe_eval(code, globals) do
    if Code.ensure_loaded?(Pythonx) do
      try do
        # We create a Python module with helpers and cache it, then we
        # reuse it across evaluations.
        intellisense =
          if intellisense = :persistent_term.get(@cache_key, nil) do
            intellisense
          else
            {intellisense, %{}} = Pythonx.eval(@intellisense_py_code, %{})
            :persistent_term.put(@cache_key, intellisense)
            intellisense
          end

        globals = Map.put(globals, "intellisense", intellisense)
        {result, %{}} = Pythonx.eval(code, globals)
        Pythonx.decode(result)
      rescue
        error ->
          Logger.error("Error during Python intellisense: #{Exception.message(error)}")
          nil
      end
    end
  end

  defp shorten_documentation(nil), do: nil

  defp shorten_documentation(documentation) do
    documentation |> String.split("\n\n", parts: 2) |> hd()
  end

  defp join_with_divider(strings), do: join_with(strings, "\n\n---\n\n")

  defp join_with_newlines(strings), do: join_with(strings, "\n\n")

  defp join_with(strings, joiner) do
    case Enum.reject(strings, &is_nil/1) do
      [] -> nil
      parts -> Enum.join(parts, joiner)
    end
  end

  defp code(nil), do: nil

  defp code(code) do
    """
    ```
    #{code}
    ```\
    """
  end

  defp extra_completion_items(hint) do
    items = [
      %{
        label: "and",
        kind: :keyword,
        documentation: "Logical operator that returns true if both operands are true.",
        insert_text: "and"
      },
      %{
        label: "as",
        kind: :keyword,
        documentation: "Creates an alias when importing a module or in with statements.",
        insert_text: "as"
      },
      %{
        label: "assert",
        kind: :keyword,
        documentation: "Used for debugging purposes to test a condition.",
        insert_text: "assert"
      },
      %{
        label: "async",
        kind: :keyword,
        documentation: "Declares an asynchronous function or context manager.",
        insert_text: "async"
      },
      %{
        label: "await",
        kind: :keyword,
        documentation: "Waits for an asynchronous operation to complete.",
        insert_text: "await"
      },
      %{
        label: "break",
        kind: :keyword,
        documentation: "Exits the current loop prematurely.",
        insert_text: "break"
      },
      %{
        label: "case",
        kind: :keyword,
        documentation: "Defines a pattern in a match statement.",
        insert_text: "case"
      },
      %{
        label: "class",
        kind: :keyword,
        documentation: "Defines a new class.",
        insert_text: "class"
      },
      %{
        label: "continue",
        kind: :keyword,
        documentation: "Skips the rest of the current loop iteration.",
        insert_text: "continue"
      },
      %{
        label: "def",
        kind: :keyword,
        documentation: "Defines a new function.",
        insert_text: "def"
      },
      %{
        label: "del",
        kind: :keyword,
        documentation: "Deletes an object or variable.",
        insert_text: "del"
      },
      %{
        label: "elif",
        kind: :keyword,
        documentation: "Else if condition in an if statement.",
        insert_text: "elif"
      },
      %{
        label: "else",
        kind: :keyword,
        documentation: "Defines the alternative branch in conditional statements.",
        insert_text: "else"
      },
      %{
        label: "except",
        kind: :keyword,
        documentation: "Catches exceptions in a try block.",
        insert_text: "except"
      },
      %{
        label: "finally",
        kind: :keyword,
        documentation: "Defines a block that always executes after try/except.",
        insert_text: "finally"
      },
      %{
        label: "for",
        kind: :keyword,
        documentation: "Creates a for loop to iterate over a sequence.",
        insert_text: "for"
      },
      %{
        label: "from",
        kind: :keyword,
        documentation: "Imports specific parts of a module.",
        insert_text: "from"
      },
      %{
        label: "global",
        kind: :keyword,
        documentation: "Declares a variable as global.",
        insert_text: "global"
      },
      %{
        label: "if",
        kind: :keyword,
        documentation: "Defines a conditional statement.",
        insert_text: "if"
      },
      %{
        label: "import",
        kind: :keyword,
        documentation: "Imports a module.",
        insert_text: "import"
      },
      %{
        label: "in",
        kind: :keyword,
        documentation: "Tests membership in a sequence or iterates in for loops.",
        insert_text: "in"
      },
      %{
        label: "is",
        kind: :keyword,
        documentation: "Tests object identity.",
        insert_text: "is"
      },
      %{
        label: "lambda",
        kind: :keyword,
        documentation: "Creates an anonymous function.",
        insert_text: "lambda"
      },
      %{
        label: "match",
        kind: :keyword,
        documentation: "Starts a pattern matching statement.",
        insert_text: "match"
      },
      %{
        label: "nonlocal",
        kind: :keyword,
        documentation: "Declares a variable as non-local in nested functions.",
        insert_text: "nonlocal"
      },
      %{
        label: "not",
        kind: :keyword,
        documentation: "Logical operator that negates a boolean value.",
        insert_text: "not"
      },
      %{
        label: "or",
        kind: :keyword,
        documentation: "Logical operator that returns true if either operand is true.",
        insert_text: "or"
      },
      %{
        label: "pass",
        kind: :keyword,
        documentation: "A null statement that does nothing.",
        insert_text: "pass"
      },
      %{
        label: "raise",
        kind: :keyword,
        documentation: "Raises an exception.",
        insert_text: "raise"
      },
      %{
        label: "return",
        kind: :keyword,
        documentation: "Exits a function and returns a value.",
        insert_text: "return"
      },
      %{
        label: "try",
        kind: :keyword,
        documentation: "Defines a block of code to test for exceptions.",
        insert_text: "try"
      },
      %{
        label: "type",
        kind: :keyword,
        documentation: "Defines a type alias.",
        insert_text: "type"
      },
      %{
        label: "while",
        kind: :keyword,
        documentation: "Creates a while loop.",
        insert_text: "while"
      },
      %{
        label: "with",
        kind: :keyword,
        documentation: "Simplifies exception handling with context managers.",
        insert_text: "with"
      },
      %{
        label: "yield",
        kind: :keyword,
        documentation: "Returns a value from a generator function.",
        insert_text: "yield"
      }
    ]

    last_word = hint |> String.split(~r/\s/) |> List.last()

    if last_word == "" do
      []
    else
      Enum.filter(items, &String.starts_with?(&1.label, last_word))
    end
  end
end
