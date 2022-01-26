defmodule Livebook.Intellisense.SignatureMatcher do
  @moduledoc false

  # This module allows for extracting information about
  # function signatures matching an incomplete call.

  alias Livebook.Intellisense.Docs

  @type signature_info :: {name :: atom(), Docs.signature(), Docs.documentation(), Docs.spec()}

  @doc """
  Looks up a list of signatures matching the given incomplete
  funciton call.

  Evaluation binding and environment is used to expand aliases,
  imports, access variable values, etc.
  """
  @spec get_matching_signatures(String.t(), Livebook.Intellisense.intellisense_context()) ::
          {:ok, list(signature_info()), active_argument :: non_neg_integer()} | :error
  def get_matching_signatures(hint, intellisense_context) do
    %{env: env} = intellisense_context

    case matching_call(hint, intellisense_context) do
      {:ok, {:remote, mod, fun}, maybe_arity, active_argument} ->
        funs = [{fun, maybe_arity || :any}]
        signature_infos = signature_infos_for_members(mod, funs, active_argument)
        {:ok, signature_infos, active_argument}

      {:ok, {:local, name}, nil, active_argument} ->
        imports = env.functions ++ env.macros

        signature_infos =
          imports
          |> Enum.map(fn {mod, funs} ->
            matching_funs = Enum.filter(funs, fn {fun, _arity} -> fun == name end)
            {mod, matching_funs}
          end)
          |> Enum.reject(fn {_mod, matching_funs} -> matching_funs == [] end)
          |> Enum.flat_map(fn {mod, matching_funs} ->
            signature_infos_for_members(mod, matching_funs, active_argument)
          end)

        {:ok, signature_infos, active_argument}

      {:ok, {:anonymous, name}, arity, active_argument} ->
        args = unnamed_args(arity)
        signature = "#{name}.(#{Enum.join(args, ", ")})"
        {:ok, [{name, signature, nil, []}], active_argument}

      _ ->
        :error
    end
  end

  defp signature_infos_for_members(mod, funs, active_argument) do
    infos = Livebook.Intellisense.Docs.lookup_module_members(mod, funs, kind: [:function, :macro])

    for info <- infos,
        info.arity >= active_argument + 1 do
      signature = hd(info.signatures)
      signature = fix_erlang_signature(signature, info.specs)
      {info.name, signature, info.documentation, info.specs}
    end
  end

  defp unnamed_args(arity) do
    Enum.map(1..arity//1, fn n -> "arg#{n}" end)
  end

  defp fix_erlang_signature(signature, specs) do
    case parse_erlang_signature(signature) do
      {:ok, fun, arity} ->
        args =
          with [spec | _] <- specs,
               {:ok, args} <- args_from_spec(spec, fun, arity) do
            args
          else
            _ -> unnamed_args(arity)
          end

        "#{fun}(#{Enum.join(args, ", ")})"

      :error ->
        signature
    end
  end

  defp parse_erlang_signature(signature) do
    case Code.string_to_quoted(signature) do
      {:ok, {:/, _, [{fun, _, _}, arity]}} when is_integer(arity) ->
        {:ok, fun, arity}

      _ ->
        :error
    end
  end

  defp args_from_spec(spec, fun, arity) do
    case Code.Typespec.spec_to_quoted(fun, spec) do
      {:"::", _, [{^fun, _, args}, _]} when length(args) == arity ->
        {:ok, Enum.map(args, &Macro.to_string/1)}

      {:when, _, [{:"::", _, [{^fun, _, args}, _]}, _]} when length(args) == arity ->
        {:ok, Enum.map(args, &Macro.to_string/1)}

      _ ->
        :error
    end
  end

  # Returns {:ok, call, exact_arity_or_nil, active_argument} or :error
  defp matching_call(hint, intellisense_context) do
    with {:ok, call_target, active_argument} <- call_target_and_argument(hint) do
      case call_target do
        local when is_atom(local) ->
          {:ok, {:local, local}, nil, active_argument}

        {:., _, [{:__aliases__, _, _} = alias, fun]} when is_atom(fun) ->
          alias = Macro.expand(alias, intellisense_context.env)
          {:ok, {:remote, alias, fun}, nil, active_argument}

        {:., _, [mod, fun]} when is_atom(mod) and is_atom(fun) ->
          {:ok, {:remote, mod, fun}, nil, active_argument}

        {:., _, [{var, _, context}]} when is_atom(var) and is_atom(context) ->
          with {:ok, fun} <- function_from_binding(var, intellisense_context) do
            info = :erlang.fun_info(fun)

            case info[:type] do
              :external ->
                {:ok, {:remote, info[:module], info[:name]}, info[:arity], active_argument}

              :local ->
                {:ok, {:anonymous, var}, info[:arity], active_argument}
            end
          end

        _ ->
          :error
      end
    end
  end

  defp call_target_and_argument(hint) do
    with {:ok, ast} <- Code.Fragment.container_cursor_to_quoted(hint, token_metadata: true),
         {{call_target, meta, args}, piped?} <- find_cursor_call(ast),
         false <- cursor_in_do_end?(meta, args) do
      number_of_args = if piped?, do: length(args) + 1, else: length(args)
      {:ok, call_target, number_of_args - 1}
    else
      _ -> :error
    end
  end

  defp find_cursor_call(ast) do
    # Accumulator: {parent_call_node, pipe_info}
    find_cursor_call(ast, {nil, :none})
  end

  defp find_cursor_call({:__cursor__, _, []}, {node, pipe_info}) when node != nil do
    piped? = pipe_info == :piped
    {node, piped?}
  end

  defp find_cursor_call({:|>, _, [_, right]}, _acc) do
    find_cursor_call(right, {nil, :pipe})
  end

  defp find_cursor_call({:@, _, [{attr, _, [arg]}]}, _acc) when is_atom(attr) do
    find_cursor_call(arg, {nil, :none})
  end

  defp find_cursor_call({left, _, right} = node, {_, pipe_info} = acc) when is_list(right) do
    if is_atom(left) and
         (Macro.operator?(left, length(right)) or
            left in [:__block__, :__aliases__, :., :{}, :<<>>, :%, :%{}]) do
      find_cursor_call(right, acc)
    else
      pipe_info =
        case pipe_info do
          :none -> :none
          :pipe -> :piped
          :piped -> :none
        end

      find_cursor_call(right, {node, pipe_info})
    end
  end

  defp find_cursor_call({left, right}, acc) do
    find_cursor_call(left, acc) || find_cursor_call(right, acc)
  end

  defp find_cursor_call(list, acc) when is_list(list) do
    Enum.find_value(list, &find_cursor_call(&1, acc))
  end

  defp find_cursor_call(_other, _acc) do
    nil
  end

  defp cursor_in_do_end?(meta, args) do
    is_list(meta[:do]) and
      args
      |> List.last()
      |> Macro.prewalker()
      |> Enum.any?(&match?({:__cursor__, _, []}, &1))
  end

  defp function_from_binding(var, intellisense_context) do
    if Macro.Env.has_var?(intellisense_context.env, {var, nil}) do
      intellisense_context.map_binding.(fn binding ->
        case Keyword.fetch(binding, var) do
          {:ok, fun} when is_function(fun) -> {:ok, fun}
          _ -> :error
        end
      end)
    else
      :error
    end
  end
end
