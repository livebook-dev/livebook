defmodule Livebook.Intellisense.Python.Code do
  # This module provides functions analyzing Python code fragments.
  #
  # The parsing here is very limited. It focuses on single, possibly
  # incomplete statements, only the ones relevant for our intellisense
  # needs.

  require Record

  Record.defrecordp(:token, type: nil, content: nil, from: nil, to: nil)

  @type cursor_context ::
          {:import, name :: String.t()}
          | {:import_member, module_name :: String.t(), name :: String.t()}
          | {:name, name :: String.t()}
          | :default
          | :none

  @doc """
  Returns context information at the end of the given snippet of code,
  relevant for completion.
  """
  @spec completion_context(String.t()) :: cursor_context()
  def completion_context(string) do
    tokens = string |> string_last_line() |> simple_tokenize() |> Enum.reverse()
    cursor_context(tokens)
  end

  defp cursor_context(tokens) do
    tokens = skip_whitespace(tokens)

    case tokens do
      [token(type: :identifier, content: "import"), token(type: :whitespace) | rest] ->
        cursor_context_import(rest)

      [token(type: :identifier, content: "from"), token(type: :whitespace) | rest] ->
        case take_dotted_name(rest) do
          {name, []} ->
            {:import, name}

          {name, rest} ->
            case rest do
              [
                token(type: :whitespace),
                token(type: :identifier, content: "import"),
                token(type: :whitespace) | rest
              ] ->
                cursor_context_import_member(rest, name)

              _ ->
                :none
            end
        end

      tokens ->
        continue_until_name(tokens)
    end
  end

  defp continue_until_name([token(type: :identifier) | _rest] = tokens) do
    case take_dotted_name(tokens) do
      {name, []} ->
        {:name, name}

      {_name, rest} ->
        continue_until_name(rest)
    end
  end

  defp continue_until_name([token(type: :symbol, content: ".")]) do
    :none
  end

  defp continue_until_name([token(type: type)]) when type in [:number, :comment, :string] do
    :none
  end

  defp continue_until_name([token(type: :symbol, content: ".") | rest]) do
    # We may run into a dot in expression such as "foo().bar", in which
    # case we ignore the subsequent dotted identifier
    case take_dotted_name(rest) do
      {_name, []} -> :none
      {_name, rest} -> continue_until_name(rest)
    end
  end

  defp continue_until_name([_token | rest]) do
    continue_until_name(rest)
  end

  defp continue_until_name([]), do: :default

  defp cursor_context_import(tokens) do
    case take_dotted_name(tokens) do
      {name, []} ->
        {:import, name}

      {_name, rest} ->
        rest
        |> skip_as_alias()
        |> skip_whitespace()
        |> case do
          [token(type: :symbol, content: ",") | rest] ->
            rest = skip_whitespace(rest)
            cursor_context_import(rest)

          _ ->
            :none
        end
    end
  end

  defp cursor_context_import_member(tokens, module_name) do
    case take_name(tokens) do
      {name, []} ->
        {:import_member, module_name, name}

      {_name, rest} ->
        rest
        |> skip_as_alias()
        |> skip_whitespace()
        |> case do
          [token(type: :symbol, content: ",") | rest] ->
            rest = skip_whitespace(rest)
            cursor_context_import_member(rest, module_name)

          _ ->
            :none
        end
    end
  end

  defp skip_as_alias([
         token(type: :whitespace),
         token(type: :identifier, content: "as"),
         token(type: :whitespace),
         token(type: :identifier) | rest
       ]) do
    rest
  end

  defp skip_as_alias(tokens), do: tokens

  defp take_dotted_name(tokens), do: take_dotted_name(tokens, "")

  defp take_dotted_name([token(type: :identifier, content: name) | rest], acc) do
    take_dotted_name(rest, acc <> name)
  end

  defp take_dotted_name([token(type: :symbol, content: ".") | rest], acc) do
    take_dotted_name(rest, acc <> ".")
  end

  defp take_dotted_name(rest, acc) do
    {acc, rest}
  end

  defp take_name([token(type: :identifier, content: name) | rest]) do
    {name, rest}
  end

  defp take_name(rest) do
    {"", rest}
  end

  defp skip_whitespace([token(type: :whitespace) | tokens]), do: tokens
  defp skip_whitespace(tokens), do: tokens

  @doc """
  Returns context information about the name at the given `column`
  within `line`.

  Returns context similar to `completion_context/1`, as well as column
  range where the given name starts and begins.
  """
  @spec completion_context(String.t()) ::
          %{context: cursor_context(), range: %{from: pos_integer(), to: pos_integer()}} | nil
  def details_context(line, column) do
    tokens = line |> simple_tokenize() |> Enum.reverse()
    reversed_tokens = take_details_tokens(tokens, column)

    case reversed_tokens do
      [token(type: :identifier, from: {_, start_column}, to: {_, end_column}) | rest] ->
        start_column = start_column(rest, start_column)
        tokens = Enum.reverse(reversed_tokens)
        context = cursor_context(tokens)
        %{context: context, range: %{from: start_column, to: end_column}}

      _ ->
        nil
    end
  end

  defp take_details_tokens(tokens, column) do
    take_details_tokens(tokens, column, [])
  end

  defp take_details_tokens([token(to: {1, end_colum}) = t | rest], column, acc)
       when column >= end_colum do
    take_details_tokens(rest, column, [t | acc])
  end

  defp take_details_tokens(
         [
           token(type: :symbol, content: ".") = dot,
           token(type: :identifier) = identifier | _rest
         ],
         _column,
         acc
       ) do
    [identifier, dot | acc]
  end

  defp take_details_tokens([t | _rest], _column, acc) do
    [t | acc]
  end

  defp take_details_tokens([], _column, acc) do
    acc
  end

  defp start_column(
         [
           token(type: :symbol, content: "."),
           token(type: :identifier, from: {1, start_column}) | rest
         ],
         _start_column
       ) do
    start_column(rest, start_column)
  end

  defp start_column(_rest, start_column), do: start_column

  @doc """
  Returns context information about an incomplete function call at
  the end of the given snippet of code, if any.

  Returns the called name, as well as information about the type of
  arguments passed to the call.
  """
  @spec call_context(String.t()) :: %{name: String.t(), args: [arg]} | nil
        when arg: :unnamed | {:named, String.t()}
  def call_context(string) do
    reversed_tokens = simple_tokenize(string)

    case backtrace_call(reversed_tokens) do
      nil -> nil
      {name, acc} -> %{name: name, args: acc.args}
    end
  end

  defp backtrace_call(tokens) do
    backtrace_call(tokens, %{args: [], pending_param_name: nil, bracket_stack: []})
  end

  defp backtrace_call(
         [token(type: :symbol, content: "("), token(type: :identifier, content: name) | rest],
         acc
       )
       when acc.bracket_stack == [] do
    case backtrace_dot_name(rest, name) do
      {_name, [token(type: :symbol, content: ".") | _rest]} ->
        # Dot before the dotted name, for example foo().bar(
        nil

      {name, _rest} ->
        {name, finish_param(acc)}
    end
  end

  defp backtrace_call([token(type: :symbol, content: ",") | rest], acc)
       when acc.bracket_stack == [] do
    case backskip_lambda_with_args(rest) do
      {:ok, rest} ->
        # If there is a lambda, ignore the comma.
        backtrace_call(rest, acc)

      :error ->
        backtrace_call(rest, finish_param(acc))
    end
  end

  defp backtrace_call([token(type: :symbol, content: "=") | rest], acc)
       when acc.bracket_stack == [] do
    rest = skip_whitespace(rest)

    case rest do
      [token(type: :identifier, content: name) | rest] ->
        backtrace_call(rest, %{acc | pending_param_name: name})

      _ ->
        backtrace_call(rest, acc)
    end
  end

  defp backtrace_call(
         [token(type: :symbol, content: symbol) | rest],
         %{bracket_stack: [symbol | bracket_stack]} = acc
       ) do
    backtrace_call(rest, %{acc | bracket_stack: bracket_stack})
  end

  defp backtrace_call([token(type: :symbol, content: symbol) | rest], acc)
       when symbol in [")", "}", "]"] do
    opening =
      case symbol do
        ")" -> "("
        "}" -> "{"
        "]" -> "["
      end

    backtrace_call(rest, %{acc | bracket_stack: [opening | acc.bracket_stack]})
  end

  defp backtrace_call([token(type: :symbol, content: symbol) | rest], acc)
       when symbol in ["(", "{", "["] and acc.bracket_stack == [] do
    # If we run into a bracket opening that is not the function call,
    # it means everything so far was within unclosed bracket, so we
    # need to ignore it (start over).
    backtrace_call(rest)
  end

  defp backtrace_call([_token | rest], acc) do
    backtrace_call(rest, acc)
  end

  defp backtrace_call([], _acc) do
    nil
  end

  defp finish_param(%{pending_param_name: nil} = acc) do
    %{acc | args: [:unnamed | acc.args]}
  end

  defp finish_param(%{pending_param_name: name} = acc) do
    %{acc | args: [{:named, name} | acc.args], pending_param_name: nil}
  end

  defp backtrace_dot_name(
         [token(type: :symbol, content: "."), token(type: :identifier, content: name) | rest],
         acc
       ) do
    backtrace_dot_name(rest, name <> "." <> acc)
  end

  defp backtrace_dot_name(rest, acc) do
    {acc, rest}
  end

  defp backskip_lambda_with_args([
         token(type: :whitespace),
         token(type: :identifier, content: "lambda") | rest
       ]) do
    {:ok, rest}
  end

  defp backskip_lambda_with_args(tokens) do
    tokens = skip_whitespace(tokens)

    case tokens do
      [token(type: :identifier) | rest] ->
        backskip_lambda_with_args(rest)

      [token(type: :symbol, content: ",") | rest] ->
        backskip_lambda_with_args(rest)

      _tokens ->
        :error
    end
  end

  # A highly simplified Python tokenizer.
  #
  # The main goal is to split code into identifiers and other symbols,
  # useful for the parsing above.
  #
  # Returns a list of token records with :type, :content, :from, :to
  # fields, where `:from` and `:to` are `{line, column}` positions,
  # with `:to` being exclusive. Type can be either of:
  #
  #   * :identifier
  #   * :symbol
  #   * :newline
  #   * :whitespace
  #   * :string
  #   * :comment
  #   * :number
  #
  # Note: the returned token list is reversed.
  defp simple_tokenize(string) do
    simple_tokenize(string, {1, 1}, [])
  end

  defp simple_tokenize(<<>>, _pos, acc), do: acc

  defp simple_tokenize(string, pos, acc) do
    {type, value, rest} =
      case string do
        <<c, rest::binary>> when c in ~c[ \t] ->
          {rest, len} = take_while_horizontal_space(rest, 1)
          value = binary_part(string, 0, len)
          {:whitespace, value, rest}

        <<?\n, rest::binary>> ->
          {:newline, "\n", rest}

        <<?\r, ?\n, rest::binary>> ->
          {:newline, "\r\n", rest}

        <<c, rest::binary>> when c in ?0..?9 ->
          {rest, len} = take_while_digit(rest, 1)
          value = binary_part(string, 0, len)
          {:number, value, rest}

        <<q, q, q, rest::binary>> when q in [?", ?'] ->
          {rest, len} = take_until_triple_quote(rest, q, 3)
          value = binary_part(string, 0, len)
          {:string, value, rest}

        <<q, rest::binary>> when q in [?", ?'] ->
          {rest, len} = take_until_quote(rest, q, 1)
          value = binary_part(string, 0, len)
          {:string, value, rest}

        <<?#, rest::binary>> ->
          {rest, len} = take_until_newline(rest, 1)
          value = binary_part(string, 0, len)
          {:comment, value, rest}

        rest ->
          identifier_regex = ~r/^[_\p{XID_Start}][_\p{XID_Continue}]*/u

          case Regex.run(identifier_regex, rest) do
            [identifier] ->
              identifier_size = byte_size(identifier)
              rest = binary_part(rest, identifier_size, byte_size(rest) - identifier_size)
              {:identifier, identifier, rest}

            nil ->
              {character, rest} = String.next_grapheme(rest)
              {:symbol, character, rest}
          end
      end

    end_pos = advance_position(pos, value)

    t = token(type: type, content: value, from: pos, to: end_pos)
    simple_tokenize(rest, end_pos, [t | acc])
  end

  defp take_while_horizontal_space(<<c, rest::binary>>, len) when c in ~c[ \t],
    do: take_while_horizontal_space(rest, len + 1)

  defp take_while_horizontal_space(rest, len), do: {rest, len}

  defp take_while_digit(<<c, rest::binary>>, len) when c in ?0..?9,
    do: take_while_digit(rest, len + 1)

  defp take_while_digit(rest, len), do: {rest, len}

  defp take_until_newline(<<?\r, ?\n, _::binary>> = rest, len), do: {rest, len}
  defp take_until_newline(<<?\n, _::binary>> = rest, len), do: {rest, len}
  defp take_until_newline(<<_, rest::binary>>, len), do: take_until_newline(rest, len + 1)
  defp take_until_newline(<<>>, len), do: {<<>>, len}

  defp take_until_triple_quote(<<q, q, q, rest::binary>>, q, len),
    do: {rest, len + 3}

  defp take_until_triple_quote(<<_, rest::binary>>, q, len),
    do: take_until_triple_quote(rest, q, len + 1)

  defp take_until_triple_quote(<<>>, _q, len), do: {<<>>, len}

  defp take_until_quote(<<q, rest::binary>>, q, len), do: {rest, len + 1}
  defp take_until_quote(<<?\\, _, rest::binary>>, q, len), do: take_until_quote(rest, q, len + 2)
  defp take_until_quote(<<_, rest::binary>>, q, len), do: take_until_quote(rest, q, len + 1)
  defp take_until_quote(<<>>, _q, len), do: {<<>>, len}

  defp advance_position({line, col}, value) do
    {newlines, last_line_offset} = newline_info(value)

    if newlines == 0 do
      {line, col + String.length(value)}
    else
      last_line = binary_part(value, last_line_offset, byte_size(value) - last_line_offset)
      {line + newlines, 1 + String.length(last_line)}
    end
  end

  defp newline_info(string), do: newline_info(string, 0, 0, 0)

  defp newline_info(<<?\r, ?\n, rest::binary>>, newlines, _last_line_offset, offset) do
    newline_info(rest, newlines + 1, offset + 2, offset + 2)
  end

  defp newline_info(<<?\n, rest::binary>>, newlines, _last_line_offset, offset) do
    newline_info(rest, newlines + 1, offset + 1, offset + 1)
  end

  defp newline_info(<<_, rest::binary>>, newlines, last_line_offset, offset) do
    newline_info(rest, newlines, last_line_offset, offset + 1)
  end

  defp newline_info(<<>>, newlines, last_line_offset, _offset), do: {newlines, last_line_offset}

  defp string_last_line(string) do
    {_newlines, last_line_offset} = newline_info(string)
    binary_part(string, last_line_offset, byte_size(string) - last_line_offset)
  end
end
