defmodule Livebook.Intellisense.Erlang do

  alias Livebook.Intellisense
  alias Livebook.Intellisense.Elixir

  @behaviour Intellisense

  @impl true
  def handle_request({:format, _code}, _context, _node) do
    # Not supported.
    nil
  end

  def handle_request({:completion, hint}, context, node) do
    handle_completion(hint, context, node)
  end

  def handle_request({:details, line, column}, context, node) do
    handle_details(line, column, context, node)
  end

  def handle_request({:signature, hint}, context, node) do
    handle_signature(hint, context, node)
  end

  defp handle_completion(hint, context, node) do
    # TODO: implement. See t:Livebook.Runtime.completion_response/0 for return type.
    IO.write("completion:")

    Intellisense.Erlang.IdentifierMatcher.completion_identifiers(hint, context, node)
    |> Intellisense.Elixir.format_completion_identifiers(extra_completion_items(hint))
  end

  defp handle_details(line, column, context, node) do
    %{matches: matches, range: range} =
      Intellisense.Erlang.IdentifierMatcher.locate_identifier(line, column, context, node)

    case Enum.filter(matches, &Intellisense.Elixir.include_in_details?/1) do
      [] ->
        nil

      matches ->
        matches = Enum.sort_by(matches, & &1[:arity], :asc)
        contents = Enum.map(matches, &Intellisense.Elixir.format_details_item/1)

        definition = Intellisense.Elixir.get_definition_location(hd(matches), context)
        %{range: range, contents: contents, definition: definition}
    end
  end

  defp handle_signature(hint, context, node) do
    case Intellisense.Erlang.SignatureMatcher.get_matching_signatures(hint, context, node) do
      {:ok, [], _active_argument} ->
        nil
      {:ok, signature_infos, active_argument} ->
        %{
          active_argument: active_argument,
          items:
            signature_infos
            |> Enum.map(&Intellisense.Elixir.format_signature_item/1)
            |> Enum.uniq()
        }
      :error ->
        nil
    end
  end

  @keywords [
      {"true", "(boolean)"},
      {"false", "(boolean)"},

      {"begin", "(block operator)"},
      {"case", "(case operator)"},
      {"fun", "(anonymous function operator)"},
      {"if", "(if operator)"},
      {"when", "(guard operator)"},

      {"after", "(after operator)"},
      {"catch", "(catch operator)"},
      {"receive", "(receive operator)"},
      {"try", "(try operator)"},

      {"and", "(logical AND operator)"},
      {"andalso", "(short-circuit logical AND operator)"},
      {"band", "(bitwise AND operator)"},

      {"not", "(logical NOT operator)"},
      {"bnot", "(bitwise NOT operator)"},

      {"or", "(logical OR operator)"},
      {"orelse", "(short-circuit logical OR operator)"},
      {"bor", "(bitwise OR operator)"},

      {"div", "(integer division operator)"},
      {"rem", "(integer remainder operator)"},
      {"bxor", "(bitwise XOR operator)"},
      {"bsl", "(bitshift left operator)"},
      {"bsr", "(bitshift right operator)"},
      {"xor", "(logical XOR operator)"},
    ]

  defp extra_completion_items(hint) do
    items = Enum.map(@keywords,
      fn {keyword, desc} -> %{
        label: keyword,
        kind: :keyword,
        documentation: desc,
        insert_text: keyword,
      } end
    )

    last_word = hint |> String.split(~r/\s/) |> List.last()

    if last_word == "" do
      []
    else
      Enum.filter(items, &String.starts_with?(&1.label, last_word))
    end
  end
end
