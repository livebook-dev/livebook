defmodule Livebook.Intellisense.Erlang do

  alias Livebook.Intellisense

  @behaviour Intellisense

  @impl true
  def handle_request({:format, _code}, _context, _node) do
    # Not supported.
    nil
  end

  def handle_request({:completion, hint}, context, node) do
    handle_completion(hint, context, node)
  end

  def handle_request({:details, line, column}, context, _node) do
    handle_details(line, column, context)
  end

  def handle_request({:signature, hint}, context, _node) do
    handle_signature(hint, context)
  end

  defp handle_completion(hint, context, node) do
    # TODO: implement. See t:Livebook.Runtime.completion_response/0 for return type.
    IO.write("completion:")

    Intellisense.Erlang.IdentifierMatcher.completion_identifiers(hint, context, node)
    |> Intellisense.Elixir.format_completion_identifiers(extra_completion_items(hint))
  end

  defp handle_details(line, _column, _context) do
    # TODO: implement. See t:Livebook.Runtime.details_response/0 for return type.
    IO.write("details:")
    IO.inspect(line)
    nil
  end

  defp handle_signature(hint, _context) do
    # TODO: implement. See t:Livebook.Runtime.signature_response/0 for return type.
    IO.write("signature:")
    IO.inspect(hint)
    nil
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
