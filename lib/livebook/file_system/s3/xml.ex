defmodule Livebook.FileSystem.S3.XML do
  @moduledoc false

  # Adapted from https://github.com/aws-beam/aws-elixir/blob/v0.8.0/lib/aws/xml.ex

  import Record

  @text "__text"

  defrecord(:xmlElement, extract(:xmlElement, from_lib: "xmerl/include/xmerl.hrl"))
  defrecord(:xmlText, extract(:xmlText, from_lib: "xmerl/include/xmerl.hrl"))

  @doc """
  Encodes a map into XML iodata.

  Raises in case of errors.
  """
  @spec encode_to_iodata!(map()) :: iodata()
  def encode_to_iodata!(map) do
    map
    |> Map.to_list()
    |> Enum.map(&encode_xml_key_value/1)
    |> :erlang.iolist_to_binary()
  end

  @doc """
  Decodes a XML into a map.

  Raises in case of errors.
  """
  @spec decode!(iodata()) :: map()
  def decode!(xml) do
    xml_str = :unicode.characters_to_list(xml)
    opts = [{:hook_fun, &hook_fun/2}]
    {element, []} = :xmerl_scan.string(xml_str, opts)
    element
  end

  defp encode_xml_key_value({k, v}) when is_binary(k) and is_binary(v) do
    ["<", k, ">", escape_xml_string(v), "</", k, ">"]
  end

  defp encode_xml_key_value({k, values}) when is_binary(k) and is_list(values) do
    for v <- values do
      encode_xml_key_value({k, v})
    end
  end

  defp encode_xml_key_value({k, v}) when is_binary(k) and is_integer(v) do
    ["<", k, ">", Integer.to_charlist(v), "</", k, ">"]
  end

  defp encode_xml_key_value({k, v}) when is_binary(k) and is_float(v) do
    ["<", k, ">", Float.to_charlist(v), "</", k, ">"]
  end

  defp encode_xml_key_value({k, v}) when is_binary(k) and is_map(v) do
    inner = v |> Map.to_list() |> Enum.map(&encode_xml_key_value/1)
    ["<", k, ">", inner, "</", k, ">"]
  end

  # Callback hook_fun for xmerl parser
  defp hook_fun(element, global_state) when Record.is_record(element, :xmlElement) do
    tag = xmlElement(element, :name)
    content = xmlElement(element, :content)

    value =
      case List.foldr(content, :none, &content_to_map/2) do
        v = %{@text => text} ->
          case String.trim(text) do
            "" -> Map.delete(v, @text)
            trimmed -> Map.put(v, @text, trimmed)
          end

        v ->
          v
      end

    {%{Atom.to_string(tag) => value}, global_state}
  end

  defp hook_fun(text, global_state) when Record.is_record(text, :xmlText) do
    text = xmlText(text, :value)
    {:unicode.characters_to_binary(text), global_state}
  end

  # Convert the content of an Xml node into a map.
  # When there is more than one element with the same tag name, their
  # values get merged into a list.
  # If the content is only text then that is what gets returned.
  # If the content is a mix between text and child elements, then the
  # elements are processed as described above and all the text parts
  # are merged under the `__text' key.

  defp content_to_map(x, :none) do
    x
  end

  defp content_to_map(x, acc) when is_map(x) and is_map(acc) do
    [{tag, value}] = Map.to_list(x)

    case Map.has_key?(acc, tag) do
      true ->
        update_fun = fn
          l when is_list(l) -> [value | l]
          v -> [value, v]
        end

        Map.update!(acc, tag, update_fun)

      false ->
        Map.put(acc, tag, value)
    end
  end

  defp content_to_map(x, %{@text => text} = acc) when is_binary(x) and is_map(acc) do
    %{acc | @text => <<x::binary, text::binary>>}
  end

  defp content_to_map(x, acc) when is_binary(x) and is_map(acc) do
    Map.put(acc, @text, x)
  end

  defp content_to_map(x, acc) when is_binary(x) and is_binary(acc) do
    <<x::binary, acc::binary>>
  end

  defp content_to_map(x, acc) when is_map(x) and is_binary(acc) do
    Map.put(x, @text, acc)
  end

  # See https://github.com/ex-aws/ex_aws_s3/blob/6973f72b0b78d928c86252767e7dcebab5be0ba8/lib/ex_aws/s3.ex#L1268
  defp escape_xml_string(value) do
    String.replace(value, ["'", "\"", "&", "<", ">", "\r", "\n"], fn
      "'" -> "&apos;"
      "\"" -> "&quot;"
      "&" -> "&amp;"
      "<" -> "&lt;"
      ">" -> "&gt;"
      "\r" -> "&#13;"
      "\n" -> "&#10;"
    end)
  end
end
