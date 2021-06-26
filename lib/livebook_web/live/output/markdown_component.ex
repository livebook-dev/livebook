defmodule LivebookWeb.Output.MarkdownComponent do
  use LivebookWeb, :live_component

  alias Livebook.LiveMarkdown.MarkdownHelpers

  @impl true
  def render(assigns) do
    ~L"""
    <div class="markdown">
      <%= render_markdown(@content) %>
    </div>
    """
  end

  defp render_markdown(markdown) do
    {_, ast, earmark_messages} = EarmarkParser.as_ast(markdown)
    messages = Enum.map(earmark_messages, &MarkdownHelpers.earmark_message_to_string/1)

    html = ast |> ast_to_html()

    assigns = %{html: html, messages: messages}

    ~L"""
    <div class="flex flex-col space-y-5">
      <%= if @messages != [] do %>
        <div class="flex flex-col space-y-2">
          <%= for message <- @messages do %>
            <div class="error-box">
              <%= message %>
            </div>
          <% end %>
        </div>
      <% end %>
      <div>
        <%= raw(@html) %>
      </div>
    </div>
    """
  end

  defp ast_to_html(ast)

  defp ast_to_html(binary) when is_binary(binary) do
    escape(binary)
  end

  defp ast_to_html(list) when is_list(list) do
    Enum.map(list, &ast_to_html/1)
  end

  # https://www.w3.org/TR/2011/WD-html-markup-20110113/syntax.html#void-element
  @void_elements ~W(area base br col command embed hr img input keygen link meta param source track wbr)

  defp ast_to_html({:comment, _attrs, _inner, %{comment: true}}) do
    []
  end

  defp ast_to_html({tag, attrs, _inner, %{verbatim: true}}) when tag in @void_elements do
    escape(["<#{tag}#{ast_attributes_to_string(attrs, false)} />"])
  end

  defp ast_to_html({tag, attrs, inner, %{verbatim: true}}) do
    escape(["<#{tag}#{ast_attributes_to_string(attrs, false)}>", inner, "</#{tag}>"])
  end

  defp ast_to_html({tag, attrs, _inner, _meta}) when tag in @void_elements do
    "<#{tag}#{ast_attributes_to_string(attrs)} />"
  end

  defp ast_to_html({tag, attrs, inner, _meta}) do
    ["<#{tag}#{ast_attributes_to_string(attrs)}>", ast_to_html(inner), "</#{tag}>"]
  end

  @allowed_attributes ~W(style href src alt)

  defp ast_attributes_to_string(attrs, only_allowed \\ true) do
    for {key, val} <- attrs, only_allowed == false or key in @allowed_attributes do
      ~s/ #{key}="#{val}"/
    end
  end

  defp escape(binary) do
    {:safe, iodata} = Phoenix.HTML.html_escape(binary)
    iodata
  end
end
