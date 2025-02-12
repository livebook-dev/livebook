defmodule LivebookWeb.Output.PlainTextComponentTest do
  use LivebookWeb.ConnCase, async: true
  import Phoenix.LiveViewTest

  alias LivebookWeb.Output.PlainTextComponent

  test "renders chunks with styles" do
    assigns = %{
      id: "test-id",
      output: %{
        text: "Hello",
        style: [color: "red", unknown: "discarded", font_size: "invalid;discarded"]
      }
    }

    content = render_component(PlainTextComponent, assigns)
    assert content =~ ~s|<div id="test-id"|
    assert content =~ ~s|style="color: red">Hello</span>|
  end
end
