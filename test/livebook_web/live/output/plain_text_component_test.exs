defmodule LivebookWeb.Output.PlainTextComponentTest do
  use LivebookWeb.ConnCase, async: true
  import Phoenix.LiveViewTest

  alias LivebookWeb.Output.PlainTextComponent

  test "renders chunks with styles" do
    assigns = %{
      id: "test-id",
      output: %{text: "Hello", style: [color: "red"]}
    }

    content = render_component(PlainTextComponent, assigns)
    assert content =~ "<div id=\"test-id\""
    assert content =~ "style=\"color: red\">Hello</span>"
  end

  test "raises on unsafe styles" do
    assert_raise ArgumentError, fn ->
      render_component(PlainTextComponent, %{
        id: "test-id",
        output: %{text: "Hello", style: [color: "red;blue"]}
      })
    end

    assert_raise FunctionClauseError, fn ->
      render_component(PlainTextComponent, %{
        id: "test-id",
        output: %{text: "Hello", style: [unknown: "red"]}
      })
    end
  end
end
