defmodule LivebookWeb.Layouts do
  use LivebookWeb, :html

  embed_templates "layouts/*"

  def dev?(), do: unquote(Mix.env() == :dev)
end
