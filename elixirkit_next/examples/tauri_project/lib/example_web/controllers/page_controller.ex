defmodule ExampleWeb.PageController do
  use ExampleWeb, :controller

  def home(conn, _params) do
    render(conn, :home)
  end
end
