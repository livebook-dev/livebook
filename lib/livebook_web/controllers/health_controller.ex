defmodule LivebookWeb.HealthController do
  use LivebookWeb, :controller

  def index(conn, _params) do
    version = Application.spec(:livebook, :vsn) |> List.to_string()

    json(conn, %{
      "application" => "livebook",
      "version" => version
    })
  end
end
