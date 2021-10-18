defmodule LivebookWeb.HealthController do
  use LivebookWeb, :controller

  def index(conn, _params) do
    version = Application.spec(:livebook, :vsn) |> List.to_string()

    conn
    |> put_resp_header("Access-Control-Allow-Origin", "*")
    |> json(%{
      "application" => "livebook",
      "version" => version
    })
  end
end
