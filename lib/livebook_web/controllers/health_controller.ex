defmodule LivebookWeb.HealthController do
  use LivebookWeb, :controller

  def index(conn, _params) do
    conn
    |> put_resp_header("Access-Control-Allow-Origin", "*")
    |> json(%{
      "application" => "livebook"
    })
  end
end
