defmodule LivebookWeb.HealthController do
  use LivebookWeb, :controller

  def index(conn, _params) do
    version = Application.spec(:livebook, :vsn) |> List.to_string()

    json =
      Jason.encode!(%{
        "application" => "livebook",
        "version" => version
      })

    send_resp(conn, 200, json)
  end
end
