defmodule LivebookWeb.SessionController do
  use LivebookWeb, :controller

  def show_image(conn, %{"id" => id, "image" => image}) do
    # TODO: get image provider for session with `id`
    # stream image from the provider
    # text(conn, image)

    content_type = MIME.from_path(image)

    # TODO: reuse StaticPlug logic for caching and compression?
    conn
    |> put_resp_header("content-type", content_type)
    |> Plug.Conn.send_file(200, Path.join("/tmp", image))
  end
end
