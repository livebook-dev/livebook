defmodule LivebookWeb.SessionController do
  use LivebookWeb, :controller

  alias Livebook.Session

  def show_image(conn, %{"id" => id, "image" => image}) do
    %{images_dir: images_dir} = Session.get_summary(id)

    path = Path.join(images_dir, image)
    content_type = MIME.from_path(image)

    # TODO: reuse StaticPlug logic for caching and compression?
    conn
    |> put_resp_header("content-type", content_type)
    |> Plug.Conn.send_file(200, path)
  end
end
