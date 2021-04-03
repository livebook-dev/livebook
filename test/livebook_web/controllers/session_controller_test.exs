defmodule LivebookWeb.SessionControllerTest do
  use LivebookWeb.ConnCase, async: true

  alias Livebook.{SessionSupervisor, Session}

  describe "show_image" do
    test "returns not found when the given session does not exist", %{conn: conn} do
      conn = get(conn, Routes.session_path(conn, :show_image, "nonexistent", "image.jpg"))

      assert conn.status == 404
      assert conn.resp_body == "Not found"
    end

    test "returns not found when the given image does not exist", %{conn: conn} do
      {:ok, session_id} = SessionSupervisor.create_session()

      conn = get(conn, Routes.session_path(conn, :show_image, session_id, "nonexistent.jpg"))

      assert conn.status == 404
      assert conn.resp_body == "Not found"

      SessionSupervisor.delete_session(session_id)
    end

    test "returns the image when it does exist", %{conn: conn} do
      {:ok, session_id} = SessionSupervisor.create_session()
      %{images_dir: images_dir} = Session.get_summary(session_id)
      File.mkdir_p!(images_dir)
      images_dir |> Path.join("test.jpg") |> File.touch!()

      conn = get(conn, Routes.session_path(conn, :show_image, session_id, "test.jpg"))

      assert conn.status == 200
      assert get_resp_header(conn, "content-type") == ["image/jpeg"]

      SessionSupervisor.delete_session(session_id)
    end
  end
end
