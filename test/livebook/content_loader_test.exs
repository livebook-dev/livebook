defmodule Livebook.ContentLoaderTest do
  use ExUnit.Case, async: true

  alias Livebook.ContentLoader

  describe "rewrite_url/1" do
    test "rewrites GitHub URLs to their raw counterpart" do
      url = "https://github.com/org/user/blob/main/notebooks/example.livemd"
      expected_url = "https://raw.githubusercontent.com/org/user/main/notebooks/example.livemd"

      assert ContentLoader.rewrite_url(url) == expected_url
    end

    test "rewrites Gist URLs to their raw counterpart" do
      url = "https://gist.github.com/user/hash"
      expected_url = "https://gist.githubusercontent.com/user/hash/raw"

      assert ContentLoader.rewrite_url(url) == expected_url
    end

    test "leaves arbitrary URLs unchanged" do
      url = "https://example.com/notebooks/example.livemd"

      assert ContentLoader.rewrite_url(url) == url
    end
  end

  describe "fetch_content/1" do
    setup do
      bypass = Bypass.open()
      {:ok, bypass: bypass}
    end

    test "returns an error when the request fails", %{bypass: bypass} do
      Bypass.expect_once(bypass, "GET", "/invalid", fn conn ->
        Plug.Conn.resp(conn, 500, "Error")
      end)

      assert ContentLoader.fetch_content(url(bypass.port) <> "/invalid") ==
               {:error, "failed to download notebook from the given URL"}
    end

    test "returns an error when the respone is HTML", %{bypass: bypass} do
      Bypass.expect_once(bypass, "GET", "/html", fn conn ->
        conn
        |> Plug.Conn.put_resp_content_type("text/html")
        |> Plug.Conn.resp(200, "<html></html>")
      end)

      assert ContentLoader.fetch_content(url(bypass.port) <> "/html") ==
               {:error, "invalid content type, make sure the URL points to live markdown"}
    end

    test "returns response body when the response is plain text", %{bypass: bypass} do
      Bypass.expect_once(bypass, "GET", "/notebook", fn conn ->
        conn
        |> Plug.Conn.put_resp_content_type("text/plain")
        |> Plug.Conn.resp(200, "# My notebook")
      end)

      assert ContentLoader.fetch_content(url(bypass.port) <> "/notebook") ==
               {:ok, "# My notebook"}
    end
  end

  defp url(port), do: "http://localhost:#{port}"
end
