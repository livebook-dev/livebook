defmodule Livebook.FileSystem.S3Test do
  use ExUnit.Case, async: true

  alias Livebook.FileSystem
  alias Livebook.FileSystem.S3

  setup do
    bypass = Bypass.open()
    {:ok, bypass: bypass}
  end

  describe "new/3" do
    test "trims trailing slash in bucket URL" do
      assert %{bucket_url: "https://example.com/mybucket"} =
               S3.new("https://example.com/mybucket/", "key", "secret")
    end
  end

  describe "FileSystem.default_path/1" do
    test "returns the root path" do
      file_system = S3.new("https://example.com/mybucket", "key", "secret")
      assert FileSystem.default_path(file_system) == "/"
    end
  end

  describe "common request errors" do
    test "authorization failure", %{bypass: bypass} do
      Bypass.expect_once(bypass, "GET", "/mybucket", fn conn ->
        conn
        |> Plug.Conn.put_resp_content_type("application/xml")
        |> Plug.Conn.resp(403, """
        <Error>
          <Message>Reason for authorization failure</Message>
        </Error>
        """)
      end)

      file_system = S3.new(bucket_url(bypass.port), "key", "secret")
      dir_path = "/dir/"

      assert {:error, "access denied, reason for authorization failure"} =
               FileSystem.list(file_system, dir_path, false)
    end

    test "an arbitrary error with message", %{bypass: bypass} do
      Bypass.expect_once(bypass, "GET", "/mybucket", fn conn ->
        conn
        |> Plug.Conn.put_resp_content_type("application/xml")
        |> Plug.Conn.resp(500, """
        <Error>
          <Message>Error message</Message>
        </Error>
        """)
      end)

      file_system = S3.new(bucket_url(bypass.port), "key", "secret")
      dir_path = "/dir/"

      assert {:error, "error message"} = FileSystem.list(file_system, dir_path, false)
    end

    test "successful response with unexpected body", %{bypass: bypass} do
      Bypass.expect_once(bypass, "GET", "/mybucket", fn conn ->
        conn
        |> Plug.Conn.put_resp_content_type("application/xml")
        |> Plug.Conn.resp(200, """
        <What>
          <No>Idea</No>
        </What>
        """)
      end)

      file_system = S3.new(bucket_url(bypass.port), "key", "secret")
      dir_path = "/dir/"

      assert {:error, "unexpected response"} = FileSystem.list(file_system, dir_path, false)
    end
  end

  describe "FileSystem.list/3" do
    test "returns an error when a nonexistent directory is given", %{bypass: bypass} do
      # When the directory doesn't exist, we get an empty list of maches

      Bypass.expect_once(bypass, "GET", "/mybucket", fn conn ->
        assert %{"delimiter" => "/"} = conn.params

        conn
        |> Plug.Conn.put_resp_content_type("application/xml")
        |> Plug.Conn.resp(200, """
        <ListBucketResult>
          <Name>mybucket</Name>
        </ListBucketResult>
        """)
      end)

      file_system = S3.new(bucket_url(bypass.port), "key", "secret")
      dir_path = "/dir/"

      assert {:error, "no such file or directory"} = FileSystem.list(file_system, dir_path, false)
    end

    test "does not return an error when the root directory is empty", %{bypass: bypass} do
      Bypass.expect_once(bypass, "GET", "/mybucket", fn conn ->
        assert %{"delimiter" => "/"} = conn.params

        conn
        |> Plug.Conn.put_resp_content_type("application/xml")
        |> Plug.Conn.resp(200, """
        <ListBucketResult>
          <Name>mybucket</Name>
        </ListBucketResult>
        """)
      end)

      file_system = S3.new(bucket_url(bypass.port), "key", "secret")
      dir_path = "/"

      assert {:ok, []} = FileSystem.list(file_system, dir_path, false)
    end

    test "returns a list of absolute child object paths", %{bypass: bypass} do
      Bypass.expect_once(bypass, "GET", "/mybucket", fn conn ->
        assert %{"delimiter" => "/"} = conn.params

        conn
        |> Plug.Conn.put_resp_content_type("application/xml")
        |> Plug.Conn.resp(200, """
        <ListBucketResult>
          <Name>mybucket</Name>
          <CommonPrefixes>
            <Prefix>dir/</Prefix>
          </CommonPrefixes>
          <Contents>
            <Key>dir</Key>
          </Contents>
          <Contents>
            <Key>file.txt</Key>
          </Contents>
        </ListBucketResult>
        """)
      end)

      file_system = S3.new(bucket_url(bypass.port), "key", "secret")
      dir_path = "/"

      assert {:ok, paths} = FileSystem.list(file_system, dir_path, false)

      assert Enum.sort(paths) == [
               "/dir",
               "/dir/",
               "/file.txt"
             ]
    end

    test "includes nested objects when called with recursive flag", %{bypass: bypass} do
      Bypass.expect_once(bypass, "GET", "/mybucket", fn conn ->
        assert %{"delimiter" => ""} = conn.params

        conn
        |> Plug.Conn.put_resp_content_type("application/xml")
        |> Plug.Conn.resp(200, """
        <ListBucketResult>
          <Name>mybucket</Name>
          <Contents>
            <Key>dir/</Key>
          </Contents>
          <Contents>
            <Key>dir/file.txt</Key>
          </Contents>
          <Contents>
            <Key>dir/nested/</Key>
          </Contents>
          <Contents>
            <Key>dir/nested/file.txt</Key>
          </Contents>
          <Contents>
            <Key>dir</Key>
          </Contents>
          <Contents>
            <Key>file.txt</Key>
          </Contents>
        </ListBucketResult>
        """)
      end)

      file_system = S3.new(bucket_url(bypass.port), "key", "secret")
      dir_path = "/"

      assert {:ok, paths} = FileSystem.list(file_system, dir_path, true)

      assert Enum.sort(paths) == [
               "/dir",
               "/dir/",
               "/dir/file.txt",
               "/dir/nested/",
               "/dir/nested/file.txt",
               "/file.txt"
             ]
    end
  end

  describe "FileSystem.read/2" do
    test "returns an error when a nonexistent key is given", %{bypass: bypass} do
      Bypass.expect_once(bypass, "GET", "/mybucket/nonexistent.txt", fn conn ->
        conn
        |> Plug.Conn.put_resp_content_type("application/xml")
        |> Plug.Conn.resp(404, """
        <Error>
          <Message>The specified key does not exist.</Message>
        </Error>
        """)
      end)

      file_system = S3.new(bucket_url(bypass.port), "key", "secret")
      file_path = "/nonexistent.txt"

      assert {:error, "no such file or directory"} = FileSystem.read(file_system, file_path)
    end

    test "returns object contents under the given key", %{bypass: bypass} do
      content = """
      <MyData>
        <Info>this should not be parsed</Info>
      </MyData>
      """

      Bypass.expect_once(bypass, "GET", "/mybucket/dir/file.txt", fn conn ->
        # When reading the content should be returned as binary,
        # regardless the content type
        conn
        |> Plug.Conn.put_resp_content_type("application/xml")
        |> Plug.Conn.resp(200, content)
      end)

      file_system = S3.new(bucket_url(bypass.port), "key", "secret")
      file_path = "/dir/file.txt"

      assert {:ok, ^content} = FileSystem.read(file_system, file_path)
    end
  end

  describe "FileSystem.write/3" do
    test "writes contents under the file key", %{bypass: bypass} do
      content = "content"

      Bypass.expect_once(bypass, "PUT", "/mybucket/dir/file.txt", fn conn ->
        assert {:ok, ^content, conn} = Plug.Conn.read_body(conn)

        Plug.Conn.resp(conn, 200, "")
      end)

      file_system = S3.new(bucket_url(bypass.port), "key", "secret")
      file_path = "/dir/file.txt"

      assert :ok = FileSystem.write(file_system, file_path, content)
    end

    # Google Cloud Storage XML API returns this type of response.
    test "returns success when the status is 200 even if the content type is text/html", %{
      bypass: bypass
    } do
      content = "content"

      Bypass.expect_once(bypass, "PUT", "/mybucket/dir/file.txt", fn conn ->
        assert {:ok, ^content, conn} = Plug.Conn.read_body(conn)

        conn
        |> Plug.Conn.put_resp_content_type("text/html; charset=UTF-8")
        |> Plug.Conn.resp(200, "")
      end)

      file_system = S3.new(bucket_url(bypass.port), "key", "secret")
      file_path = "/dir/file.txt"

      assert :ok = FileSystem.write(file_system, file_path, content)
    end
  end

  describe "FileSystem.create_dir/2" do
    test "write empty content under the directory key", %{bypass: bypass} do
      Bypass.expect_once(bypass, "PUT", "/mybucket/dir/", fn conn ->
        assert {:ok, "", conn} = Plug.Conn.read_body(conn)

        Plug.Conn.resp(conn, 200, "")
      end)

      file_system = S3.new(bucket_url(bypass.port), "key", "secret")
      dir_path = "/dir/"

      assert :ok = FileSystem.create_dir(file_system, dir_path)
    end
  end

  describe "FileSystem.remove/2" do
    test "returns successful value when a nonexistent key is given", %{bypass: bypass} do
      Bypass.expect_once(bypass, "DELETE", "/mybucket/file.txt", fn conn ->
        conn
        |> Plug.Conn.put_resp_content_type("application/xml")
        |> Plug.Conn.resp(404, """
        <Error>
          <Message>The specified key does not exist.</Message>
        </Error>
        """)
      end)

      file_system = S3.new(bucket_url(bypass.port), "key", "secret")
      file_path = "/file.txt"

      assert :ok = FileSystem.remove(file_system, file_path)
    end

    test "deletes object under the corresponding key", %{bypass: bypass} do
      Bypass.expect_once(bypass, "DELETE", "/mybucket/file.txt", fn conn ->
        Plug.Conn.resp(conn, 204, "")
      end)

      file_system = S3.new(bucket_url(bypass.port), "key", "secret")
      file_path = "/file.txt"

      assert :ok = FileSystem.remove(file_system, file_path)
    end

    test "when a directory is given, recursively lists and batch deletes all matching keys",
         %{bypass: bypass} do
      Bypass.expect_once(bypass, "GET", "/mybucket", fn conn ->
        assert %{"prefix" => "dir/", "delimiter" => ""} = conn.params

        conn
        |> Plug.Conn.put_resp_content_type("application/xml")
        |> Plug.Conn.resp(200, """
        <ListBucketResult>
          <Name>mybucket</Name>
          <Contents>
            <Key>dir/</Key>
          </Contents>
          <Contents>
            <Key>dir/file.txt</Key>
          </Contents>
          <Contents>
            <Key>dir/nested/</Key>
          </Contents>
          <Contents>
            <Key>dir/nested/file.txt</Key>
          </Contents>
        </ListBucketResult>
        """)
      end)

      expected_body =
        """
        <Delete>
          <Object>
            <Key>dir/</Key>
          </Object>
          <Object>
            <Key>dir/file.txt</Key>
          </Object>
          <Object>
            <Key>dir/nested/</Key>
          </Object>
          <Object>
            <Key>dir/nested/file.txt</Key>
          </Object>
          <Quiet>true</Quiet>
        </Delete>
        """
        |> String.replace(~r/\s/, "")

      Bypass.expect_once(bypass, "POST", "/mybucket", fn conn ->
        assert %{"delete" => ""} = conn.params

        assert {:ok, ^expected_body, conn} = Plug.Conn.read_body(conn)

        conn
        |> Plug.Conn.put_resp_content_type("application/xml")
        |> Plug.Conn.resp(200, """
        <DeleteResult>
        </DeleteResult>
        """)
      end)

      file_system = S3.new(bucket_url(bypass.port), "key", "secret")
      dir_path = "/dir/"

      assert :ok = FileSystem.remove(file_system, dir_path)
    end
  end

  describe "FileSystem.copy/3" do
    test "raises an error if the given paths have different type" do
      file_system = S3.new("https://example.com/mybucket", "key", "secret")
      src_file_path = "/src_file.txt"
      dest_dir_path = "/dir/"

      assert_raise ArgumentError, ~r/^expected paths of the same type/, fn ->
        FileSystem.copy(file_system, src_file_path, dest_dir_path)
      end
    end

    test "given file paths, returns an error if the source object does not exist",
         %{bypass: bypass} do
      # Request for the bucket name
      Bypass.expect_once(bypass, "GET", "/mybucket", fn conn ->
        conn
        |> Plug.Conn.put_resp_content_type("application/xml")
        |> Plug.Conn.resp(200, """
        <ListBucketResult>
          <Name>mybucket</Name>
        </ListBucketResult>
        """)
      end)

      Bypass.expect_once(bypass, "PUT", "/mybucket/dest_file.txt", fn conn ->
        assert ["mybucket/src_file.txt"] = Plug.Conn.get_req_header(conn, "x-amz-copy-source")

        conn
        |> Plug.Conn.put_resp_content_type("application/xml")
        |> Plug.Conn.resp(404, """
        <Error>
          <Message>The specified key does not exist.</Message>
        </Error>
        """)
      end)

      file_system = S3.new(bucket_url(bypass.port), "key", "secret")
      src_file_path = "/src_file.txt"
      dest_file_path = "/dest_file.txt"

      assert {:error, "no such file or directory"} =
               FileSystem.copy(file_system, src_file_path, dest_file_path)
    end

    test "given file paths, copies contents into the new key", %{bypass: bypass} do
      # Request for the bucket name
      Bypass.expect_once(bypass, "GET", "/mybucket", fn conn ->
        conn
        |> Plug.Conn.put_resp_content_type("application/xml")
        |> Plug.Conn.resp(200, """
        <ListBucketResult>
          <Name>mybucket</Name>
        </ListBucketResult>
        """)
      end)

      Bypass.expect_once(bypass, "PUT", "/mybucket/dest_file.txt", fn conn ->
        assert ["mybucket/src_file.txt"] = Plug.Conn.get_req_header(conn, "x-amz-copy-source")

        conn
        |> Plug.Conn.put_resp_content_type("application/xml")
        |> Plug.Conn.resp(200, """
        <CopyObjectResult>
        </CopyObjectResult>
        """)
      end)

      file_system = S3.new(bucket_url(bypass.port), "key", "secret")
      src_file_path = "/src_file.txt"
      dest_file_path = "/dest_file.txt"

      assert :ok = FileSystem.copy(file_system, src_file_path, dest_file_path)
    end

    test "given directory paths, returns an error if the source directory does not exist",
         %{bypass: bypass} do
      # Directory listing with no results
      Bypass.expect_once(bypass, "GET", "/mybucket", fn conn ->
        assert %{"prefix" => "src_dir/", "delimiter" => ""} = conn.params

        conn
        |> Plug.Conn.put_resp_content_type("application/xml")
        |> Plug.Conn.resp(200, """
        <ListBucketResult>
          <Name>mybucket</Name>
        </ListBucketResult>
        """)
      end)

      file_system = S3.new(bucket_url(bypass.port), "key", "secret")
      src_dir_path = "/src_dir/"
      dest_dir_path = "/dest_dir/"

      assert {:error, "no such file or directory"} =
               FileSystem.copy(file_system, src_dir_path, dest_dir_path)
    end

    test "given directory paths, recursively lists all matching keys and individually copies objects",
         %{bypass: bypass} do
      # Directory listing
      Bypass.expect_once(bypass, "GET", "/mybucket", fn conn ->
        assert %{"prefix" => "src_dir/", "delimiter" => ""} = conn.params

        conn
        |> Plug.Conn.put_resp_content_type("application/xml")
        |> Plug.Conn.resp(200, """
        <ListBucketResult>
          <Name>mybucket</Name>
          <Contents>
            <Key>src_dir/</Key>
          </Contents>
          <Contents>
            <Key>src_dir/file.txt</Key>
          </Contents>
          <Contents>
            <Key>src_dir/nested/</Key>
          </Contents>
          <Contents>
            <Key>src_dir/nested/file.txt</Key>
          </Contents>
        </ListBucketResult>
        """)
      end)

      # Copy requests, one per object
      for {src_key, dest_key} <- [
            {"src_dir/", "dest_dir/"},
            {"src_dir/file.txt", "dest_dir/file.txt"},
            {"src_dir/nested/", "dest_dir/nested/"},
            {"src_dir/nested/file.txt", "dest_dir/nested/file.txt"}
          ] do
        Bypass.expect_once(bypass, "PUT", "/mybucket/#{dest_key}", fn conn ->
          assert [src_header] = Plug.Conn.get_req_header(conn, "x-amz-copy-source")
          assert src_header == "mybucket/#{src_key}"

          conn
          |> Plug.Conn.put_resp_content_type("application/xml")
          |> Plug.Conn.resp(200, """
          <CopyObjectResult>
          </CopyObjectResult>
          """)
        end)
      end

      file_system = S3.new(bucket_url(bypass.port), "key", "secret")
      src_dir_path = "/src_dir/"
      dest_dir_path = "/dest_dir/"

      assert :ok = FileSystem.copy(file_system, src_dir_path, dest_dir_path)
    end
  end

  describe "FileSystem.rename/3" do
    test "raises an error if the given paths have different type" do
      file_system = S3.new("https://example.com/mybucket", "key", "secret")
      src_file_path = "/src_file.txt"
      dest_dir_path = "/dir/"

      assert_raise ArgumentError, ~r/^expected paths of the same type/, fn ->
        FileSystem.rename(file_system, src_file_path, dest_dir_path)
      end
    end

    test "returns an error when the desination file exists", %{bypass: bypass} do
      # Existence is verified by listing
      Bypass.expect_once(bypass, "GET", "/mybucket", fn conn ->
        assert %{"prefix" => "dest_file.txt", "delimiter" => "/"} = conn.params

        conn
        |> Plug.Conn.put_resp_content_type("application/xml")
        |> Plug.Conn.resp(200, """
        <ListBucketResult>
          <Name>mybucket</Name>
          <Contents>
            <Key>dest_file.txt</Key>
          </Contents>
        </ListBucketResult>
        """)
      end)

      file_system = S3.new(bucket_url(bypass.port), "key", "secret")
      src_file_path = "/src_file.txt"
      dest_file_path = "/dest_file.txt"

      assert {:error, "file already exists"} =
               FileSystem.rename(file_system, src_file_path, dest_file_path)
    end

    # Rename is implemented as copy and delete, both of which are
    # tested separately, so here's just one integration test to
    # verify this behaviour
    test "given file paths, copies the content and deletes the destination", %{bypass: bypass} do
      # Expects two requests:
      #   * destination existence check by listing, we return no entries
      #   * bucket name request
      Bypass.expect(bypass, "GET", "/mybucket", fn conn ->
        conn
        |> Plug.Conn.put_resp_content_type("application/xml")
        |> Plug.Conn.resp(200, """
        <ListBucketResult>
          <Name>mybucket</Name>
        </ListBucketResult>
        """)
      end)

      Bypass.expect_once(bypass, "PUT", "/mybucket/dest_file.txt", fn conn ->
        assert ["mybucket/src_file.txt"] = Plug.Conn.get_req_header(conn, "x-amz-copy-source")

        conn
        |> Plug.Conn.put_resp_content_type("application/xml")
        |> Plug.Conn.resp(200, """
        <CopyObjectResult>
        </CopyObjectResult>
        """)
      end)

      Bypass.expect_once(bypass, "DELETE", "/mybucket/src_file.txt", fn conn ->
        Plug.Conn.resp(conn, 204, "")
      end)

      file_system = S3.new(bucket_url(bypass.port), "key", "secret")
      src_file_path = "/src_file.txt"
      dest_file_path = "/dest_file.txt"

      assert :ok = FileSystem.rename(file_system, src_file_path, dest_file_path)
    end
  end

  describe "FileSystem.etag_for/2" do
    test "returns an error when a nonexistent key is given", %{bypass: bypass} do
      Bypass.expect_once(bypass, "HEAD", "/mybucket/nonexistent.txt", fn conn ->
        Plug.Conn.resp(conn, 404, "")
      end)

      file_system = S3.new(bucket_url(bypass.port), "key", "secret")
      file_path = "/nonexistent.txt"

      assert {:error, "no such file or directory"} = FileSystem.etag_for(file_system, file_path)
    end

    test "returns the ETag value received from the server", %{bypass: bypass} do
      Bypass.expect_once(bypass, "HEAD", "/mybucket/nonexistent.txt", fn conn ->
        conn
        |> Plug.Conn.put_resp_header("ETag", "value")
        |> Plug.Conn.resp(200, "")
      end)

      file_system = S3.new(bucket_url(bypass.port), "key", "secret")
      file_path = "/nonexistent.txt"

      assert {:ok, "value"} = FileSystem.etag_for(file_system, file_path)
    end
  end

  describe "FileSystem.exists?/2" do
    test "returns false when the given object doesn't exist", %{bypass: bypass} do
      # Existence is verified by listing
      Bypass.expect_once(bypass, "GET", "/mybucket", fn conn ->
        assert %{"prefix" => "file.txt", "delimiter" => "/"} = conn.params

        conn
        |> Plug.Conn.put_resp_content_type("application/xml")
        |> Plug.Conn.resp(200, """
        <ListBucketResult>
          <Name>mybucket</Name>
        </ListBucketResult>
        """)
      end)

      file_system = S3.new(bucket_url(bypass.port), "key", "secret")
      file_path = "/file.txt"

      assert {:ok, false} = FileSystem.exists?(file_system, file_path)
    end

    test "returns true when the given object exists", %{bypass: bypass} do
      # Existence is verified by listing
      Bypass.expect_once(bypass, "GET", "/mybucket", fn conn ->
        assert %{"prefix" => "file.txt", "delimiter" => "/"} = conn.params

        conn
        |> Plug.Conn.put_resp_content_type("application/xml")
        |> Plug.Conn.resp(200, """
        <ListBucketResult>
          <Name>mybucket</Name>
          <Contents>
            <Key>file.txt</Key>
          </Contents>
        </ListBucketResult>
        """)
      end)

      file_system = S3.new(bucket_url(bypass.port), "key", "secret")
      file_path = "/file.txt"

      assert {:ok, true} = FileSystem.exists?(file_system, file_path)
    end
  end

  describe "FileSystem.resolve_path/3" do
    test "resolves relative paths" do
      file_system = S3.new("https://example.com/mybucket/", "key", "secret")

      assert "/dir/" = FileSystem.resolve_path(file_system, "/dir/", "")
      assert "/dir/file.txt" = FileSystem.resolve_path(file_system, "/dir/", "file.txt")
      assert "/dir/nested/" = FileSystem.resolve_path(file_system, "/dir/", "nested/")
      assert "/dir/" = FileSystem.resolve_path(file_system, "/dir/", ".")
      assert "/" = FileSystem.resolve_path(file_system, "/dir/", "..")

      assert "/file.txt" =
               FileSystem.resolve_path(file_system, "/dir/", "nested/../.././file.txt")
    end

    test "resolves absolute paths" do
      file_system = S3.new("https://example.com/mybucket/", "key", "secret")

      assert "/" = FileSystem.resolve_path(file_system, "/dir/", "/")
      assert "/file.txt" = FileSystem.resolve_path(file_system, "/dir/", "/file.txt")
      assert "/nested/" = FileSystem.resolve_path(file_system, "/dir/", "/nested/")

      assert "/nested/file.txt" =
               FileSystem.resolve_path(file_system, "/dir/", "///nested///other/..///file.txt")
    end
  end

  # Helpers

  defp bucket_url(port), do: "http://localhost:#{port}/mybucket"
end
