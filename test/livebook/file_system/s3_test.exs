defmodule Livebook.FileSystem.S3Test do
  use Livebook.DataCase, async: true

  alias Livebook.FileSystem
  alias Livebook.FileSystem.S3

  setup do
    bypass = Bypass.open()
    file_system = build(:fs_s3, bucket_url: bucket_url(bypass.port))

    {:ok, bypass: bypass, file_system: file_system}
  end

  describe "FileSystem.default_path/1" do
    test "returns the root path" do
      file_system = build(:fs_s3, bucket_url: "https://example.com/mybucket", region: "auto")
      assert FileSystem.default_path(file_system) == "/"
    end
  end

  describe "common request errors" do
    test "authorization failure", %{bypass: bypass, file_system: file_system} do
      Bypass.expect_once(bypass, "GET", "/mybucket", fn conn ->
        conn
        |> Plug.Conn.put_resp_content_type("application/xml")
        |> Plug.Conn.resp(403, """
        <Error>
          <Message>Reason for authorization failure</Message>
        </Error>
        """)
      end)

      dir_path = "/dir/"

      assert {:error, "access denied, reason for authorization failure"} =
               FileSystem.list(file_system, dir_path, false)
    end

    test "an arbitrary error with message", %{bypass: bypass, file_system: file_system} do
      Bypass.expect_once(bypass, "GET", "/mybucket", fn conn ->
        conn
        |> Plug.Conn.put_resp_content_type("application/xml")
        |> Plug.Conn.resp(404, """
        <Error>
          <Message>Error message</Message>
        </Error>
        """)
      end)

      dir_path = "/dir/"

      assert {:error, "error message"} = FileSystem.list(file_system, dir_path, false)
    end

    test "successful response with unexpected body", %{bypass: bypass, file_system: file_system} do
      Bypass.expect_once(bypass, "GET", "/mybucket", fn conn ->
        conn
        |> Plug.Conn.put_resp_content_type("application/xml")
        |> Plug.Conn.resp(200, """
        <What>
          <No>Idea</No>
        </What>
        """)
      end)

      dir_path = "/dir/"

      assert {:error, "unexpected response"} = FileSystem.list(file_system, dir_path, false)
    end
  end

  describe "FileSystem.list/3" do
    test "returns an error when a nonexistent directory is given",
         %{bypass: bypass, file_system: file_system} do
      # When the directory doesn't exist, we get an empty list of matches

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

      dir_path = "/dir/"

      assert {:error, "no such file or directory"} =
               FileSystem.list(file_system, dir_path, false)
    end

    test "does not return an error when the root directory is empty",
         %{bypass: bypass, file_system: file_system} do
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

      dir_path = "/"

      assert {:ok, []} = FileSystem.list(file_system, dir_path, false)
    end

    test "returns a list of absolute child object paths",
         %{bypass: bypass, file_system: file_system} do
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

      dir_path = "/"

      assert {:ok, paths} = FileSystem.list(file_system, dir_path, false)

      assert Enum.sort(paths) == [
               "/dir",
               "/dir/",
               "/file.txt"
             ]
    end

    test "includes nested objects when called with recursive flag",
         %{bypass: bypass, file_system: file_system} do
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
    test "returns an error when a nonexistent key is given",
         %{bypass: bypass, file_system: file_system} do
      Bypass.expect_once(bypass, "GET", "/mybucket/nonexistent.txt", fn conn ->
        conn
        |> Plug.Conn.put_resp_content_type("application/xml")
        |> Plug.Conn.resp(404, """
        <Error>
          <Message>The specified key does not exist.</Message>
        </Error>
        """)
      end)

      file_path = "/nonexistent.txt"

      assert {:error, "no such file or directory"} = FileSystem.read(file_system, file_path)
    end

    test "returns object contents under the given key",
         %{bypass: bypass, file_system: file_system} do
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

      file_path = "/dir/file.txt"

      assert {:ok, ^content} = FileSystem.read(file_system, file_path)
    end
  end

  describe "FileSystem.write/3" do
    test "writes contents under the file key", %{bypass: bypass, file_system: file_system} do
      content = "content"

      Bypass.expect_once(bypass, "PUT", "/mybucket/dir/file.txt", fn conn ->
        assert {:ok, ^content, conn} = Plug.Conn.read_body(conn)

        Plug.Conn.resp(conn, 200, "")
      end)

      file_path = "/dir/file.txt"

      assert :ok = FileSystem.write(file_system, file_path, content)
    end

    # Google Cloud Storage XML API returns this type of response.
    test "returns success when the status is 200 even if the content type is text/html",
         %{bypass: bypass, file_system: file_system} do
      content = "content"

      Bypass.expect_once(bypass, "PUT", "/mybucket/dir/file.txt", fn conn ->
        assert {:ok, ^content, conn} = Plug.Conn.read_body(conn)

        conn
        |> Plug.Conn.put_resp_content_type("text/html; charset=UTF-8")
        |> Plug.Conn.resp(200, "")
      end)

      file_path = "/dir/file.txt"

      assert :ok = FileSystem.write(file_system, file_path, content)
    end
  end

  describe "FileSystem.create_dir/2" do
    test "write empty content under the directory key",
         %{bypass: bypass, file_system: file_system} do
      Bypass.expect_once(bypass, "PUT", "/mybucket/dir/", fn conn ->
        assert {:ok, "", conn} = Plug.Conn.read_body(conn)

        Plug.Conn.resp(conn, 200, "")
      end)

      dir_path = "/dir/"

      assert :ok = FileSystem.create_dir(file_system, dir_path)
    end
  end

  describe "FileSystem.remove/2" do
    test "returns successful value when a nonexistent key is given",
         %{bypass: bypass, file_system: file_system} do
      Bypass.expect_once(bypass, "DELETE", "/mybucket/file.txt", fn conn ->
        conn
        |> Plug.Conn.put_resp_content_type("application/xml")
        |> Plug.Conn.resp(404, """
        <Error>
          <Message>The specified key does not exist.</Message>
        </Error>
        """)
      end)

      file_path = "/file.txt"

      assert :ok = FileSystem.remove(file_system, file_path)
    end

    test "deletes object under the corresponding key", %{bypass: bypass, file_system: file_system} do
      Bypass.expect_once(bypass, "DELETE", "/mybucket/file.txt", fn conn ->
        Plug.Conn.resp(conn, 204, "")
      end)

      file_path = "/file.txt"

      assert :ok = FileSystem.remove(file_system, file_path)
    end

    test "when a directory is given, recursively lists and batch deletes all matching keys",
         %{bypass: bypass, file_system: file_system} do
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

      dir_path = "/dir/"

      assert :ok = FileSystem.remove(file_system, dir_path)
    end
  end

  describe "FileSystem.copy/3" do
    test "raises an error if the given paths have different type" do
      file_system = build(:fs_s3, bucket_url: "https://example.com/mybucket")
      src_file_path = "/src_file.txt"
      dest_dir_path = "/dir/"

      assert_raise ArgumentError, ~r/^expected paths of the same type/, fn ->
        FileSystem.copy(file_system, src_file_path, dest_dir_path)
      end
    end

    test "given file paths, returns an error if the source object does not exist",
         %{bypass: bypass, file_system: file_system} do
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

      src_file_path = "/src_file.txt"
      dest_file_path = "/dest_file.txt"

      assert {:error, "no such file or directory"} =
               FileSystem.copy(file_system, src_file_path, dest_file_path)
    end

    test "given file paths, copies contents into the new key",
         %{bypass: bypass, file_system: file_system} do
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

      src_file_path = "/src_file.txt"
      dest_file_path = "/dest_file.txt"

      assert :ok = FileSystem.copy(file_system, src_file_path, dest_file_path)
    end

    test "given directory paths, returns an error if the source directory does not exist",
         %{bypass: bypass, file_system: file_system} do
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

      src_dir_path = "/src_dir/"
      dest_dir_path = "/dest_dir/"

      assert {:error, "no such file or directory"} =
               FileSystem.copy(file_system, src_dir_path, dest_dir_path)
    end

    test "given directory paths, recursively lists all matching keys and individually copies objects",
         %{bypass: bypass, file_system: file_system} do
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

      src_dir_path = "/src_dir/"
      dest_dir_path = "/dest_dir/"

      assert :ok = FileSystem.copy(file_system, src_dir_path, dest_dir_path)
    end
  end

  describe "FileSystem.rename/3" do
    test "raises an error if the given paths have different type" do
      file_system = build(:fs_s3, bucket_url: "https://example.com/mybucket")
      src_file_path = "/src_file.txt"
      dest_dir_path = "/dir/"

      assert_raise ArgumentError, ~r/^expected paths of the same type/, fn ->
        FileSystem.rename(file_system, src_file_path, dest_dir_path)
      end
    end

    test "returns an error when the destination file exists",
         %{bypass: bypass, file_system: file_system} do
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

      src_file_path = "/src_file.txt"
      dest_file_path = "/dest_file.txt"

      assert {:error, "file already exists"} =
               FileSystem.rename(file_system, src_file_path, dest_file_path)
    end

    # Rename is implemented as copy and delete, both of which are
    # tested separately, so here's just one integration test to
    # verify this behaviour
    test "given file paths, copies the content and deletes the destination",
         %{bypass: bypass, file_system: file_system} do
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

      src_file_path = "/src_file.txt"
      dest_file_path = "/dest_file.txt"

      assert :ok = FileSystem.rename(file_system, src_file_path, dest_file_path)
    end
  end

  describe "FileSystem.etag_for/2" do
    test "returns an error when a nonexistent key is given",
         %{bypass: bypass, file_system: file_system} do
      Bypass.expect_once(bypass, "HEAD", "/mybucket/nonexistent.txt", fn conn ->
        Plug.Conn.resp(conn, 404, "")
      end)

      file_path = "/nonexistent.txt"

      assert {:error, "no such file or directory"} = FileSystem.etag_for(file_system, file_path)
    end

    test "returns the ETag value received from the server",
         %{bypass: bypass, file_system: file_system} do
      Bypass.expect_once(bypass, "HEAD", "/mybucket/nonexistent.txt", fn conn ->
        conn
        |> Plug.Conn.put_resp_header("ETag", "value")
        |> Plug.Conn.resp(200, "")
      end)

      file_path = "/nonexistent.txt"

      assert {:ok, "value"} = FileSystem.etag_for(file_system, file_path)
    end
  end

  describe "FileSystem.exists?/2" do
    test "returns false when the given object doesn't exist",
         %{bypass: bypass, file_system: file_system} do
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

      file_path = "/file.txt"

      assert {:ok, false} = FileSystem.exists?(file_system, file_path)
    end

    test "returns true when the given object exists", %{bypass: bypass, file_system: file_system} do
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

      file_path = "/file.txt"

      assert {:ok, true} = FileSystem.exists?(file_system, file_path)
    end
  end

  describe "FileSystem.resolve_path/3" do
    test "resolves relative paths" do
      file_system = build(:fs_s3, bucket_url: "https://example.com/mybucket/")

      assert "/dir/" = FileSystem.resolve_path(file_system, "/dir/", "")
      assert "/dir/file.txt" = FileSystem.resolve_path(file_system, "/dir/", "file.txt")
      assert "/dir/nested/" = FileSystem.resolve_path(file_system, "/dir/", "nested/")
      assert "/dir/" = FileSystem.resolve_path(file_system, "/dir/", ".")
      assert "/" = FileSystem.resolve_path(file_system, "/dir/", "..")

      assert "/file.txt" =
               FileSystem.resolve_path(file_system, "/dir/", "nested/../.././file.txt")
    end

    test "resolves absolute paths" do
      file_system = build(:fs_s3, bucket_url: "https://example.com/mybucket/")

      assert "/" = FileSystem.resolve_path(file_system, "/dir/", "/")
      assert "/file.txt" = FileSystem.resolve_path(file_system, "/dir/", "/file.txt")
      assert "/nested/" = FileSystem.resolve_path(file_system, "/dir/", "/nested/")

      assert "/nested/file.txt" =
               FileSystem.resolve_path(file_system, "/dir/", "///nested///other/..///file.txt")
    end
  end

  describe "FileSystem chunked write" do
    test "accumulates small chunks and sends a single request if the content is small",
         %{bypass: bypass, file_system: file_system} do
      Bypass.expect_once(bypass, "PUT", "/mybucket/dir/file.txt", fn conn ->
        assert {:ok, "ab", conn} = Plug.Conn.read_body(conn)

        Plug.Conn.resp(conn, 200, "")
      end)

      file_path = "/dir/file.txt"

      assert {:ok, state} =
               FileSystem.write_stream_init(file_system, file_path, part_size: 5_000)

      assert {:ok, state} = FileSystem.write_stream_chunk(file_system, state, "a")
      assert {:ok, state} = FileSystem.write_stream_chunk(file_system, state, "b")
      assert :ok = FileSystem.write_stream_finish(file_system, state)
    end

    test "creates a multi-part upload for contents over 50MB",
         %{bypass: bypass, file_system: file_system} do
      file_path = "/dir/file.txt"

      chunk_3kb = String.duplicate("a", 3_000)

      assert {:ok, state} =
               FileSystem.write_stream_init(file_system, file_path, part_size: 5_000)

      Bypass.expect_once(bypass, "POST", "/mybucket/dir/file.txt", fn conn ->
        assert %{"uploads" => ""} = conn.params

        # AWS does not return Content-Type for this request, so we emulate that
        Plug.Conn.resp(conn, 200, """
        <?xml version="1.0" encoding="UTF-8"?>
        <InitiateMultipartUploadResult>
          <UploadId>1</UploadId>
        </InitiateMultipartUploadResult>
        """)
      end)

      Bypass.expect_once(bypass, "PUT", "/mybucket/dir/file.txt", fn conn ->
        assert %{"uploadId" => "1", "partNumber" => "1"} = conn.params
        assert {:ok, body, conn} = Plug.Conn.read_body(conn)
        assert byte_size(body) == 5_000

        conn
        |> Plug.Conn.put_resp_header("ETag", "value1")
        |> Plug.Conn.resp(200, "")
      end)

      assert {:ok, state} = FileSystem.write_stream_chunk(file_system, state, chunk_3kb)
      assert {:ok, state} = FileSystem.write_stream_chunk(file_system, state, chunk_3kb)

      Bypass.expect_once(bypass, "PUT", "/mybucket/dir/file.txt", fn conn ->
        assert %{"uploadId" => "1", "partNumber" => "2"} = conn.params
        assert {:ok, body, conn} = Plug.Conn.read_body(conn)
        assert byte_size(body) == 5_000

        conn
        |> Plug.Conn.put_resp_header("ETag", "value2")
        |> Plug.Conn.resp(200, "")
      end)

      assert {:ok, state} = FileSystem.write_stream_chunk(file_system, state, chunk_3kb)
      assert {:ok, state} = FileSystem.write_stream_chunk(file_system, state, chunk_3kb)

      Bypass.expect_once(bypass, "PUT", "/mybucket/dir/file.txt", fn conn ->
        assert %{"uploadId" => "1", "partNumber" => "3"} = conn.params
        assert {:ok, body, conn} = Plug.Conn.read_body(conn)
        assert byte_size(body) == 2_000

        conn
        |> Plug.Conn.put_resp_header("ETag", "value3")
        |> Plug.Conn.resp(200, "")
      end)

      expected_body =
        """
        <CompleteMultipartUpload>
          <Part>
            <ETag>value1</ETag>
            <PartNumber>1</PartNumber>
          </Part>
          <Part>
            <ETag>value2</ETag>
            <PartNumber>2</PartNumber>
          </Part>
          <Part>
            <ETag>value3</ETag>
            <PartNumber>3</PartNumber>
          </Part>
        </CompleteMultipartUpload>
        """
        |> String.replace(~r/\s/, "")

      Bypass.expect_once(bypass, "POST", "/mybucket/dir/file.txt", fn conn ->
        assert {:ok, ^expected_body, conn} = Plug.Conn.read_body(conn)

        conn
        |> Plug.Conn.put_resp_content_type("application/xml")
        |> Plug.Conn.resp(200, """
        <CompleteMultipartUploadResult>
        </CompleteMultipartUploadResult>
        """)
      end)

      assert :ok = FileSystem.write_stream_finish(file_system, state)
    end

    test "aborts the multi-part upload when halted", %{bypass: bypass, file_system: file_system} do
      file_path = "/dir/file.txt"

      chunk_5kb = String.duplicate("a", 5_000)

      assert {:ok, state} =
               FileSystem.write_stream_init(file_system, file_path, part_size: 5_000)

      Bypass.expect_once(bypass, "POST", "/mybucket/dir/file.txt", fn conn ->
        assert %{"uploads" => ""} = conn.params

        # AWS does not return Content-Type for this request, so we emulate that
        Plug.Conn.resp(conn, 200, """
        <?xml version="1.0" encoding="UTF-8"?>
        <InitiateMultipartUploadResult>
          <UploadId>1</UploadId>
        </InitiateMultipartUploadResult>
        """)
      end)

      Bypass.expect_once(bypass, "PUT", "/mybucket/dir/file.txt", fn conn ->
        assert %{"uploadId" => "1", "partNumber" => "1"} = conn.params
        assert {:ok, body, conn} = Plug.Conn.read_body(conn)
        assert byte_size(body) == 5_000

        conn
        |> Plug.Conn.put_resp_header("ETag", "value1")
        |> Plug.Conn.resp(200, "")
      end)

      assert {:ok, state} = FileSystem.write_stream_chunk(file_system, state, chunk_5kb)

      Bypass.expect_once(bypass, "DELETE", "/mybucket/dir/file.txt", fn conn ->
        assert %{"uploadId" => "1"} = conn.params
        Plug.Conn.resp(conn, 204, "")
      end)

      assert :ok = FileSystem.write_stream_halt(file_system, state)
    end

    test "aborts the multi-part upload when finish fails",
         %{bypass: bypass, file_system: file_system} do
      file_path = "/dir/file.txt"

      chunk_5kb = String.duplicate("a", 5_000)

      assert {:ok, state} =
               FileSystem.write_stream_init(file_system, file_path, part_size: 5_000)

      Bypass.expect_once(bypass, "POST", "/mybucket/dir/file.txt", fn conn ->
        assert %{"uploads" => ""} = conn.params

        # AWS does not return Content-Type for this request, so we emulate that
        Plug.Conn.resp(conn, 200, """
        <?xml version="1.0" encoding="UTF-8"?>
        <InitiateMultipartUploadResult>
          <UploadId>1</UploadId>
        </InitiateMultipartUploadResult>
        """)
      end)

      Bypass.expect_once(bypass, "PUT", "/mybucket/dir/file.txt", fn conn ->
        assert %{"uploadId" => "1", "partNumber" => "1"} = conn.params
        assert {:ok, body, conn} = Plug.Conn.read_body(conn)
        assert byte_size(body) == 5_000

        conn
        |> Plug.Conn.put_resp_header("ETag", "value1")
        |> Plug.Conn.resp(200, "")
      end)

      assert {:ok, state} = FileSystem.write_stream_chunk(file_system, state, chunk_5kb)

      expected_body =
        """
        <CompleteMultipartUpload>
          <Part>
            <ETag>value1</ETag>
            <PartNumber>1</PartNumber>
          </Part>
        </CompleteMultipartUpload>
        """
        |> String.replace(~r/\s/, "")

      Bypass.expect_once(bypass, "POST", "/mybucket/dir/file.txt", fn conn ->
        assert {:ok, ^expected_body, conn} = Plug.Conn.read_body(conn)

        conn
        |> Plug.Conn.put_resp_content_type("application/xml")
        |> Plug.Conn.resp(500, """
        <Error>
          <Message>Error message</Message>
        </Error>
        """)
      end)

      Bypass.expect_once(bypass, "DELETE", "/mybucket/dir/file.txt", fn conn ->
        assert %{"uploadId" => "1"} = conn.params
        Plug.Conn.resp(conn, 204, "")
      end)

      assert {:error, "error message"} = FileSystem.write_stream_finish(file_system, state)
    end
  end

  describe "FileSystem.read_stream_into/2" do
    test "returns an error when a nonexistent key is given",
         %{bypass: bypass, file_system: file_system} do
      Bypass.expect_once(bypass, "GET", "/mybucket/nonexistent.txt", fn conn ->
        conn
        |> Plug.Conn.put_resp_content_type("application/xml")
        |> Plug.Conn.resp(404, """
        <Error>
          <Message>The specified key does not exist.</Message>
        </Error>
        """)
      end)

      file_path = "/nonexistent.txt"

      assert {:error, "no such file or directory"} =
               FileSystem.read_stream_into(file_system, file_path, <<>>)
    end

    test "collects regular response", %{bypass: bypass, file_system: file_system} do
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

      file_path = "/dir/file.txt"

      assert {:ok, ^content} = FileSystem.read_stream_into(file_system, file_path, <<>>)
    end

    test "collects chunked response", %{bypass: bypass, file_system: file_system} do
      chunk = String.duplicate("a", 2048)

      Bypass.expect_once(bypass, "GET", "/mybucket/dir/file.txt", fn conn ->
        conn = Plug.Conn.send_chunked(conn, 200)

        for _ <- 1..10, reduce: conn do
          conn ->
            {:ok, conn} = Plug.Conn.chunk(conn, chunk)
            conn
        end
      end)

      file_path = "/dir/file.txt"

      assert {:ok, content} = FileSystem.read_stream_into(file_system, file_path, <<>>)
      assert content == String.duplicate(chunk, 10)
    end
  end

  describe "FileSystem.load/2" do
    test "loads from atom keys" do
      bucket_url = "https://mybucket.s3.amazonaws.com"
      hash = :crypto.hash(:sha256, bucket_url)
      encrypted_hash = Base.url_encode64(hash, padding: false)

      fields = %{
        id: "s3-#{encrypted_hash}",
        bucket_url: bucket_url,
        region: "us-east-1",
        external_id: "123456789",
        access_key_id: "key",
        secret_access_key: "secret",
        hub_id: "personal-hub"
      }

      assert FileSystem.load(%S3{}, fields) == %S3{
               id: fields.id,
               bucket_url: fields.bucket_url,
               external_id: fields.external_id,
               region: fields.region,
               access_key_id: fields.access_key_id,
               secret_access_key: fields.secret_access_key,
               hub_id: fields.hub_id
             }
    end

    test "loads from string keys" do
      bucket_url = "https://mybucket.s3.amazonaws.com"
      hash = :crypto.hash(:sha256, bucket_url)
      encrypted_hash = Base.url_encode64(hash, padding: false)

      fields = %{
        "id" => "s3-#{encrypted_hash}",
        "bucket_url" => bucket_url,
        "region" => "us-east-1",
        "external_id" => "123456789",
        "access_key_id" => "key",
        "secret_access_key" => "secret",
        "hub_id" => "personal-hub"
      }

      assert FileSystem.load(%S3{}, fields) == %S3{
               id: fields["id"],
               bucket_url: fields["bucket_url"],
               external_id: fields["external_id"],
               region: fields["region"],
               access_key_id: fields["access_key_id"],
               secret_access_key: fields["secret_access_key"],
               hub_id: fields["hub_id"]
             }
    end
  end

  describe "FileSystem.dump/1" do
    test "dumps into a map ready to be stored" do
      file_system =
        build(:fs_s3,
          id: "personal-hub-s3-HsVlgSlWeap3BklGso76U1WdxiX7afWg2k4cXs8z7hI",
          bucket_url: "https://mybucket.s3.amazonaws.com"
        )

      assert FileSystem.dump(file_system) == %{
               id: "personal-hub-s3-HsVlgSlWeap3BklGso76U1WdxiX7afWg2k4cXs8z7hI",
               bucket_url: "https://mybucket.s3.amazonaws.com",
               region: "us-east-1",
               access_key_id: "key",
               secret_access_key: "secret",
               hub_id: "personal-hub",
               external_id: nil
             }
    end
  end

  # Helpers

  defp bucket_url(port), do: "http://localhost:#{port}/mybucket"
end
