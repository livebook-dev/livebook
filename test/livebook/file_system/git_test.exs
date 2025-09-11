defmodule Livebook.FileSystem.GitTest do
  use Livebook.DataCase, async: true

  @moduletag :git

  alias Livebook.FileSystem
  alias Livebook.FileSystem.Git

  setup %{test: test} = tags do
    data = test |> to_string() |> Base.encode32(padding: false, case: :lower)
    hub_id = Livebook.Hubs.Personal.id()
    id = Livebook.FileSystem.Utils.id("git", hub_id, data)
    file_system = build(:fs_git, id: id, hub_id: hub_id)

    if tags[:init] do
      Git.Client.init(file_system)
    end

    {:ok, file_system: file_system}
  end

  describe "FileSystem.default_path/1" do
    test "returns the root path", %{file_system: file_system} do
      assert FileSystem.default_path(file_system) == "/"
    end
  end

  describe "common request errors" do
    test "authorization failure", %{file_system: file_system} do
      file_system = %{file_system | key: "foo"}

      assert {:error, reason} = Git.Client.init(file_system)
      assert reason =~ "Permission denied (publickey)."
    end
  end

  describe "FileSystem.list/3" do
    @describetag init: true

    test "returns an empty list with invalid path", %{file_system: file_system} do
      assert FileSystem.list(file_system, "/path/", false) ==
               {:error, "no such file or directory"}
    end

    test "returns a list of absolute child object paths", %{file_system: file_system} do
      assert {:ok, paths} = FileSystem.list(file_system, "/", false)
      assert "/notebook_files/" in paths
      assert "/file.txt" in paths
    end
  end

  describe "FileSystem.read/2" do
    @describetag init: true
    test "returns an error when a nonexistent key is given", %{file_system: file_system} do
      assert FileSystem.read(file_system, "/another_file.txt") ==
               {:error, "path 'another_file.txt' does not exist in 'main'"}
    end

    test "returns object contents under the given key", %{file_system: file_system} do
      assert {:ok, content} = FileSystem.read(file_system, "/file.txt")
      assert content =~ "git file storage works"
    end
  end

  describe "FileSystem.write/3" do
    test "not implemented", %{file_system: file_system} do
      assert_raise RuntimeError, "not implemented", fn ->
        FileSystem.write(file_system, "/file.txt", "")
      end
    end
  end

  describe "FileSystem.create_dir/2" do
    test "not implemented", %{file_system: file_system} do
      assert_raise RuntimeError, "not implemented", fn ->
        FileSystem.create_dir(file_system, "/folder")
      end
    end
  end

  describe "FileSystem.remove/2" do
    test "not implemented", %{file_system: file_system} do
      assert_raise RuntimeError, "not implemented", fn ->
        FileSystem.remove(file_system, "/file.txt")
      end
    end
  end

  describe "FileSystem.copy/3" do
    test "not implemented", %{file_system: file_system} do
      assert_raise RuntimeError, "not implemented", fn ->
        FileSystem.copy(file_system, "/file.txt", "/folder/file.txt")
      end
    end
  end

  describe "FileSystem.rename/3" do
    test "not implemented", %{file_system: file_system} do
      assert_raise RuntimeError, "not implemented", fn ->
        FileSystem.rename(file_system, "/file.txt", "/another_file.txt")
      end
    end
  end

  describe "FileSystem.etag_for/2" do
    @describetag init: true
    test "returns an error when a nonexistent key is given", %{file_system: file_system} do
      assert {:error, reason} = FileSystem.etag_for(file_system, "/another_file.txt")
      assert reason =~ "path 'another_file.txt' does not exist in 'main'"
    end

    test "returns the ETag value received from the server", %{file_system: file_system} do
      assert {:ok, _etag} = FileSystem.etag_for(file_system, "/file.txt")
    end
  end

  describe "FileSystem.exists?/2" do
    @describetag init: true
    test "returns valid response", %{file_system: file_system} do
      assert {:ok, true} = FileSystem.exists?(file_system, "/file.txt")
      assert {:ok, false} = FileSystem.exists?(file_system, "/another_file.txt")
    end

    test "returns error with invalid path", %{file_system: file_system} do
      assert {:error, "../../.bashrc: '../../.bashrc' is outside repository at" <> _} =
               FileSystem.exists?(file_system, "../../.bashrc")
    end
  end

  describe "FileSystem.resolve_path/3" do
    test "resolves relative paths", %{file_system: file_system} do
      assert "/dir/" = FileSystem.resolve_path(file_system, "/dir/", "")
      assert "/dir/file.txt" = FileSystem.resolve_path(file_system, "/dir/", "file.txt")
      assert "/dir/nested/" = FileSystem.resolve_path(file_system, "/dir/", "nested/")
      assert "/dir/" = FileSystem.resolve_path(file_system, "/dir/", ".")
      assert "/" = FileSystem.resolve_path(file_system, "/dir/", "..")

      assert "/file.txt" =
               FileSystem.resolve_path(file_system, "/dir/", "nested/../.././file.txt")
    end

    test "resolves absolute paths", %{file_system: file_system} do
      assert "/" = FileSystem.resolve_path(file_system, "/dir/", "/")
      assert "/file.txt" = FileSystem.resolve_path(file_system, "/dir/", "/file.txt")
      assert "/nested/" = FileSystem.resolve_path(file_system, "/dir/", "/nested/")

      assert "/nested/file.txt" =
               FileSystem.resolve_path(file_system, "/dir/", "///nested///other/..///file.txt")
    end
  end

  describe "FileSystem chunked write" do
    test "not implemented", %{file_system: file_system} do
      assert_raise RuntimeError, "not implemented", fn ->
        FileSystem.write_stream_init(file_system, "/readme.txt", part_size: 5_000)
      end
    end
  end

  describe "FileSystem.read_stream_into/2" do
    test "not implemented", %{file_system: file_system} do
      assert_raise RuntimeError, "not implemented", fn ->
        FileSystem.read_stream_into(file_system, "/file.txt", <<>>)
      end
    end
  end

  describe "FileSystem.load/2" do
    test "loads from atom keys" do
      fields = %{
        id: "team-123456-git-Ios91o6sRIRnTmpRlO2jpwHPtFXlZh2FH6rkvxuN_8M",
        repo_url: "git@github.com:livebook-dev/test.git",
        branch: "main",
        key: "foo",
        hub_id: "team-123456",
        external_id: "1"
      }

      assert FileSystem.load(%Git{}, fields) == %Git{
               id: "team-123456-git-Ios91o6sRIRnTmpRlO2jpwHPtFXlZh2FH6rkvxuN_8M",
               repo_url: "git@github.com:livebook-dev/test.git",
               branch: "main",
               key: "foo",
               hub_id: "team-123456",
               external_id: "1"
             }
    end

    test "loads from string keys" do
      fields = %{
        "id" => "team-123456-git-Ios91o6sRIRnTmpRlO2jpwHPtFXlZh2FH6rkvxuN_8M",
        "repo_url" => "git@github.com:livebook-dev/test.git",
        "branch" => "main",
        "key" => "foo",
        "hub_id" => "team-123456",
        "external_id" => "1"
      }

      assert FileSystem.load(%Git{}, fields) == %Git{
               id: "team-123456-git-Ios91o6sRIRnTmpRlO2jpwHPtFXlZh2FH6rkvxuN_8M",
               repo_url: "git@github.com:livebook-dev/test.git",
               branch: "main",
               key: "foo",
               hub_id: "team-123456",
               external_id: "1"
             }
    end
  end

  describe "FileSystem.dump/1" do
    test "dumps into a map ready to be stored" do
      repo_url = "git@github.com:livebook-dev/test.git"
      hub_id = "team-123456"

      file_system =
        build(:fs_git,
          id: Livebook.FileSystem.Utils.id("git", hub_id, repo_url),
          repo_url: repo_url,
          branch: "main",
          key: "foo",
          hub_id: hub_id,
          external_id: "1"
        )

      assert FileSystem.dump(file_system) == %{
               id: "team-123456-git-Ios91o6sRIRnTmpRlO2jpwHPtFXlZh2FH6rkvxuN_8M",
               repo_url: "git@github.com:livebook-dev/test.git",
               branch: "main",
               key: "foo",
               hub_id: "team-123456",
               external_id: "1"
             }
    end
  end
end
