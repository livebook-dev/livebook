defmodule Livebook.FileSystem.LocalTest do
  use ExUnit.Case, async: true

  import Livebook.TestHelpers

  alias Livebook.FileSystem
  alias Livebook.FileSystem.Local

  describe "new/1" do
    test "raises when :default_path is not a directory" do
      assert_raise ArgumentError,
                   ~s{expected a directory path, got: "#{p("/notebook.livemd")}"},
                   fn ->
                     Local.new(default_path: p("/notebook.livemd"))
                   end
    end
  end

  describe "FileSystem.default_path/1" do
    test "defaults to the current directory" do
      file_system = Local.new()
      assert FileSystem.default_path(file_system) == File.cwd!() <> "/"
    end

    test "returns custom directory path if configured" do
      file_system = Local.new(default_path: p("/dir/"))
      assert FileSystem.default_path(file_system) == p("/dir/")
    end
  end

  describe "FileSystem.list/3" do
    @tag :tmp_dir
    test "returns an error when a nonexistent directory is given", %{tmp_dir: tmp_dir} do
      file_system = Local.new()
      dir_path = Path.join(tmp_dir, "nonexistent") <> "/"

      assert {:error, "no such file or directory"} = FileSystem.list(file_system, dir_path, false)
    end

    @tag :tmp_dir
    test "returns a list of absolute child file paths", %{tmp_dir: tmp_dir} do
      create_tree!(tmp_dir,
        dir: [
          nested: [
            "file.txt": "content"
          ]
        ],
        file: "content",
        "file.txt": "content"
      )

      file_system = Local.new()
      dir_path = tmp_dir <> "/"

      assert {:ok, paths} = FileSystem.list(file_system, dir_path, false)

      assert Enum.sort(paths) == [
               Path.join(tmp_dir, "dir") <> "/",
               Path.join(tmp_dir, "file"),
               Path.join(tmp_dir, "file.txt")
             ]
    end

    @tag :tmp_dir
    test "includes nested files when called with recursive flag", %{tmp_dir: tmp_dir} do
      create_tree!(tmp_dir,
        dir: [
          nested: [
            double_nested: [],
            "file.txt": "content"
          ]
        ],
        file: "content",
        "file.txt": "content"
      )

      file_system = Local.new()
      dir_path = tmp_dir <> "/"

      assert {:ok, paths} = FileSystem.list(file_system, dir_path, true)

      assert Enum.sort(paths) == [
               Path.join(tmp_dir, "dir") <> "/",
               Path.join(tmp_dir, "dir/nested") <> "/",
               Path.join(tmp_dir, "dir/nested/double_nested") <> "/",
               Path.join(tmp_dir, "dir/nested/file.txt"),
               Path.join(tmp_dir, "file"),
               Path.join(tmp_dir, "file.txt")
             ]
    end
  end

  describe "FileSystem.read/2" do
    @tag :tmp_dir
    test "returns an error when a nonexistent file is given", %{tmp_dir: tmp_dir} do
      file_system = Local.new()
      file_path = Path.join(tmp_dir, "nonexistent.txt")

      assert {:error, "no such file or directory"} = FileSystem.read(file_system, file_path)
    end

    @tag :tmp_dir
    test "returns file contents", %{tmp_dir: tmp_dir} do
      create_tree!(tmp_dir,
        dir: [
          "file.txt": "content"
        ]
      )

      file_system = Local.new()
      file_path = Path.join(tmp_dir, "dir/file.txt")

      assert {:ok, "content"} = FileSystem.read(file_system, file_path)
    end
  end

  describe "FileSystem.write/3" do
    @tag :tmp_dir
    test "writes file contents", %{tmp_dir: tmp_dir} do
      file_system = Local.new()
      file_path = Path.join(tmp_dir, "file.txt")

      assert :ok = FileSystem.write(file_system, file_path, "content")
      assert File.read!(file_path) == "content"
    end

    @tag :tmp_dir
    test "creates nonexistent directories", %{tmp_dir: tmp_dir} do
      file_system = Local.new()
      file_path = Path.join(tmp_dir, "dir/nested/file.txt")

      assert :ok = FileSystem.write(file_system, file_path, "content")
      assert File.read!(file_path) == "content"
    end

    @tag :tmp_dir
    test "overrides existing files", %{tmp_dir: tmp_dir} do
      create_tree!(tmp_dir,
        "file.txt": "content"
      )

      file_system = Local.new()
      file_path = Path.join(tmp_dir, "file.txt")

      assert :ok = FileSystem.write(file_system, file_path, "new content")
      assert File.read!(file_path) == "new content"
    end
  end

  describe "FileSystem.access/2" do
    @tag :tmp_dir
    test "returns an error when a nonexistent file is given", %{tmp_dir: tmp_dir} do
      file_system = Local.new()
      file_path = Path.join(tmp_dir, "file.txt")

      assert {:error, "no such file or directory"} = FileSystem.access(file_system, file_path)
    end

    @tag :tmp_dir
    test "returns permissions atom", %{tmp_dir: tmp_dir} do
      create_tree!(tmp_dir,
        "file.txt": "content"
      )

      file_system = Local.new()
      file_path = Path.join(tmp_dir, "file.txt")

      assert {:ok, :read_write} = FileSystem.access(file_system, file_path)

      File.chmod!(file_path, 0o444)
      assert {:ok, :read} = FileSystem.access(file_system, file_path)
    end

    @tag :tmp_dir
    test "handles directory paths", %{tmp_dir: tmp_dir} do
      create_tree!(tmp_dir,
        dir: []
      )

      file_system = Local.new()
      dir_path = Path.join(tmp_dir, "dir") <> "/"

      assert {:ok, :read_write} = FileSystem.access(file_system, dir_path)
    end
  end

  describe "FileSystem.create_dir/2" do
    @tag :tmp_dir
    test "creates the given directory and all nonexistent parent directories", %{tmp_dir: tmp_dir} do
      create_tree!(tmp_dir,
        dir: []
      )

      file_system = Local.new()
      dir_path = Path.join(tmp_dir, "nested/child/dir") <> "/"

      assert :ok = FileSystem.create_dir(file_system, dir_path)
      assert File.dir?(dir_path)
    end
  end

  describe "FileSystem.remove/2" do
    @tag :tmp_dir
    test "returns successful value when a nonexistent file is given", %{tmp_dir: tmp_dir} do
      file_system = Local.new()
      file_path = Path.join(tmp_dir, "file.txt")

      assert :ok = FileSystem.remove(file_system, file_path)
    end

    @tag :tmp_dir
    test "when a file is given, removes only this file", %{tmp_dir: tmp_dir} do
      create_tree!(tmp_dir,
        dir: [
          "file.txt": "content"
        ]
      )

      file_system = Local.new()
      file_path = Path.join(tmp_dir, "dir/file.txt")

      assert :ok = FileSystem.remove(file_system, file_path)

      refute File.exists?(file_path)
      assert File.exists?(Path.join(tmp_dir, "dir"))
    end

    @tag :tmp_dir
    test "when a directory is given, removes it with all contents", %{tmp_dir: tmp_dir} do
      create_tree!(tmp_dir,
        dir: [
          nested: [
            "file.txt": "content"
          ],
          "file.txt": "content"
        ],
        dir2: [],
        "file.txt": "content"
      )

      file_system = Local.new()
      dir_path = Path.join(tmp_dir, "dir") <> "/"

      assert :ok = FileSystem.remove(file_system, dir_path)

      refute File.exists?(dir_path)
      assert File.exists?(Path.join(tmp_dir, "dir2"))
      assert File.exists?(Path.join(tmp_dir, "file.txt"))
    end
  end

  describe "FileSystem.copy/3" do
    @tag :tmp_dir
    test "raises an error if the given paths have different type", %{tmp_dir: tmp_dir} do
      file_system = Local.new()
      src_file_path = Path.join(tmp_dir, "src_file.txt")
      dest_dir_path = Path.join(tmp_dir, "dir") <> "/"

      assert_raise ArgumentError, ~r/^expected paths of the same type/, fn ->
        FileSystem.copy(file_system, src_file_path, dest_dir_path)
      end
    end

    @tag :tmp_dir
    test "returns an error when the source file does not exist", %{tmp_dir: tmp_dir} do
      file_system = Local.new()
      src_file_path = Path.join(tmp_dir, "src_file.txt")
      dest_file_path = Path.join(tmp_dir, "dest_file.txt")

      assert {:error, "no such file or directory"} =
               FileSystem.copy(file_system, src_file_path, dest_file_path)
    end

    @tag :tmp_dir
    test "when files are given, copies the content", %{tmp_dir: tmp_dir} do
      create_tree!(tmp_dir,
        "src_file.txt": "content"
      )

      file_system = Local.new()
      src_file_path = Path.join(tmp_dir, "src_file.txt")
      dest_file_path = Path.join(tmp_dir, "dest_file.txt")

      assert :ok = FileSystem.copy(file_system, src_file_path, dest_file_path)

      assert File.read!(dest_file_path) == "content"
      assert File.read!(src_file_path) == "content"
    end

    @tag :tmp_dir
    test "creates nonexistent directories if necessary", %{tmp_dir: tmp_dir} do
      create_tree!(tmp_dir,
        "src_file.txt": "content"
      )

      file_system = Local.new()
      src_file_path = Path.join(tmp_dir, "src_file.txt")
      dest_file_path = Path.join(tmp_dir, "dir/nested/dest_file.txt")

      assert :ok = FileSystem.copy(file_system, src_file_path, dest_file_path)

      assert File.read!(dest_file_path) == "content"
      assert File.read!(src_file_path) == "content"
    end

    @tag :tmp_dir
    test "overrides destination file if already exists", %{tmp_dir: tmp_dir} do
      create_tree!(tmp_dir,
        "src_file.txt": "content",
        "dest_file.txt": "current content"
      )

      file_system = Local.new()
      src_file_path = Path.join(tmp_dir, "src_file.txt")
      dest_file_path = Path.join(tmp_dir, "dest_file.txt")

      assert :ok = FileSystem.copy(file_system, src_file_path, dest_file_path)

      assert File.read!(dest_file_path) == "content"
    end

    @tag :tmp_dir
    test "when a directories are given, copies all content recursively", %{tmp_dir: tmp_dir} do
      create_tree!(tmp_dir,
        src_dir: [
          nested: [
            "file.txt": "content"
          ],
          "file.txt": "content"
        ],
        dest_dir: []
      )

      file_system = Local.new()
      src_dir_path = Path.join(tmp_dir, "src_dir") <> "/"
      dest_dir_path = Path.join(tmp_dir, "dest_dir") <> "/"

      assert :ok = FileSystem.copy(file_system, src_dir_path, dest_dir_path)

      assert File.read!(Path.join(dest_dir_path, "nested/file.txt")) == "content"
      assert File.read!(Path.join(src_dir_path, "nested/file.txt")) == "content"
      assert File.read!(Path.join(dest_dir_path, "file.txt")) == "content"
      assert File.read!(Path.join(src_dir_path, "file.txt")) == "content"
    end
  end

  describe "FileSystem.rename/3" do
    @tag :tmp_dir
    test "raises an error if the given paths have different type", %{tmp_dir: tmp_dir} do
      file_system = Local.new()
      src_file_path = Path.join(tmp_dir, "src_file.txt")
      dest_dir_path = Path.join(tmp_dir, "dir") <> "/"

      assert_raise ArgumentError, ~r/^expected paths of the same type/, fn ->
        FileSystem.rename(file_system, src_file_path, dest_dir_path)
      end
    end

    @tag :tmp_dir
    test "returns an error when the souce file does not exist", %{tmp_dir: tmp_dir} do
      file_system = Local.new()
      src_file_path = Path.join(tmp_dir, "src_file.txt")
      dest_file_path = Path.join(tmp_dir, "dest_file.txt")

      assert {:error, "no such file or directory"} =
               FileSystem.rename(file_system, src_file_path, dest_file_path)
    end

    @tag :tmp_dir
    test "returns an error when the desination file exists", %{tmp_dir: tmp_dir} do
      create_tree!(tmp_dir,
        "src_file.txt": "content",
        "dest_file.txt": "content"
      )

      file_system = Local.new()
      src_file_path = Path.join(tmp_dir, "src_file.txt")
      dest_file_path = Path.join(tmp_dir, "dest_file.txt")

      assert {:error, "file already exists"} =
               FileSystem.rename(file_system, src_file_path, dest_file_path)
    end

    @tag :tmp_dir
    test "when files are given, moves source to destination", %{tmp_dir: tmp_dir} do
      create_tree!(tmp_dir,
        "src_file.txt": "content"
      )

      file_system = Local.new()
      src_file_path = Path.join(tmp_dir, "src_file.txt")
      dest_file_path = Path.join(tmp_dir, "dest_file.txt")

      assert :ok = FileSystem.rename(file_system, src_file_path, dest_file_path)

      assert File.read!(dest_file_path) == "content"
      refute File.exists?(src_file_path)
    end

    @tag :tmp_dir
    test "creates nonexistent directories if necessary", %{tmp_dir: tmp_dir} do
      create_tree!(tmp_dir,
        "src_file.txt": "content"
      )

      file_system = Local.new()
      src_file_path = Path.join(tmp_dir, "src_file.txt")
      dest_file_path = Path.join(tmp_dir, "dir/nested/dest_file.txt")

      assert :ok = FileSystem.rename(file_system, src_file_path, dest_file_path)

      assert File.read!(dest_file_path) == "content"
      refute File.exists?(src_file_path) == "content"
    end

    @tag :tmp_dir
    test "handles directories", %{tmp_dir: tmp_dir} do
      create_tree!(tmp_dir,
        src_dir: [
          nested: [
            "file.txt": "content"
          ],
          "file.txt": "content"
        ]
      )

      file_system = Local.new()
      src_dir_path = Path.join(tmp_dir, "src_dir") <> "/"
      dest_dir_path = Path.join(tmp_dir, "dest_dir") <> "/"

      assert :ok = FileSystem.rename(file_system, src_dir_path, dest_dir_path)

      assert File.read!(Path.join(dest_dir_path, "nested/file.txt")) == "content"
      assert File.read!(Path.join(dest_dir_path, "file.txt")) == "content"
      refute File.exists?(src_dir_path)
    end
  end

  describe "FileSystem.etag_for/2" do
    @tag :tmp_dir
    test "returns an error when a nonexistent file is given", %{tmp_dir: tmp_dir} do
      file_system = Local.new()
      file_path = Path.join(tmp_dir, "file.txt")

      assert {:error, "no such file or directory"} = FileSystem.etag_for(file_system, file_path)
    end

    @tag :tmp_dir
    test "returns different value only when the file is updated", %{tmp_dir: tmp_dir} do
      create_tree!(tmp_dir,
        "file.txt": "content"
      )

      file_system = Local.new()
      file_path = Path.join(tmp_dir, "file.txt")

      assert {:ok, etag1} = FileSystem.etag_for(file_system, file_path)
      assert {:ok, ^etag1} = FileSystem.etag_for(file_system, file_path)

      File.write!(file_path, "update")

      assert {:ok, etag2} = FileSystem.etag_for(file_system, file_path)

      assert etag1 != etag2
    end
  end

  describe "FileSystem.exists?/2" do
    @tag :tmp_dir
    test "returns false when the given file doesn't exist", %{tmp_dir: tmp_dir} do
      file_system = Local.new()
      file_path = Path.join(tmp_dir, "file.txt")

      assert {:ok, false} = FileSystem.exists?(file_system, file_path)
    end

    @tag :tmp_dir
    test "returns true when the given file exists", %{tmp_dir: tmp_dir} do
      create_tree!(tmp_dir,
        "file.txt": "content"
      )

      file_system = Local.new()
      file_path = Path.join(tmp_dir, "file.txt")

      assert {:ok, true} = FileSystem.exists?(file_system, file_path)
    end
  end

  describe "FileSystem.resolve_path/3" do
    test "resolves relative paths" do
      file_system = Local.new()

      assert p("/dir/") = FileSystem.resolve_path(file_system, p("/dir/"), "")
      assert p("/dir/file.txt") = FileSystem.resolve_path(file_system, p("/dir/"), "file.txt")
      assert p("/dir/nested/") = FileSystem.resolve_path(file_system, p("/dir/"), "nested/")
      assert p("/dir/") = FileSystem.resolve_path(file_system, p("/dir/"), ".")
      assert p("/") = FileSystem.resolve_path(file_system, p("/dir/"), "..")

      assert p("/file.txt") =
               FileSystem.resolve_path(file_system, p("/dir/"), "nested/../.././file.txt")
    end

    test "resolves absolute paths" do
      file_system = Local.new()

      assert p("/") = FileSystem.resolve_path(file_system, p("/dir/"), p("/"))
      assert p("/file.txt") = FileSystem.resolve_path(file_system, p("/dir/"), p("/file.txt"))
      assert p("/nested/") = FileSystem.resolve_path(file_system, p("/dir/"), p("/nested/"))

      assert p("/nested/file.txt") =
               FileSystem.resolve_path(
                 file_system,
                 p("/dir/"),
                 p("///nested///other/..///file.txt")
               )
    end
  end
end
