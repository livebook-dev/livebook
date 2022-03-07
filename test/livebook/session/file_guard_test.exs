defmodule Livebook.Session.FileGuardTest do
  use ExUnit.Case, async: false

  import Livebook.TestHelpers

  alias Livebook.Session.FileGuard
  alias Livebook.FileSystem

  test "lock/2 returns an error if the given file is already locked" do
    file = FileSystem.File.local(p("/some/path"))

    assert :ok = FileGuard.lock(file, self())
    assert {:error, :already_in_use} = FileGuard.lock(file, self())
  end

  test "lock/2 is agnostic to irrelevant file system configuration" do
    fs1 = FileSystem.Local.new(default_path: p("/path/1/"))
    fs2 = FileSystem.Local.new(default_path: p("/path/2/"))

    # The file system has different configuration, but it's the same resource
    file1 = FileSystem.File.new(fs1, p("/some/path"))
    file2 = FileSystem.File.new(fs2, p("/some/path"))

    assert :ok = FileGuard.lock(file1, self())
    assert {:error, :already_in_use} = FileGuard.lock(file2, self())
  end

  test "unlock/1 unlocks the given file" do
    file = FileSystem.File.local(p("/some/path"))

    assert :ok = FileGuard.lock(file, self())
    :ok = FileGuard.unlock(file)
    assert :ok = FileGuard.lock(file, self())
  end

  test "file is automatically unloacked when the owner process termiantes" do
    file = FileSystem.File.local(p("/some/path"))

    owner = spawn(fn -> :ok end)
    :ok = FileGuard.lock(file, owner)
    assert :ok = FileGuard.lock(file, self())
  end
end
