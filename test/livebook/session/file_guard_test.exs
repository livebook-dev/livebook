defmodule Livebook.Session.FileGuardTest do
  use ExUnit.Case, async: false

  alias Livebook.Session.FileGuard

  test "lock/2 returns an error if the given path is already locked" do
    assert :ok = FileGuard.lock("/some/path", self())
    assert {:error, :already_in_use} = FileGuard.lock("/some/path", self())
  end

  test "unlock/1 unlocks the given path" do
    assert :ok = FileGuard.lock("/some/path", self())
    :ok = FileGuard.unlock("/some/path")
    assert :ok = FileGuard.lock("/some/path", self())
  end

  test "path is automatically unloacked when the owner process termiantes" do
    owner = spawn(fn -> :ok end)
    :ok = FileGuard.lock("/some/path", owner)
    assert :ok = FileGuard.lock("/some/path", self())
  end
end
