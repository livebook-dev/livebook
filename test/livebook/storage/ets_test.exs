defmodule Livebook.Storage.EtsTest do
  use ExUnit.Case, async: false

  alias Livebook.Storage.Ets

  setup_all do
    {:ok, _pid} = Ets.start_link()

    []
  end

  describe "insert/3" do
    test "properly inserts a new key-value attribute" do
      assert %{
               id: "test",
               key1: "val1",
               key2: "val2"
             } = Ets.insert(:insert, "test", key1: "val1", key2: "val2")
    end

    test "replaces already existing attributes with new values" do
      assert %{
               key1: "val1",
               key2: "val2"
             } = Ets.insert(:insert, "replace", key1: "val1", key2: "val2")

      assert %{
               key1: "updated_val1",
               key2: "val2",
               key3: "val3"
             } = Ets.insert(:insert, "replace", key1: "updated_val1", key2: "val2", key3: "val3")
    end
  end

  test "fetch/2" do
    _entity = Ets.insert(:fetch, "test", key1: "val1")

    assert {:ok,
            %{
              id: "test",
              key1: "val1"
            }} = Ets.fetch(:fetch, "test")

    assert :error = Ets.fetch(:fetch, "unknown")
  end

  test "delete/2" do
    _entity = Ets.insert(:delete, "test", key1: "val1")

    assert {:ok, _entity} = Ets.fetch(:delete, "test")

    assert :ok = Ets.delete(:delete, "test")

    assert :error = Ets.fetch(:delete, "test")
  end

  test "all/1" do
    entity1 = Ets.insert(:all, "test1", key1: "val1")
    entity2 = Ets.insert(:all, "test2", key1: "val1")

    assert [^entity1, ^entity2] = Ets.all(:all)
  end
end
