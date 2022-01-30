defmodule Livebook.Storage.EtsTest do
  use ExUnit.Case, async: false

  alias Livebook.Storage.Ets

  describe "insert/3 and fetch/2" do
    test "properly inserts a new key-value attribute" do
      assert :ok = Ets.insert(:insert, "test", key1: "val1", key2: "val2")

      assert {:ok,
              %{
                id: "test",
                key1: "val1",
                key2: "val2"
              }} = Ets.fetch(:insert, "test")
    end

    test "replaces already existing attributes with new values" do
      assert :ok = Ets.insert(:insert, "replace", key1: "val1", key2: "val2")

      assert {:ok,
              %{
                key1: "val1",
                key2: "val2"
              }} = Ets.fetch(:insert, "replace")

      assert :ok =
               Ets.insert(:insert, "replace", key1: "updated_val1", key2: "val2", key3: "val3")

      assert {:ok,
              %{
                key1: "updated_val1",
                key2: "val2",
                key3: "val3"
              }} = Ets.fetch(:insert, "replace")
    end
  end

  test "fetch/2" do
    :ok = Ets.insert(:fetch, "test", key1: "val1")

    assert {:ok,
            %{
              id: "test",
              key1: "val1"
            }} = Ets.fetch(:fetch, "test")

    assert :error = Ets.fetch(:fetch, "unknown")
  end

  test "delete/2" do
    :ok = Ets.insert(:delete, "test", key1: "val1")

    assert {:ok, _entity} = Ets.fetch(:delete, "test")

    assert :ok = Ets.delete(:delete, "test")

    assert :error = Ets.fetch(:delete, "test")
  end

  describe "all/1" do
    test "returns all inserted entities for given namespace" do
      :ok = Ets.insert(:all, "test1", key1: "val1")
      :ok = Ets.insert(:all, "test2", key1: "val1")

      {:ok, entity1} = Ets.fetch(:all, "test1")
      {:ok, entity2} = Ets.fetch(:all, "test2")

      assert [^entity1, ^entity2] = Ets.all(:all)
    end

    test "returns an empty list if no entities exist for given namespace" do
      assert [] = Ets.all(:unknown_namespace)
    end
  end
end
