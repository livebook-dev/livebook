defmodule Livebook.StorageTest do
  use ExUnit.Case, async: true

  alias Livebook.Storage

  describe "insert/3 and fetch/2" do
    test "properly inserts a new key-value attribute" do
      assert :ok = Storage.insert(:insert, "test", key1: "val1", key2: "val2")

      assert {:ok,
              %{
                id: "test",
                key1: "val1",
                key2: "val2"
              }} = Storage.fetch(:insert, "test")
    end

    test "replaces already existing attributes with new values" do
      assert :ok = Storage.insert(:insert, "replace", key1: "val1", key2: "val2")

      assert {:ok,
              %{
                key1: "val1",
                key2: "val2"
              }} = Storage.fetch(:insert, "replace")

      assert :ok =
               Storage.insert(:insert, "replace",
                 key1: "updated_val1",
                 key2: "val2",
                 key3: "val3"
               )

      assert {:ok,
              %{
                key1: "updated_val1",
                key2: "val2",
                key3: "val3"
              }} = Storage.fetch(:insert, "replace")

      assert {:ok, "updated_val1"} = Storage.fetch_key(:insert, "replace", :key1)
    end

    test "concurrent reads never observe a partially replaced entity" do
      :ok = Storage.insert(:insert, "concurrent", key1: 0, key2: 0)

      writer =
        Task.async(fn ->
          for i <- 1..1000 do
            :ok = Storage.insert(:insert, "concurrent", key1: i, key2: i)
          end
        end)

      assert_consistent_reads(writer.pid, 0)

      Task.await(writer)
    end
  end

  describe "fetch_key/3" do
    test "reads a given key" do
      :ok = Storage.insert(:fetch_key, "test", key1: "val1")
      assert Storage.fetch_key(:fetch_key, "test", :key1) == {:ok, "val1"}
      assert Storage.fetch_key(:fetch_key, "test", :key2) == :error
    end

    test "handles nil accordingly" do
      assert Storage.fetch_key(:fetch_key, "test_nil", :key1) == :error
      :ok = Storage.insert(:fetch_key, "test_nil", key1: nil)
      assert Storage.fetch_key(:fetch_key, "test_nil", :key1) == {:ok, nil}
    end
  end

  test "fetch/2" do
    :ok = Storage.insert(:fetch, "test", key1: "val1")

    assert {:ok,
            %{
              id: "test",
              key1: "val1"
            }} = Storage.fetch(:fetch, "test")

    assert :error = Storage.fetch(:fetch, "unknown")
  end

  test "delete/2" do
    :ok = Storage.insert(:delete, "test", key1: "val1")

    assert {:ok, _entity} = Storage.fetch(:delete, "test")

    assert :ok = Storage.delete(:delete, "test")

    assert :error = Storage.fetch(:delete, "test")
  end

  test "delete_key/3" do
    :ok = Storage.insert(:delete_key, "test", key1: "val1", key2: "val2")

    assert :ok = Storage.delete_key(:delete_key, "test", :key2)

    assert {:ok, "val1"} = Storage.fetch_key(:delete_key, "test", :key1)
    assert :error = Storage.fetch_key(:delete_key, "test", :key2)
  end

  describe "all/1" do
    test "returns all inserted entities for given namespace" do
      :ok = Storage.insert(:all, "test1", key1: "val1")
      :ok = Storage.insert(:all, "test2", key1: "val1")

      {:ok, entity1} = Storage.fetch(:all, "test1")
      {:ok, entity2} = Storage.fetch(:all, "test2")

      assert [^entity1, ^entity2] = Enum.sort(Storage.all(:all))
    end

    test "returns an empty list if no entities exist for given namespace" do
      assert [] = Storage.all(:unknown_namespace)
    end
  end

  # Values only increase, so a read must never return an older value
  # than the previous read
  defp assert_consistent_reads(writer_pid, last_value) do
    if Process.alive?(writer_pid) do
      assert {:ok, %{key1: value, key2: value}} = Storage.fetch(:insert, "concurrent")
      assert value >= last_value

      assert {:ok, key_value} = Storage.fetch_key(:insert, "concurrent", :key1)
      assert key_value >= value

      assert_consistent_reads(writer_pid, key_value)
    end
  end
end
