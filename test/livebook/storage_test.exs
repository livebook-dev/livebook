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
      :ok = Storage.insert(:insert, "concurrent", attributes(0))

      writer = Task.async(fn -> keep_replacing("concurrent", 1) end)

      assert_consistent_reads("concurrent", 0, 2000)

      send(writer.pid, :stop)
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

  # Many attributes, so that replacing them takes a while
  @keys Enum.map(1..64, &:"key#{&1}")

  defp attributes(value), do: Enum.map(@keys, &{&1, value})

  defp keep_replacing(entity_id, i) do
    receive do
      :stop -> :ok
    after
      0 ->
        :ok = Storage.insert(:insert, entity_id, attributes(i))
        keep_replacing(entity_id, i + 1)
    end
  end

  # Values only increase, so all attributes must have the same value
  # and a read must never return an older value than the previous read
  defp assert_consistent_reads(_entity_id, _last_value, 0), do: :ok

  defp assert_consistent_reads(entity_id, last_value, reads) do
    assert {:ok, entity} = Storage.fetch(:insert, entity_id)
    assert [value] = entity |> Map.delete(:id) |> Map.values() |> Enum.uniq()
    assert value >= last_value

    assert {:ok, key_value} = Storage.fetch_key(:insert, entity_id, List.last(@keys))
    assert key_value >= value

    assert_consistent_reads(entity_id, key_value, reads - 1)
  end
end
