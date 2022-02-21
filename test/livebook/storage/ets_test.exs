defmodule Livebook.Storage.EtsTest do
  use ExUnit.Case, async: true

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

  describe "fetch_key/3" do
    test "reads a given key" do
      :ok = Ets.insert(:fetch_key, "test", key1: "val1")
      assert Ets.fetch_key(:fetch_key, "test", :key1) == {:ok, "val1"}
      assert Ets.fetch_key(:fetch_key, "test", :key2) == :error
    end

    test "handles nil accordingly" do
      assert Ets.fetch_key(:fetch_key, "test_nil", :key1) == :error
      :ok = Ets.insert(:fetch_key, "test_nil", key1: nil)
      assert Ets.fetch_key(:fetch_key, "test_nil", :key1) == {:ok, nil}
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

  test "delete_key/3" do
    :ok = Ets.insert(:delete_key, "test", key1: "val1", key2: "val2")

    assert :ok = Ets.delete_key(:delete_key, "test", :key2)

    assert {:ok, "val1"} = Ets.fetch_key(:delete_key, "test", :key1)
    assert :error = Ets.fetch_key(:delete_key, "test", :key2)
  end

  describe "all/1" do
    test "returns all inserted entities for given namespace" do
      :ok = Ets.insert(:all, "test1", key1: "val1")
      :ok = Ets.insert(:all, "test2", key1: "val1")

      {:ok, entity1} = Ets.fetch(:all, "test1")
      {:ok, entity2} = Ets.fetch(:all, "test2")

      assert [^entity1, ^entity2] = Enum.sort(Ets.all(:all))
    end

    test "returns an empty list if no entities exist for given namespace" do
      assert [] = Ets.all(:unknown_namespace)
    end
  end

  describe "persistence" do
    defp read_table_and_lookup(entity) do
      :ok = Ets.sync()

      {:ok, tab} =
        Ets.config_file_path()
        |> String.to_charlist()
        |> :ets.file2tab()

      :ets.lookup(tab, {:persistence, entity})
    end

    test "insert triggers saving to file" do
      :ok = Ets.insert(:persistence, "insert", key: "val")

      assert [_test] = read_table_and_lookup("insert")
    end

    test "delete triggers saving to file" do
      :ok = Ets.insert(:persistence, "delete", key: "val")
      :ok = Ets.delete(:persistence, "delete")

      assert [] = read_table_and_lookup("delete")
    end
  end
end
