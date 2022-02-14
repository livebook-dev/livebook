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
    defp load_state(%{tmp_dir: tmp_dir, secret: secret, state: state}) do
      %{state | secret: secret, file_path: Path.join([tmp_dir, "config.txt"])}
    end

    @table_name __MODULE__
    setup do
      # intentionally don't pass the 'file_path' nor 'secret' into init/1
      # to avoid loading the file on initialization
      {:ok, state} = Ets.init(table_name: @table_name)

      [state: state, secret: :crypto.strong_rand_bytes(32)]
    end

    @tag :tmp_dir
    test "saves/loads the ets content to/from an encrypted file", ctx do
      state = load_state(ctx)

      {:reply, :ok, state} = Ets.handle_call({:insert, :all, "test1", key: "val"}, {}, state)
      {:reply, :ok, state} = Ets.handle_call({:insert, :all, "test2", key: "val"}, {}, state)

      assert {:noreply, _state} = Ets.handle_info(:persist, state)

      assert File.exists?(state.file_path)

      # delete the ets table and reinitialize the ETS module
      assert [_, _] = entries = :ets.tab2list(@table_name)
      :ets.delete(@table_name)
      assert :undefined == :ets.info(@table_name)

      # make the Ets load the entries on init
      {:ok, _state} =
        Ets.init(table_name: @table_name, secret: state.secret, file_path: state.file_path)

      assert ^entries = :ets.tab2list(@table_name)
    end
  end
end
