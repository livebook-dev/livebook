defmodule LiveBook.SessionTest do
  use ExUnit.Case, async: true

  alias LiveBook.Session

  setup do
    {:ok, _} = Session.start_link("1")
    %{session_id: "1"}
  end

  describe "insert_section/2" do
    test "sends an insert opreation to subscribers", %{session_id: session_id} do
      Phoenix.PubSub.subscribe(LiveBook.PubSub, "sessions:#{session_id}")

      Session.insert_section(session_id, 0)
      assert_receive {:operation, {:insert_section, 0, _id}}
    end
  end

  describe "insert_cell/4" do
    test "sends an insert opreation to subscribers", %{session_id: session_id} do
      Phoenix.PubSub.subscribe(LiveBook.PubSub, "sessions:#{session_id}")

      Session.insert_section(session_id, 0)
      assert_receive {:operation, {:insert_section, 0, section_id}}

      Session.insert_cell(session_id, section_id, 0, :elixir)
      assert_receive {:operation, {:insert_cell, ^section_id, 0, :elixir, _id}}
    end
  end

  describe "delete_section/2" do
    test "sends a delete opreation to subscribers", %{session_id: session_id} do
      Phoenix.PubSub.subscribe(LiveBook.PubSub, "sessions:#{session_id}")

      {section_id, _cell_id} = insert_section_and_cell(session_id)

      Session.delete_section(session_id, section_id)
      assert_receive {:operation, {:delete_section, ^section_id}}
    end
  end

  describe "delete_cell/2" do
    test "sends a delete opreation to subscribers", %{session_id: session_id} do
      Phoenix.PubSub.subscribe(LiveBook.PubSub, "sessions:#{session_id}")

      {_section_id, cell_id} = insert_section_and_cell(session_id)

      Session.delete_cell(session_id, cell_id)
      assert_receive {:operation, {:delete_cell, ^cell_id}}
    end
  end

  describe "queue_cell_evaluation/2" do
    test "sends a queue evaluation operation to subscribers", %{session_id: session_id} do
      Phoenix.PubSub.subscribe(LiveBook.PubSub, "sessions:#{session_id}")

      {_section_id, cell_id} = insert_section_and_cell(session_id)

      Session.queue_cell_evaluation(session_id, cell_id)
      assert_receive {:operation, {:queue_cell_evaluation, ^cell_id}}
    end

    test "triggers evaluation and sends update operation once it finishes",
         %{session_id: session_id} do
      Phoenix.PubSub.subscribe(LiveBook.PubSub, "sessions:#{session_id}")

      {_section_id, cell_id} = insert_section_and_cell(session_id)

      Session.queue_cell_evaluation(session_id, cell_id)
      assert_receive {:operation, {:add_cell_evaluation_response, ^cell_id, _}}
    end
  end

  defp insert_section_and_cell(session_id) do
    Session.insert_section(session_id, 0)
    assert_receive {:operation, {:insert_section, 0, section_id}}
    Session.insert_cell(session_id, section_id, 0, :elixir)
    assert_receive {:operation, {:insert_cell, ^section_id, 0, :elixir, cell_id}}

    {section_id, cell_id}
  end
end
