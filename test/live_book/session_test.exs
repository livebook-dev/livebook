defmodule LiveBook.SessionTest do
  use ExUnit.Case, async: true

  alias LiveBook.{Session, Delta, Runtime, Utils}

  setup do
    session_id = start_session()
    %{session_id: session_id}
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

  describe "cancel_cell_evaluation/2" do
    test "sends a cancel evaluation operation to subscribers", %{session_id: session_id} do
      Phoenix.PubSub.subscribe(LiveBook.PubSub, "sessions:#{session_id}")

      {_section_id, cell_id} = insert_section_and_cell(session_id)
      queue_evaluation(session_id, cell_id)

      Session.cancel_cell_evaluation(session_id, cell_id)
      assert_receive {:operation, {:cancel_cell_evaluation, ^cell_id}}
    end
  end

  describe "set_notebook_name/2" do
    test "sends a notebook name update operation to subscribers", %{session_id: session_id} do
      Phoenix.PubSub.subscribe(LiveBook.PubSub, "sessions:#{session_id}")

      Session.set_notebook_name(session_id, "Cat's guide to life")
      assert_receive {:operation, {:set_notebook_name, "Cat's guide to life"}}
    end
  end

  describe "set_section_name/3" do
    test "sends a section name update operation to subscribers", %{session_id: session_id} do
      Phoenix.PubSub.subscribe(LiveBook.PubSub, "sessions:#{session_id}")

      {section_id, _cell_id} = insert_section_and_cell(session_id)

      Session.set_section_name(session_id, section_id, "Chapter 1")
      assert_receive {:operation, {:set_section_name, ^section_id, "Chapter 1"}}
    end
  end

  describe "apply_cell_delta/5" do
    test "sends a cell delta operation to subscribers", %{session_id: session_id} do
      Phoenix.PubSub.subscribe(LiveBook.PubSub, "sessions:#{session_id}")

      {_section_id, cell_id} = insert_section_and_cell(session_id)

      from = self()
      delta = Delta.new() |> Delta.insert("cats")
      revision = 1

      Session.apply_cell_delta(session_id, from, cell_id, delta, revision)
      assert_receive {:operation, {:apply_cell_delta, ^from, ^cell_id, ^delta, ^revision}}
    end
  end

  describe "connect_runtime/2" do
    test "sends a runtime update operation to subscribers", %{session_id: session_id} do
      Phoenix.PubSub.subscribe(LiveBook.PubSub, "sessions:#{session_id}")

      {:ok, runtime} = LiveBookTest.Runtime.SingleEvaluator.init()
      Session.connect_runtime(session_id, runtime)

      assert_receive {:operation, {:set_runtime, ^runtime}}
    end
  end

  describe "disconnect_runtime/1" do
    test "sends a runtime update operation to subscribers", %{session_id: session_id} do
      Phoenix.PubSub.subscribe(LiveBook.PubSub, "sessions:#{session_id}")

      Session.disconnect_runtime(session_id)

      assert_receive {:operation, {:set_runtime, nil}}
    end
  end

  describe "set_path/1" do
    @tag :tmp_dir
    test "sends a path update operation to subscribers",
         %{session_id: session_id, tmp_dir: tmp_dir} do
      Phoenix.PubSub.subscribe(LiveBook.PubSub, "sessions:#{session_id}")

      path = Path.join(tmp_dir, "notebook.livemd")
      Session.set_path(session_id, path)

      assert_receive {:operation, {:set_path, ^path}}
    end

    @tag :tmp_dir
    test "broadcasts an error if the path is already in use",
         %{session_id: session_id, tmp_dir: tmp_dir} do
      path = Path.join(tmp_dir, "notebook.livemd")
      start_session(path: path)

      Phoenix.PubSub.subscribe(LiveBook.PubSub, "sessions:#{session_id}")

      Session.set_path(session_id, path)

      assert_receive {:error, "failed to set new path because it is already in use"}
    end
  end

  describe "save/1" do
    @tag :tmp_dir
    test "persists the notebook to the associated file and notifies subscribers",
         %{session_id: session_id, tmp_dir: tmp_dir} do
      path = Path.join(tmp_dir, "notebook.livemd")
      Session.set_path(session_id, path)
      # Perform a change, so the notebook is dirty
      Session.set_notebook_name(session_id, "My notebook")

      Phoenix.PubSub.subscribe(LiveBook.PubSub, "sessions:#{session_id}")

      refute File.exists?(path)

      Session.save(session_id)

      assert_receive {:operation, :mark_as_not_dirty}
      assert File.exists?(path)
      assert File.read!(path) =~ "My notebook"
    end
  end

  describe "close/1" do
    @tag :tmp_dir
    test "saves the notebook and notifies subscribers once the session is closed",
         %{session_id: session_id, tmp_dir: tmp_dir} do
      path = Path.join(tmp_dir, "notebook.livemd")
      Session.set_path(session_id, path)
      # Perform a change, so the notebook is dirty
      Session.set_notebook_name(session_id, "My notebook")

      Phoenix.PubSub.subscribe(LiveBook.PubSub, "sessions:#{session_id}")

      refute File.exists?(path)

      Process.flag(:trap_exit, true)
      Session.close(session_id)

      assert_receive :session_closed
      assert File.exists?(path)
      assert File.read!(path) =~ "My notebook"
    end
  end

  describe "start_link/1" do
    @tag :tmp_dir
    test "fails if the given path is already in use", %{tmp_dir: tmp_dir} do
      path = Path.join(tmp_dir, "notebook.livemd")
      start_session(path: path)

      assert {:error, "the given path is already in use"} ==
               Session.start_link(id: Utils.random_id(), path: path)
    end
  end

  # For most tests we use the lightweight runtime, so that they are cheap to run.
  # Here go several integration tests that actually start a separate runtime
  # to verify session integrates well with it.

  test "starts a standalone runtime upon first evaluation if there was none set explicitly" do
    session_id = Utils.random_id()
    {:ok, _} = Session.start_link(id: session_id)

    Phoenix.PubSub.subscribe(LiveBook.PubSub, "sessions:#{session_id}")

    {_section_id, cell_id} = insert_section_and_cell(session_id)

    Session.queue_cell_evaluation(session_id, cell_id)
    # Give it a bit more time as this involves starting a system process.
    assert_receive {:operation, {:add_cell_evaluation_response, ^cell_id, _}}, 1000
  end

  test "if the runtime node goes down, notifies the subscribers" do
    session_id = Utils.random_id()
    {:ok, _} = Session.start_link(id: session_id)
    {:ok, runtime} = Runtime.ElixirStandalone.init()

    Phoenix.PubSub.subscribe(LiveBook.PubSub, "sessions:#{session_id}")

    # Wait for the runtime to be set
    Session.connect_runtime(session_id, runtime)
    assert_receive {:operation, {:set_runtime, ^runtime}}

    # Terminate the other node, the session should detect that
    Node.spawn(runtime.node, System, :halt, [])

    assert_receive {:operation, {:set_runtime, nil}}
    assert_receive {:info, "runtime node terminated unexpectedly"}
  end

  defp start_session(opts \\ []) do
    session_id = Utils.random_id()
    {:ok, _} = Session.start_link(Keyword.merge(opts, id: session_id))
    # By default, use the current node for evaluation,
    # rather than starting a standalone one.
    {:ok, runtime} = LiveBookTest.Runtime.SingleEvaluator.init()
    Session.connect_runtime(session_id, runtime)
    session_id
  end

  defp insert_section_and_cell(session_id) do
    Session.insert_section(session_id, 0)
    assert_receive {:operation, {:insert_section, 0, section_id}}
    Session.insert_cell(session_id, section_id, 0, :elixir)
    assert_receive {:operation, {:insert_cell, ^section_id, 0, :elixir, cell_id}}

    {section_id, cell_id}
  end

  defp queue_evaluation(session_id, cell_id) do
    Session.queue_cell_evaluation(session_id, cell_id)
    assert_receive {:operation, {:add_cell_evaluation_response, ^cell_id, _}}
  end
end
