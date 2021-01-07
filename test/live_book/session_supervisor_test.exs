defmodule LiveBook.SessionSupervisorTest do
  use ExUnit.Case

  alias LiveBook.SessionSupervisor

  setup do
    on_exit(fn ->
      # Start a fresh SessionSupervisor for each test
      Supervisor.terminate_child(LiveBook.Supervisor, LiveBook.SessionSupervisor)
      Supervisor.restart_child(LiveBook.Supervisor, LiveBook.SessionSupervisor)
    end)
  end

  describe "create_session/0" do
    test "creates a new session process and returns its id" do
      {:ok, id} = SessionSupervisor.create_session()

      assert [{_, pid, _, [LiveBook.Session]}] =
               DynamicSupervisor.which_children(SessionSupervisor)

      assert pid == :global.whereis_name({:session, id})
    end

    test "broadcasts a message" do
      Phoenix.PubSub.subscribe(LiveBook.PubSub, "sessions")
      {:ok, id} = SessionSupervisor.create_session()

      assert_receive {:session_created, ^id}
    end
  end

  describe "delete_session/1" do
    test "stops the session process identified by the given id" do
      {:ok, id} = SessionSupervisor.create_session()

      SessionSupervisor.delete_session(id)

      assert [] = DynamicSupervisor.which_children(SessionSupervisor)
    end

    test "broadcasts a message" do
      Phoenix.PubSub.subscribe(LiveBook.PubSub, "sessions")
      {:ok, id} = SessionSupervisor.create_session()

      SessionSupervisor.delete_session(id)

      assert_receive {:session_deleted, ^id}
    end
  end

  describe "get_session_ids/0" do
    test "lists ids identifying sessions running under the supervisor" do
      {:ok, id} = SessionSupervisor.create_session()

      assert SessionSupervisor.get_session_ids() == [id]
    end
  end

  describe "session_exists?/1" do
    test "returns true if a session process with the given id exists" do
      {:ok, id} = SessionSupervisor.create_session()

      assert SessionSupervisor.session_exists?(id)
    end

    test "returns false if a session process with the given id does not exist" do
      refute SessionSupervisor.session_exists?("nonexistent")
    end
  end
end
