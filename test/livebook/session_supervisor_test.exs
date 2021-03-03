defmodule Livebook.SessionSupervisorTest do
  use ExUnit.Case

  alias Livebook.SessionSupervisor

  describe "create_session/0" do
    test "creates a new session process and returns its id" do
      {:ok, id} = SessionSupervisor.create_session()
      {:ok, pid} = SessionSupervisor.get_session_pid(id)

      assert has_child_with_pid?(SessionSupervisor, pid)
    end

    test "broadcasts a message" do
      Phoenix.PubSub.subscribe(Livebook.PubSub, "sessions")
      {:ok, id} = SessionSupervisor.create_session()

      assert_receive {:session_created, ^id}
    end
  end

  describe "delete_session/1" do
    test "stops the session process identified by the given id" do
      {:ok, id} = SessionSupervisor.create_session()
      {:ok, pid} = SessionSupervisor.get_session_pid(id)
      ref = Process.monitor(pid)

      SessionSupervisor.delete_session(id)

      assert_receive {:DOWN, ^ref, :process, _, _}
      refute has_child_with_pid?(SessionSupervisor, pid)
    end

    test "broadcasts a message" do
      Phoenix.PubSub.subscribe(Livebook.PubSub, "sessions")
      {:ok, id} = SessionSupervisor.create_session()

      SessionSupervisor.delete_session(id)

      assert_receive {:session_deleted, ^id}
    end
  end

  describe "get_session_ids/0" do
    test "lists ids identifying sessions running under the supervisor" do
      {:ok, id} = SessionSupervisor.create_session()

      assert id in SessionSupervisor.get_session_ids()
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

  describe "get_session_pid/1" do
    test "returns pid if a session process with the given id is running" do
      {:ok, id} = SessionSupervisor.create_session()

      assert {:ok, pid} = SessionSupervisor.get_session_pid(id)
      assert :global.whereis_name({:session, id}) == pid
    end

    test "returns an error if a session process with the given id does not exist" do
      assert {:error, :nonexistent} = SessionSupervisor.get_session_pid("nonexistent")
    end
  end

  defp has_child_with_pid?(supervisor, pid) do
    List.keymember?(DynamicSupervisor.which_children(supervisor), pid, 1)
  end
end
