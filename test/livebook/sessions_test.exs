defmodule Livebook.SessionsTest do
  use ExUnit.Case, async: true

  alias Livebook.{Sessions, Session}

  describe "create_session/0" do
    test "starts a new session process under the sessions supervisor" do
      {:ok, session} = Sessions.create_session()
      assert has_child_with_pid?(Livebook.SessionSupervisor, session.pid)

      Session.close(session.pid)
    end

    test "broadcasts a message to subscribers" do
      Sessions.subscribe()
      {:ok, %{id: id} = session} = Sessions.create_session()
      assert_receive {:session_created, %{id: ^id}}

      Session.close(session.pid)
    end
  end

  describe "list_sessions/0" do
    test "lists all sessions" do
      {:ok, session} = Sessions.create_session()
      assert session in Sessions.list_sessions()

      Session.close(session.pid)
    end
  end

  describe "fetch_session/1" do
    test "returns an error if no session with the given id exists" do
      id = Livebook.Utils.random_node_aware_id()
      assert Sessions.fetch_session(id) == {:error, :not_found}
    end

    test "returns session matching the given id" do
      {:ok, session} = Sessions.create_session()
      assert {:ok, ^session} = Sessions.fetch_session(session.id)

      Session.close(session.pid)
    end

    test "returns an error if session comes from a different boot" do
      Application.put_env(:livebook, :random_boot_id, "aaa")
      id = Livebook.Utils.random_node_aware_id()

      assert Sessions.fetch_session(id) == {:error, :not_found}
      Application.put_env(:livebook, :random_boot_id, "bbb")
      assert Sessions.fetch_session(id) == {:error, :different_boot_id}
    end
  end

  describe "update_session/1" do
    test "broadcasts a message to subscribers" do
      {:ok, %{id: id} = session} = Sessions.create_session()
      Sessions.subscribe()
      updated_session = %{session | notebook_name: "New name"}
      Livebook.Sessions.update_session(updated_session)
      assert_receive {:session_updated, %{id: ^id, notebook_name: "New name"}}

      Session.close(session.pid)
    end
  end

  defp has_child_with_pid?(supervisor, pid) do
    List.keymember?(DynamicSupervisor.which_children(supervisor), pid, 1)
  end
end
