defmodule Livebook.SessionHelpers do
  @moduledoc false

  alias Livebook.{Session, Sessions}

  import ExUnit.Assertions
  import Phoenix.LiveViewTest

  def wait_for_session_update(session_pid) do
    # This call is synchronous, so it gives the session time
    # for handling the previously sent change messages.
    Session.get_data(session_pid)
    :ok
  end

  def evaluate_setup(session_pid) do
    Session.queue_cell_evaluation(session_pid, "setup")
    assert_receive {:operation, {:add_cell_evaluation_response, _, "setup", _, _}}
  end

  def insert_cell_with_output(session_pid, section_id, output) do
    code =
      quote do
        send(
          Process.group_leader(),
          {:io_request, self(), make_ref(), {:livebook_put_output, unquote(Macro.escape(output))}}
        )
      end
      |> Macro.to_string()

    cell_id = insert_text_cell(session_pid, section_id, :code, code)
    Session.queue_cell_evaluation(session_pid, cell_id)
    assert_receive {:operation, {:add_cell_evaluation_response, _, ^cell_id, _, _}}
    cell_id
  end

  def bypass_url(port), do: "http://localhost:#{port}"

  def close_session_by_id(session_id) do
    {:ok, session} = Sessions.fetch_session(session_id)
    Session.close(session.pid)
  end

  def insert_section(session_pid) do
    Session.insert_section(session_pid, 0)
    %{notebook: %{sections: [section]}} = Session.get_data(session_pid)
    section.id
  end

  def insert_text_cell(session_pid, section_id, type, content \\ " ") do
    Session.insert_cell(session_pid, section_id, 0, type, %{source: content})
    %{notebook: %{sections: [%{cells: [cell]}]}} = Session.get_data(session_pid)
    cell.id
  end

  def assert_session_secret(view, session_pid, secret) do
    selector =
      case secret do
        %{name: name, origin: :session} -> "#session-secret-#{name}-title"
        %{name: name, origin: :app} -> "#app-secret-#{name}-title"
        %{name: name, origin: {:hub, id}} -> "#hub-#{id}-secret-#{name}-title"
      end

    assert has_element?(view, selector)
    secrets = Session.get_data(session_pid).secrets

    assert Map.has_key?(secrets, secret.name)
    assert secrets[secret.name] == secret.value
  end
end
