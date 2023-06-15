defmodule Livebook.SessionHelpers do
  @moduledoc false

  alias Livebook.{Hubs, Session, Sessions}
  alias Livebook.Secrets.Secret

  import ExUnit.Assertions
  import Phoenix.LiveViewTest
  import Livebook.TestHelpers

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
    code = source_for_output(output)
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
    %{notebook: %{sections: [section | _]}} = Session.get_data(session_pid)
    section.id
  end

  def insert_text_cell(session_pid, section_id, type, content \\ " ") do
    Session.insert_cell(session_pid, section_id, 0, type, %{source: content})
    data = Session.get_data(session_pid)
    {:ok, section} = Livebook.Notebook.fetch_section(data.notebook, section_id)
    cell = hd(section.cells)
    cell.id
  end

  def assert_session_secret(view, session_pid, secret, key \\ :secrets) do
    selector =
      case secret do
        %{name: name, hub_id: nil} -> "#session-secret-#{name}"
        %{name: name, hub_id: id} -> "#hub-#{id}-secret-#{name}"
      end

    session_data = Session.get_data(session_pid)

    secrets =
      case Map.fetch!(session_data, key) do
        secrets when is_map(secrets) -> Map.values(secrets)
        secrets -> secrets
      end

    assert has_element?(view, selector)
    assert secret in secrets
  end

  def hub_label(%Secret{hub_id: id}), do: hub_label(Hubs.fetch_hub!(id))
  def hub_label(hub), do: "#{hub.hub_emoji} #{hub.hub_name}"
end
