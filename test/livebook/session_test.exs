defmodule Livebook.SessionTest do
  use ExUnit.Case, async: true

  alias Livebook.{Session, Delta, Runtime, Utils, Notebook, FileSystem}
  alias Livebook.Notebook.{Section, Cell}

  setup do
    session = start_session()
    %{session: session}
  end

  describe "set_notebook_attributes/2" do
    test "sends an attributes update to subscribers", %{session: session} do
      Phoenix.PubSub.subscribe(Livebook.PubSub, "sessions:#{session.id}")
      pid = self()

      attrs = %{set_notebook_attributes: true}
      Session.set_notebook_attributes(session.pid, attrs)
      assert_receive {:operation, {:set_notebook_attributes, ^pid, ^attrs}}
    end
  end

  describe "insert_section/2" do
    test "sends an insert opreation to subscribers", %{session: session} do
      Phoenix.PubSub.subscribe(Livebook.PubSub, "sessions:#{session.id}")
      pid = self()

      Session.insert_section(session.pid, 0)
      assert_receive {:operation, {:insert_section, ^pid, 0, _id}}
    end
  end

  describe "insert_cell/4" do
    test "sends an insert opreation to subscribers", %{session: session} do
      Phoenix.PubSub.subscribe(Livebook.PubSub, "sessions:#{session.id}")
      pid = self()

      Session.insert_section(session.pid, 0)
      assert_receive {:operation, {:insert_section, ^pid, 0, section_id}}

      Session.insert_cell(session.pid, section_id, 0, :elixir)
      assert_receive {:operation, {:insert_cell, ^pid, ^section_id, 0, :elixir, _id}}
    end
  end

  describe "delete_section/3" do
    test "sends a delete opreation to subscribers", %{session: session} do
      Phoenix.PubSub.subscribe(Livebook.PubSub, "sessions:#{session.id}")
      pid = self()

      {section_id, _cell_id} = insert_section_and_cell(session.pid)

      Session.delete_section(session.pid, section_id, false)
      assert_receive {:operation, {:delete_section, ^pid, ^section_id, false}}
    end
  end

  describe "delete_cell/2" do
    test "sends a delete opreation to subscribers", %{session: session} do
      Phoenix.PubSub.subscribe(Livebook.PubSub, "sessions:#{session.id}")
      pid = self()

      {_section_id, cell_id} = insert_section_and_cell(session.pid)

      Session.delete_cell(session.pid, cell_id)
      assert_receive {:operation, {:delete_cell, ^pid, ^cell_id}}
    end
  end

  describe "restore_cell/2" do
    test "sends a restore opreation to subscribers", %{session: session} do
      Phoenix.PubSub.subscribe(Livebook.PubSub, "sessions:#{session.id}")
      pid = self()

      {_section_id, cell_id} = insert_section_and_cell(session.pid)
      Session.delete_cell(session.pid, cell_id)

      Session.restore_cell(session.pid, cell_id)
      assert_receive {:operation, {:restore_cell, ^pid, ^cell_id}}
    end
  end

  describe "queue_cell_evaluation/2" do
    test "triggers evaluation and sends update operation once it finishes",
         %{session: session} do
      Phoenix.PubSub.subscribe(Livebook.PubSub, "sessions:#{session.id}")
      pid = self()

      {_section_id, cell_id} = insert_section_and_cell(session.pid)

      Session.queue_cell_evaluation(session.pid, cell_id)

      assert_receive {:operation, {:queue_cells_evaluation, ^pid, [^cell_id]}}

      assert_receive {:operation,
                      {:add_cell_evaluation_response, _, ^cell_id, _,
                       %{evaluation_time_ms: _time_ms}}}
    end
  end

  describe "cancel_cell_evaluation/2" do
    test "sends a cancel evaluation operation to subscribers", %{session: session} do
      Phoenix.PubSub.subscribe(Livebook.PubSub, "sessions:#{session.id}")
      pid = self()

      {_section_id, cell_id} = insert_section_and_cell(session.pid)
      Session.queue_cell_evaluation(session.pid, cell_id)

      Session.cancel_cell_evaluation(session.pid, cell_id)

      assert_receive {:operation, {:cancel_cell_evaluation, ^pid, ^cell_id}}
    end
  end

  describe "set_notebook_name/2" do
    test "sends a notebook name update operation to subscribers", %{session: session} do
      Phoenix.PubSub.subscribe(Livebook.PubSub, "sessions:#{session.id}")
      pid = self()

      Session.set_notebook_name(session.pid, "Cat's guide to life")
      assert_receive {:operation, {:set_notebook_name, ^pid, "Cat's guide to life"}}
    end
  end

  describe "set_section_name/3" do
    test "sends a section name update operation to subscribers", %{session: session} do
      Phoenix.PubSub.subscribe(Livebook.PubSub, "sessions:#{session.id}")
      pid = self()

      {section_id, _cell_id} = insert_section_and_cell(session.pid)

      Session.set_section_name(session.pid, section_id, "Chapter 1")
      assert_receive {:operation, {:set_section_name, ^pid, ^section_id, "Chapter 1"}}
    end
  end

  describe "apply_cell_delta/4" do
    test "sends a cell delta operation to subscribers", %{session: session} do
      Phoenix.PubSub.subscribe(Livebook.PubSub, "sessions:#{session.id}")
      pid = self()

      {_section_id, cell_id} = insert_section_and_cell(session.pid)

      delta = Delta.new() |> Delta.insert("cats")
      revision = 1

      Session.apply_cell_delta(session.pid, cell_id, delta, revision)
      assert_receive {:operation, {:apply_cell_delta, ^pid, ^cell_id, ^delta, ^revision}}
    end
  end

  describe "report_cell_revision/3" do
    test "sends a revision report operation to subscribers", %{session: session} do
      Phoenix.PubSub.subscribe(Livebook.PubSub, "sessions:#{session.id}")
      pid = self()

      {_section_id, cell_id} = insert_section_and_cell(session.pid)
      revision = 1

      Session.report_cell_revision(session.pid, cell_id, revision)
      assert_receive {:operation, {:report_cell_revision, ^pid, ^cell_id, ^revision}}
    end
  end

  describe "set_cell_attributes/3" do
    test "sends an attributes update operation to subscribers", %{session: session} do
      Phoenix.PubSub.subscribe(Livebook.PubSub, "sessions:#{session.id}")
      pid = self()

      {_section_id, cell_id} = insert_section_and_cell(session.pid)
      attrs = %{disable_formatting: true}

      Session.set_cell_attributes(session.pid, cell_id, attrs)
      assert_receive {:operation, {:set_cell_attributes, ^pid, ^cell_id, ^attrs}}
    end
  end

  describe "connect_runtime/2" do
    test "sends a runtime update operation to subscribers", %{session: session} do
      Phoenix.PubSub.subscribe(Livebook.PubSub, "sessions:#{session.id}")
      pid = self()

      {:ok, runtime} = Livebook.Runtime.Embedded.init()
      Session.connect_runtime(session.pid, runtime)

      assert_receive {:operation, {:set_runtime, ^pid, ^runtime}}
    end
  end

  describe "disconnect_runtime/1" do
    test "sends a runtime update operation to subscribers", %{session: session} do
      Phoenix.PubSub.subscribe(Livebook.PubSub, "sessions:#{session.id}")
      pid = self()

      {:ok, runtime} = Livebook.Runtime.Embedded.init()
      Session.connect_runtime(session.pid, runtime)

      # Calling twice can happen in a race, make sure it doesn't crash
      Session.disconnect_runtime(session.pid)
      Session.disconnect_runtime([session.pid])

      assert_receive {:operation, {:set_runtime, ^pid, nil}}
    end
  end

  describe "set_file/1" do
    @tag :tmp_dir
    test "sends a file update operation to subscribers",
         %{session: session, tmp_dir: tmp_dir} do
      Phoenix.PubSub.subscribe(Livebook.PubSub, "sessions:#{session.id}")
      pid = self()

      tmp_dir = FileSystem.File.local(tmp_dir <> "/")
      file = FileSystem.File.resolve(tmp_dir, "notebook.livemd")
      Session.set_file(session.pid, file)

      assert_receive {:operation, {:set_file, ^pid, ^file}}
    end

    @tag :tmp_dir
    test "broadcasts an error if the path is already in use",
         %{session: session, tmp_dir: tmp_dir} do
      tmp_dir = FileSystem.File.local(tmp_dir <> "/")
      file = FileSystem.File.resolve(tmp_dir, "notebook.livemd")
      start_session(file: file)

      Phoenix.PubSub.subscribe(Livebook.PubSub, "sessions:#{session.id}")

      Session.set_file(session.pid, file)

      assert_receive {:error, "failed to set new file because it is already in use"}
    end

    @tag :tmp_dir
    test "moves images to the new directory", %{session: session, tmp_dir: tmp_dir} do
      tmp_dir = FileSystem.File.local(tmp_dir <> "/")
      %{images_dir: images_dir} = session

      image_file = FileSystem.File.resolve(images_dir, "test.jpg")
      :ok = FileSystem.File.write(image_file, "")

      file = FileSystem.File.resolve(tmp_dir, "notebook.livemd")
      Session.set_file(session.pid, file)

      # Wait for the session to deal with the files
      Process.sleep(100)

      assert {:ok, true} =
               FileSystem.File.exists?(FileSystem.File.resolve(tmp_dir, "images/test.jpg"))

      assert {:ok, false} = FileSystem.File.exists?(images_dir)
    end

    @tag :tmp_dir
    test "does not remove images from the previous dir if not temporary",
         %{session: session, tmp_dir: tmp_dir} do
      tmp_dir = FileSystem.File.local(tmp_dir <> "/")
      file = FileSystem.File.resolve(tmp_dir, "notebook.livemd")
      Session.set_file(session.pid, file)

      %{images_dir: images_dir} = session
      image_file = FileSystem.File.resolve(images_dir, "test.jpg")
      :ok = FileSystem.File.write(image_file, "")

      Session.set_file(session.pid, nil)

      # Wait for the session to deal with the files
      Process.sleep(200)

      assert {:ok, true} = FileSystem.File.exists?(image_file)

      %{images_dir: new_images_dir} = session

      assert {:ok, true} =
               FileSystem.File.exists?(FileSystem.File.resolve(new_images_dir, "test.jpg"))
    end
  end

  describe "save/1" do
    @tag :tmp_dir
    test "persists the notebook to the associated file and notifies subscribers",
         %{session: session, tmp_dir: tmp_dir} do
      tmp_dir = FileSystem.File.local(tmp_dir <> "/")
      file = FileSystem.File.resolve(tmp_dir, "notebook.livemd")
      Session.set_file(session.pid, file)
      # Perform a change, so the notebook is dirty
      Session.set_notebook_name(session.pid, "My notebook")

      Phoenix.PubSub.subscribe(Livebook.PubSub, "sessions:#{session.id}")

      assert {:ok, false} = FileSystem.File.exists?(file)

      Session.save(session.pid)

      assert_receive {:operation, {:mark_as_not_dirty, _}}
      assert {:ok, "# My notebook\n" <> _rest} = FileSystem.File.read(file)
    end

    @tag :tmp_dir
    test "creates nonexistent directories", %{session: session, tmp_dir: tmp_dir} do
      tmp_dir = FileSystem.File.local(tmp_dir <> "/")
      file = FileSystem.File.resolve(tmp_dir, "nonexistent/dir/notebook.livemd")
      Session.set_file(session.pid, file)
      # Perform a change, so the notebook is dirty
      Session.set_notebook_name(session.pid, "My notebook")

      Phoenix.PubSub.subscribe(Livebook.PubSub, "sessions:#{session.id}")

      assert {:ok, false} = FileSystem.File.exists?(file)

      Session.save(session.pid)

      assert_receive {:operation, {:mark_as_not_dirty, _}}
      assert {:ok, "# My notebook\n" <> _rest} = FileSystem.File.read(file)
    end
  end

  describe "close/1" do
    @tag :tmp_dir
    test "saves the notebook and notifies subscribers once the session is closed",
         %{session: session, tmp_dir: tmp_dir} do
      tmp_dir = FileSystem.File.local(tmp_dir <> "/")
      file = FileSystem.File.resolve(tmp_dir, "notebook.livemd")
      Session.set_file(session.pid, file)
      # Perform a change, so the notebook is dirty
      Session.set_notebook_name(session.pid, "My notebook")

      Phoenix.PubSub.subscribe(Livebook.PubSub, "sessions:#{session.id}")

      assert {:ok, false} = FileSystem.File.exists?(file)

      Process.flag(:trap_exit, true)

      # Calling twice can happen in a race, make sure it doesn't crash
      Session.close(session.pid)
      Session.close([session.pid])

      assert_receive :session_closed
      assert {:ok, "# My notebook\n" <> _rest} = FileSystem.File.read(file)
    end

    test "clears session temporary directory", %{session: session} do
      %{images_dir: images_dir} = session
      :ok = FileSystem.File.create_dir(images_dir)

      assert {:ok, true} = FileSystem.File.exists?(images_dir)

      Process.flag(:trap_exit, true)
      Session.close(session.pid)

      # Wait for the session to deal with the files
      Process.sleep(50)

      assert {:ok, false} = FileSystem.File.exists?(images_dir)
    end
  end

  describe "start_link/1" do
    @tag :tmp_dir
    test "fails if the given path is already in use", %{tmp_dir: tmp_dir} do
      tmp_dir = FileSystem.File.local(tmp_dir <> "/")
      file = FileSystem.File.resolve(tmp_dir, "notebook.livemd")
      start_session(file: file)

      assert {:error, "the given file is already in use"} ==
               Session.start_link(id: Utils.random_id(), file: file)
    end

    @tag :tmp_dir
    test "copies images when :copy_images_from option is specified", %{tmp_dir: tmp_dir} do
      tmp_dir = FileSystem.File.local(tmp_dir <> "/")

      image_file = FileSystem.File.resolve(tmp_dir, "image.jpg")
      :ok = FileSystem.File.write(image_file, "")

      session = start_session(copy_images_from: tmp_dir)
      %{images_dir: images_dir} = session

      assert {:ok, true} =
               FileSystem.File.exists?(FileSystem.File.resolve(images_dir, "image.jpg"))
    end

    test "saves images when :images option is specified" do
      images = %{"image.jpg" => "binary content"}

      session = start_session(images: images)
      %{images_dir: images_dir} = session

      assert FileSystem.File.resolve(images_dir, "image.jpg") |> FileSystem.File.read() ==
               {:ok, "binary content"}
    end
  end

  # For most tests we use the lightweight embedded runtime,
  # so that they are cheap to run. Here go several integration
  # tests that actually start a Elixir standalone runtime (default in production)
  # to verify session integrates well with it properly.

  test "starts a standalone runtime upon first evaluation if there was none set explicitly" do
    session = start_session()

    Phoenix.PubSub.subscribe(Livebook.PubSub, "sessions:#{session.id}")

    {_section_id, cell_id} = insert_section_and_cell(session.pid)

    Session.queue_cell_evaluation(session.pid, cell_id)
    # Give it a bit more time as this involves starting a system process.
    assert_receive {:operation,
                    {:add_cell_evaluation_response, _, ^cell_id, _,
                     %{evaluation_time_ms: _time_ms}}}
  end

  test "if the runtime node goes down, notifies the subscribers" do
    session = start_session()
    {:ok, runtime} = Runtime.ElixirStandalone.init()

    Phoenix.PubSub.subscribe(Livebook.PubSub, "sessions:#{session.id}")

    # Wait for the runtime to be set
    Session.connect_runtime(session.pid, runtime)
    assert_receive {:operation, {:set_runtime, _, ^runtime}}

    # Terminate the other node, the session should detect that
    Node.spawn(runtime.node, System, :halt, [])

    assert_receive {:operation, {:set_runtime, _, nil}}
    assert_receive {:info, "runtime node terminated unexpectedly"}
  end

  test "on user change sends an update operation subscribers", %{session: session} do
    user = Livebook.Users.User.new()
    Session.register_client(session.pid, self(), user)

    Phoenix.PubSub.subscribe(Livebook.PubSub, "sessions:#{session.id}")

    updated_user = %{user | name: "Jake Peralta"}
    Livebook.Users.broadcast_change(updated_user)

    assert_receive {:operation, {:update_user, _pid, ^updated_user}}
  end

  # Integration tests concerning input communication
  # between runtime and session

  @livebook_put_input_code """
  input = %{id: "input1", type: :number, label: "Name", default: "hey"}

  send(
    Process.group_leader(),
    {:io_request, self(), make_ref(), {:livebook_put_output, {:input, input}}}
  )
  """

  @livebook_get_input_value_code """
  ref = make_ref()
  send(Process.group_leader(), {:io_request, self(), ref, {:livebook_get_input_value, "input1"}})

  receive do
    {:io_reply, ^ref, reply} -> reply
  end
  """

  describe "user input" do
    test "replies to runtime input request" do
      input_elixir_cell = %{Notebook.Cell.new(:elixir) | source: @livebook_put_input_code}

      elixir_cell = %{Notebook.Cell.new(:elixir) | source: @livebook_get_input_value_code}

      notebook = %{
        Notebook.new()
        | sections: [
            %{Notebook.Section.new() | cells: [input_elixir_cell, elixir_cell]}
          ]
      }

      session = start_session(notebook: notebook)

      cell_id = elixir_cell.id

      Phoenix.PubSub.subscribe(Livebook.PubSub, "sessions:#{session.id}")
      Session.queue_cell_evaluation(session.pid, cell_id)

      assert_receive {:operation,
                      {:add_cell_evaluation_response, _, ^cell_id, {:text, text_output},
                       %{evaluation_time_ms: _time_ms}}}

      assert text_output =~ "hey"
    end

    test "replies with error when no matching input is found" do
      elixir_cell = %{Notebook.Cell.new(:elixir) | source: @livebook_get_input_value_code}

      notebook = %{
        Notebook.new()
        | sections: [
            %{Notebook.Section.new() | cells: [elixir_cell]}
          ]
      }

      session = start_session(notebook: notebook)

      cell_id = elixir_cell.id

      Phoenix.PubSub.subscribe(Livebook.PubSub, "sessions:#{session.id}")
      Session.queue_cell_evaluation(session.pid, cell_id)

      assert_receive {:operation,
                      {:add_cell_evaluation_response, _, ^cell_id, {:text, text_output},
                       %{evaluation_time_ms: _time_ms}}}

      assert text_output =~ ":error"
    end
  end

  describe "find_prev_locator/3" do
    test "given cell in main flow returns previous Elixir cell" do
      cell1 = %{Cell.new(:elixir) | id: "c1"}
      cell2 = %{Cell.new(:markdown) | id: "c2"}
      section1 = %{Section.new() | id: "s1", cells: [cell1, cell2]}

      cell3 = %{Cell.new(:elixir) | id: "c3"}
      section2 = %{Section.new() | id: "s2", cells: [cell3]}

      notebook = %{Notebook.new() | sections: [section1, section2]}

      assert {:main_flow, "c1"} = Session.find_prev_locator(notebook, cell3, section2)
    end

    test "given cell in branching section returns previous Elixir cell in that section" do
      section1 = %{Section.new() | id: "s1"}

      cell1 = %{Cell.new(:elixir) | id: "c1"}
      cell2 = %{Cell.new(:markdown) | id: "c2"}
      cell3 = %{Cell.new(:elixir) | id: "c3"}

      section2 = %{
        Section.new()
        | id: "s2",
          parent_id: "s1",
          cells: [cell1, cell2, cell3]
      }

      notebook = %{Notebook.new() | sections: [section1, section2]}

      assert {"s2", "c1"} = Session.find_prev_locator(notebook, cell3, section2)
    end

    test "given cell in main flow returns nil if there is no previous cell" do
      cell1 = %{Cell.new(:markdown) | id: "c1"}
      section1 = %{Section.new() | id: "s1", cells: [cell1]}

      cell2 = %{Cell.new(:elixir) | id: "c2"}
      section2 = %{Section.new() | id: "s2", cells: [cell2]}

      notebook = %{Notebook.new() | sections: [section1, section2]}

      assert {:main_flow, nil} = Session.find_prev_locator(notebook, cell2, section2)
    end

    test "given cell in branching section returns nil in that section if there is no previous cell" do
      cell1 = %{Cell.new(:markdown) | id: "c1"}
      section1 = %{Section.new() | id: "s1", cells: [cell1]}

      cell2 = %{Cell.new(:elixir) | id: "c2"}

      section2 = %{
        Section.new()
        | id: "s2",
          parent_id: "s1",
          cells: [cell2]
      }

      notebook = %{Notebook.new() | sections: [section1, section2]}

      assert {"s2", nil} = Session.find_prev_locator(notebook, cell2, section2)
    end
  end

  test "session has created_at attribute when it is created", %{session: session} do
    assert Map.has_key?(session, :created_at)
  end

  test "session created_at attribute is a date time", %{session: session} do
    assert %DateTime{} = session.created_at
  end

  test "session created_at is before now", %{session: session} do
    assert DateTime.compare(session.created_at, DateTime.utc_now()) == :lt
  end

  @tag :tmp_dir
  test "session without a file is persisted to autosave path", %{tmp_dir: tmp_dir} do
    session = start_session(autosave_path: tmp_dir)

    notebook_glob = Path.join(tmp_dir, "**/*.livemd")

    Phoenix.PubSub.subscribe(Livebook.PubSub, "sessions:#{session.id}")

    Session.save(session.pid)
    assert_receive {:operation, {:mark_as_not_dirty, _}}

    assert [notebook_path] = Path.wildcard(notebook_glob)
    assert Path.basename(notebook_path) =~ "untitled_notebook"

    # After the name is changed we should save to a different file
    Session.set_notebook_name(session.pid, "Cat's guide to life")

    Session.save(session.pid)
    assert_receive {:operation, {:mark_as_not_dirty, _}}

    assert [notebook_path] = Path.wildcard(notebook_glob)
    assert Path.basename(notebook_path) =~ "cats_guide_to_life"
  end

  defp start_session(opts \\ []) do
    session_id = Utils.random_id()
    {:ok, pid} = Session.start_link(Keyword.merge([id: session_id], opts))
    Session.get_by_pid(pid)
  end

  defp insert_section_and_cell(session_pid) do
    Session.insert_section(session_pid, 0)
    assert_receive {:operation, {:insert_section, _, 0, section_id}}
    Session.insert_cell(session_pid, section_id, 0, :elixir)
    assert_receive {:operation, {:insert_cell, _, ^section_id, 0, :elixir, cell_id}}

    {section_id, cell_id}
  end
end
