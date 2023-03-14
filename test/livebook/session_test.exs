defmodule Livebook.SessionTest do
  use ExUnit.Case, async: true

  import Livebook.TestHelpers

  alias Livebook.{Session, Delta, Runtime, Utils, Notebook, FileSystem}
  alias Livebook.Notebook.{Section, Cell}
  alias Livebook.Session.Data
  alias Livebook.NotebookManager

  @eval_meta %{
    errored: false,
    evaluation_time_ms: 10,
    identifiers_used: [],
    identifiers_defined: %{}
  }

  setup do
    session = start_session()
    %{session: session}
  end

  describe "file_name_for_download/1" do
    @tag :tmp_dir
    test "uses associated file name if one is attached", %{tmp_dir: tmp_dir} do
      tmp_dir = FileSystem.File.local(tmp_dir <> "/")
      file = FileSystem.File.resolve(tmp_dir, "my_notebook.livemd")
      session = start_session(file: file)

      assert Session.file_name_for_download(session) == "my_notebook"
    end

    test "defaults to notebook name", %{session: session} do
      Session.set_notebook_name(session.pid, "Cat's guide to life!")
      # Get the updated struct
      session = Session.get_by_pid(session.pid)

      assert Session.file_name_for_download(session) == "cats_guide_to_life"
    end

    test "removes non-ascii characters from notebook name", %{session: session} do
      Session.set_notebook_name(session.pid, "Notebook 😺")
      # Get the updated struct
      session = Session.get_by_pid(session.pid)

      assert Session.file_name_for_download(session) == "notebook"
    end
  end

  describe "set_notebook_attributes/2" do
    test "sends an attributes update to subscribers", %{session: session} do
      Session.subscribe(session.id)

      attrs = %{set_notebook_attributes: true}
      Session.set_notebook_attributes(session.pid, attrs)
      assert_receive {:operation, {:set_notebook_attributes, _client_id, ^attrs}}
    end
  end

  describe "insert_section/2" do
    test "sends an insert operation to subscribers", %{session: session} do
      Session.subscribe(session.id)

      Session.insert_section(session.pid, 0)
      assert_receive {:operation, {:insert_section, _client_id, 0, _id}}
    end
  end

  describe "insert_cell/4" do
    test "sends an insert operation to subscribers", %{session: session} do
      Session.subscribe(session.id)

      Session.insert_section(session.pid, 0)
      assert_receive {:operation, {:insert_section, _client_id, 0, section_id}}

      Session.insert_cell(session.pid, section_id, 0, :code)

      assert_receive {:operation, {:insert_cell, _client_id, ^section_id, 0, :code, _id, _attrs}}
    end
  end

  describe "delete_section/3" do
    test "sends a delete operation to subscribers", %{session: session} do
      Session.subscribe(session.id)

      {section_id, _cell_id} = insert_section_and_cell(session.pid)

      Session.delete_section(session.pid, section_id, false)
      assert_receive {:operation, {:delete_section, _client_id, ^section_id, false}}
    end
  end

  describe "delete_cell/2" do
    test "sends a delete operation to subscribers", %{session: session} do
      Session.subscribe(session.id)

      {_section_id, cell_id} = insert_section_and_cell(session.pid)

      Session.delete_cell(session.pid, cell_id)
      assert_receive {:operation, {:delete_cell, _client_id, ^cell_id}}
    end
  end

  describe "restore_cell/2" do
    test "sends a restore operation to subscribers", %{session: session} do
      Session.subscribe(session.id)

      {_section_id, cell_id} = insert_section_and_cell(session.pid)
      Session.delete_cell(session.pid, cell_id)

      Session.restore_cell(session.pid, cell_id)
      assert_receive {:operation, {:restore_cell, _client_id, ^cell_id}}
    end
  end

  describe "recover_smart_cell/2" do
    test "sends a recover operations to subscribers and starts the smart cell" do
      smart_cell = %{Notebook.Cell.new(:smart) | kind: "text", source: "content"}
      section = %{Notebook.Section.new() | cells: [smart_cell]}
      notebook = %{Notebook.new() | sections: [section]}

      session = start_session(notebook: notebook)

      send(
        session.pid,
        {:runtime_smart_cell_definitions, [%{kind: "text", name: "Text", requirement: nil}]}
      )

      send(
        session.pid,
        {:runtime_smart_cell_started, smart_cell.id,
         %{source: "content!", js_view: %{}, editor: nil}}
      )

      send(session.pid, {:runtime_smart_cell_down, smart_cell.id})

      Session.subscribe(session.id)

      Session.recover_smart_cell(session.pid, smart_cell.id)

      cell_id = smart_cell.id

      assert_receive {:operation, {:recover_smart_cell, _client_id, ^cell_id}}
      assert_receive {:operation, {:smart_cell_started, _, ^cell_id, _, _, _, _}}
    end
  end

  describe "convert_smart_cell/2" do
    test "sends a delete and insert operations to subscribers" do
      smart_cell = %{Notebook.Cell.new(:smart) | kind: "text", source: "content"}
      section = %{Notebook.Section.new() | cells: [smart_cell]}
      notebook = %{Notebook.new() | sections: [section]}

      session = start_session(notebook: notebook)

      Session.subscribe(session.id)

      Session.convert_smart_cell(session.pid, smart_cell.id)

      cell_id = smart_cell.id
      section_id = section.id

      assert_receive {:operation, {:delete_cell, _client_id, ^cell_id}}

      assert_receive {:operation,
                      {:insert_cell, _client_id, ^section_id, 0, :code, _id,
                       %{source: "content", outputs: []}}}
    end

    test "inserts multiple cells when the smart cell has explicit chunks" do
      smart_cell = %{
        Notebook.Cell.new(:smart)
        | kind: "text",
          source: "chunk 1\n\nchunk 2",
          chunks: [{0, 7}, {9, 7}],
          outputs: [{1, {:text, "Hello"}}]
      }

      section = %{Notebook.Section.new() | cells: [smart_cell]}
      notebook = %{Notebook.new() | sections: [section]}

      session = start_session(notebook: notebook)

      Session.subscribe(session.id)

      Session.convert_smart_cell(session.pid, smart_cell.id)

      cell_id = smart_cell.id
      section_id = section.id

      assert_receive {:operation, {:delete_cell, _client_id, ^cell_id}}

      assert_receive {:operation,
                      {:insert_cell, _client_id, ^section_id, 0, :code, _id,
                       %{source: "chunk 1", outputs: []}}}

      assert_receive {:operation,
                      {:insert_cell, _client_id, ^section_id, 1, :code, _id,
                       %{source: "chunk 2", outputs: [{1, {:text, "Hello"}}]}}}
    end

    test "doesn't garbage collect input values" do
      input = %{
        ref: :input_ref,
        id: "input1",
        type: :text,
        label: "Name",
        default: "hey",
        destination: :noop
      }

      smart_cell = %{
        Notebook.Cell.new(:smart)
        | kind: "text",
          source: "content",
          outputs: [{1, {:input, input}}]
      }

      section = %{Notebook.Section.new() | cells: [smart_cell]}
      notebook = %{Notebook.new() | sections: [section]}

      session = start_session(notebook: notebook)

      Session.subscribe(session.id)

      Session.convert_smart_cell(session.pid, smart_cell.id)

      assert %{input_values: %{"input1" => "hey"}} = Session.get_data(session.pid)
    end
  end

  describe "add_dependencies/2" do
    test "applies source change to the setup cell to include the given dependencies",
         %{session: session} do
      runtime = connected_noop_runtime()
      Session.set_runtime(session.pid, runtime)

      Session.subscribe(session.id)

      Session.add_dependencies(session.pid, [%{dep: {:jason, "~> 1.3.0"}, config: []}])

      assert_receive {:operation, {:apply_cell_delta, "__server__", "setup", :primary, _delta, 1}}

      assert %{
               notebook: %{
                 setup_section: %{
                   cells: [
                     %{
                       source: """
                       Mix.install([
                         {:jason, "~> 1.3.0"}
                       ])\
                       """
                     }
                   ]
                 }
               }
             } = Session.get_data(session.pid)
    end

    test "broadcasts an error if modifying the setup source fails" do
      notebook = Notebook.new() |> Notebook.update_cell("setup", &%{&1 | source: "[,]"})
      session = start_session(notebook: notebook)

      runtime = connected_noop_runtime()
      Session.set_runtime(session.pid, runtime)

      Session.subscribe(session.id)

      Session.add_dependencies(session.pid, [%{dep: {:jason, "~> 1.3.0"}, config: []}])

      assert_receive {:error, "failed to add dependencies to the setup cell, reason:" <> _}
    end
  end

  describe "queue_cell_evaluation/2" do
    test "triggers evaluation and sends update operation once it finishes",
         %{session: session} do
      Session.subscribe(session.id)

      {_section_id, cell_id} = insert_section_and_cell(session.pid)

      Session.queue_cell_evaluation(session.pid, cell_id)

      assert_receive {:operation, {:queue_cells_evaluation, _client_id, [^cell_id]}}

      assert_receive {:operation,
                      {:add_cell_evaluation_response, _, ^cell_id, _,
                       %{evaluation_time_ms: _time_ms}}}
    end
  end

  describe "cancel_cell_evaluation/2" do
    test "sends a cancel evaluation operation to subscribers", %{session: session} do
      Session.subscribe(session.id)

      {_section_id, cell_id} = insert_section_and_cell(session.pid)
      Session.queue_cell_evaluation(session.pid, cell_id)

      Session.cancel_cell_evaluation(session.pid, cell_id)

      assert_receive {:operation, {:cancel_cell_evaluation, _client_id, ^cell_id}}
    end
  end

  describe "set_notebook_name/2" do
    test "sends a notebook name update operation to subscribers", %{session: session} do
      Session.subscribe(session.id)

      Session.set_notebook_name(session.pid, "Cat's guide to life")
      assert_receive {:operation, {:set_notebook_name, _client_id, "Cat's guide to life"}}
    end

    @tag :tmp_dir
    test "updates name information in recent notebooks", %{session: session, tmp_dir: tmp_dir} do
      tmp_dir = FileSystem.File.local(tmp_dir <> "/")
      file = FileSystem.File.resolve(tmp_dir, "my_notebook.livemd")
      Session.set_file(session.pid, file)

      Session.set_notebook_name(session.pid, "New notebook name")

      wait_for_session_update(session.pid)

      assert %{name: "New notebook name"} =
               NotebookManager.recent_notebooks() |> Enum.find(&(&1.file == file))
    end
  end

  describe "set_section_name/3" do
    test "sends a section name update operation to subscribers", %{session: session} do
      Session.subscribe(session.id)

      {section_id, _cell_id} = insert_section_and_cell(session.pid)

      Session.set_section_name(session.pid, section_id, "Chapter 1")
      assert_receive {:operation, {:set_section_name, _client_id, ^section_id, "Chapter 1"}}
    end
  end

  describe "apply_cell_delta/4" do
    test "sends a cell delta operation to subscribers", %{session: session} do
      Session.subscribe(session.id)

      {_section_id, cell_id} = insert_section_and_cell(session.pid)

      delta = Delta.new() |> Delta.insert("cats")
      revision = 1

      Session.apply_cell_delta(session.pid, cell_id, :primary, delta, revision)

      assert_receive {:operation,
                      {:apply_cell_delta, _client_id, ^cell_id, :primary, ^delta, ^revision}}

      # Sends new digest to clients
      digest = :erlang.md5("cats")
      assert_receive {:hydrate_cell_source_digest, ^cell_id, :primary, ^digest}
    end
  end

  describe "report_cell_revision/3" do
    test "sends a revision report operation to subscribers", %{session: session} do
      Session.subscribe(session.id)

      {_section_id, cell_id} = insert_section_and_cell(session.pid)
      revision = 1

      Session.report_cell_revision(session.pid, cell_id, :primary, revision)

      assert_receive {:operation,
                      {:report_cell_revision, _client_id, ^cell_id, :primary, ^revision}}
    end
  end

  describe "set_cell_attributes/3" do
    test "sends an attributes update operation to subscribers", %{session: session} do
      Session.subscribe(session.id)

      {_section_id, cell_id} = insert_section_and_cell(session.pid)
      attrs = %{disable_formatting: true}

      Session.set_cell_attributes(session.pid, cell_id, attrs)
      assert_receive {:operation, {:set_cell_attributes, _client_id, ^cell_id, ^attrs}}
    end
  end

  describe "connect_runtime/2" do
    test "sends a runtime update operation to subscribers", %{session: session} do
      Session.subscribe(session.id)

      runtime = connected_noop_runtime()
      Session.set_runtime(session.pid, runtime)

      assert_receive {:operation, {:set_runtime, _client_id, ^runtime}}
    end
  end

  describe "disconnect_runtime/1" do
    test "sends a runtime update operation to subscribers", %{session: session} do
      Session.subscribe(session.id)

      runtime = connected_noop_runtime()
      Session.set_runtime(session.pid, runtime)
      assert_receive {:operation, {:set_runtime, _client_id, _}}

      # Calling twice can happen in a race, make sure it doesn't crash
      Session.disconnect_runtime(session.pid)
      Session.disconnect_runtime([session.pid])

      assert_receive {:operation, {:set_runtime, _client_id, runtime}}
      refute Runtime.connected?(runtime)
    end
  end

  describe "set_file/1" do
    @tag :tmp_dir
    test "sends a file update operation to subscribers",
         %{session: session, tmp_dir: tmp_dir} do
      Session.subscribe(session.id)

      tmp_dir = FileSystem.File.local(tmp_dir <> "/")
      file = FileSystem.File.resolve(tmp_dir, "notebook.livemd")
      Session.set_file(session.pid, file)

      assert_receive {:operation, {:set_file, _client_id, ^file}}
    end

    @tag :tmp_dir
    test "broadcasts an error if the path is already in use",
         %{session: session, tmp_dir: tmp_dir} do
      tmp_dir = FileSystem.File.local(tmp_dir <> "/")
      file = FileSystem.File.resolve(tmp_dir, "notebook.livemd")
      start_session(file: file)

      Session.subscribe(session.id)

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
      wait_for_session_update(session.pid)

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
      wait_for_session_update(session.pid)

      assert {:ok, true} = FileSystem.File.exists?(image_file)

      %{images_dir: new_images_dir} = session

      assert {:ok, true} =
               FileSystem.File.exists?(FileSystem.File.resolve(new_images_dir, "test.jpg"))
    end

    @tag :tmp_dir
    test "adds the new file to recent notebooks", %{session: session, tmp_dir: tmp_dir} do
      tmp_dir = FileSystem.File.local(tmp_dir <> "/")
      file = FileSystem.File.resolve(tmp_dir, "notebook.livemd")
      Session.set_file(session.pid, file)

      wait_for_session_update(session.pid)

      assert NotebookManager.recent_notebooks() |> Enum.any?(&(&1.file == file))
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

      Session.subscribe(session.id)

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

      Session.subscribe(session.id)

      assert {:ok, false} = FileSystem.File.exists?(file)

      Session.save(session.pid)

      assert_receive {:operation, {:mark_as_not_dirty, _}}
      assert {:ok, "# My notebook\n" <> _rest} = FileSystem.File.read(file)
    end
  end

  describe "register_file/4" do
    @tag :tmp_dir
    test "schedules old file for deletion when a file is registered for existing key",
         %{tmp_dir: tmp_dir} do
      session = start_session(registered_file_deletion_delay: 0)

      source_path = Path.join(tmp_dir, "old.txt")
      File.write!(source_path, "content")
      {:ok, old_file_ref} = Session.register_file(session.pid, source_path, "key")

      runtime = connected_noop_runtime()
      Session.set_runtime(session.pid, runtime)
      send(session.pid, {:runtime_file_lookup, self(), old_file_ref})
      assert_receive {:runtime_file_lookup_reply, {:ok, old_path}}

      source_path = Path.join(tmp_dir, "new.txt")
      File.write!(source_path, "content")
      {:ok, new_file_ref} = Session.register_file(session.pid, source_path, "key")

      send(session.pid, {:runtime_file_lookup, self(), new_file_ref})
      assert_receive {:runtime_file_lookup_reply, {:ok, new_path}}

      Process.sleep(50)

      refute File.exists?(old_path)
      assert File.exists?(new_path)
    end

    @tag :tmp_dir
    test "schedules file for deletion when a linked client leaves", %{tmp_dir: tmp_dir} do
      session = start_session(registered_file_deletion_delay: 0)

      client_pid =
        spawn_link(fn ->
          receive do
            :stop -> :ok
          end
        end)

      user = Livebook.Users.User.new()
      {_data, client_id} = Session.register_client(session.pid, client_pid, user)

      source_path = Path.join(tmp_dir, "old.txt")
      File.write!(source_path, "content")

      {:ok, file_ref} =
        Session.register_file(session.pid, source_path, "key", linked_client_id: client_id)

      runtime = connected_noop_runtime()
      Session.set_runtime(session.pid, runtime)
      send(session.pid, {:runtime_file_lookup, self(), file_ref})
      assert_receive {:runtime_file_lookup_reply, {:ok, path}}

      send(client_pid, :stop)

      Process.sleep(50)

      refute File.exists?(path)
    end

    @tag :tmp_dir
    test "schedules file for deletion when the corresponding input is removed",
         %{tmp_dir: tmp_dir} do
      input = %{
        ref: :input_ref,
        id: "input1",
        type: :file,
        label: "File",
        default: nil,
        destination: :noop,
        accept: :any
      }

      cell = %{Notebook.Cell.new(:code) | outputs: [{1, {:input, input}}]}
      notebook = %{Notebook.new() | sections: [%{Notebook.Section.new() | cells: [cell]}]}

      session = start_session(notebook: notebook, registered_file_deletion_delay: 0)

      source_path = Path.join(tmp_dir, "old.txt")
      File.write!(source_path, "content")

      {:ok, file_ref} = Session.register_file(session.pid, source_path, "key")

      Session.set_input_value(session.pid, "input1", %{
        file_ref: file_ref,
        client_name: "data.txt"
      })

      runtime = connected_noop_runtime()
      Session.set_runtime(session.pid, runtime)
      send(session.pid, {:runtime_file_lookup, self(), file_ref})
      assert_receive {:runtime_file_lookup_reply, {:ok, path}}

      Session.erase_outputs(session.pid)

      Process.sleep(50)

      refute File.exists?(path)
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

      Session.subscribe(session.id)

      assert {:ok, false} = FileSystem.File.exists?(file)

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

      Session.close(session.pid)

      # Wait for the session to deal with the files
      ref = Process.monitor(session.pid)
      assert_receive {:DOWN, ^ref, :process, _, _}

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

    @tag :tmp_dir
    test "adds file to recent notebooks when :file option is specified", %{tmp_dir: tmp_dir} do
      tmp_dir = FileSystem.File.local(tmp_dir <> "/")
      file = FileSystem.File.resolve(tmp_dir, "my_notebook.livemd")

      start_session(file: file)

      assert NotebookManager.recent_notebooks() |> Enum.any?(&(&1.file == file))
    end
  end

  # For most tests we use the lightweight embedded runtime,
  # so that they are cheap to run. Here go several integration
  # tests that actually start a Elixir standalone runtime (default in production)
  # to verify session integrates well with it properly.

  test "starts a standalone runtime upon first evaluation if there was none set explicitly" do
    session = start_session()

    Session.subscribe(session.id)

    {_section_id, cell_id} = insert_section_and_cell(session.pid)

    Session.queue_cell_evaluation(session.pid, cell_id)
    # Give it a bit more time as this involves starting a system process.
    assert_receive {:operation,
                    {:add_cell_evaluation_response, _, ^cell_id, _,
                     %{evaluation_time_ms: _time_ms}}}
  end

  test "if the runtime node goes down, notifies the subscribers" do
    session = start_session()
    {:ok, runtime} = Runtime.ElixirStandalone.new() |> Runtime.connect()

    Session.subscribe(session.id)

    # Wait for the runtime to be set
    Session.set_runtime(session.pid, runtime)
    assert_receive {:operation, {:set_runtime, _, ^runtime}}

    # Terminate the other node, the session should detect that
    Node.spawn(runtime.node, System, :halt, [])

    assert_receive {:operation, {:set_runtime, _, runtime}}
    refute Runtime.connected?(runtime)
    assert_receive {:error, "runtime node terminated unexpectedly - no connection"}
  end

  test "on user change sends an update operation subscribers", %{session: session} do
    user = Livebook.Users.User.new()
    Session.register_client(session.pid, self(), user)

    Session.subscribe(session.id)

    updated_user = %{user | name: "Jake Peralta"}
    Livebook.Users.broadcast_change(updated_user)

    assert_receive {:operation, {:update_user, _client_id, ^updated_user}}
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
      input_code_cell = %{Notebook.Cell.new(:code) | source: @livebook_put_input_code}

      code_cell = %{Notebook.Cell.new(:code) | source: @livebook_get_input_value_code}

      notebook = %{
        Notebook.new()
        | sections: [
            %{Notebook.Section.new() | cells: [input_code_cell, code_cell]}
          ]
      }

      session = start_session(notebook: notebook)

      cell_id = code_cell.id

      Session.subscribe(session.id)
      Session.queue_cell_evaluation(session.pid, cell_id)

      assert_receive {:operation,
                      {:add_cell_evaluation_response, _, ^cell_id, {:text, text_output},
                       %{evaluation_time_ms: _time_ms}}}

      assert text_output =~ "hey"
    end

    test "replies with error when no matching input is found" do
      code_cell = %{Notebook.Cell.new(:code) | source: @livebook_get_input_value_code}

      notebook = %{
        Notebook.new()
        | sections: [
            %{Notebook.Section.new() | cells: [code_cell]}
          ]
      }

      session = start_session(notebook: notebook)

      cell_id = code_cell.id

      Session.subscribe(session.id)
      Session.queue_cell_evaluation(session.pid, cell_id)

      assert_receive {:operation,
                      {:add_cell_evaluation_response, _, ^cell_id, {:text, text_output},
                       %{evaluation_time_ms: _time_ms}}}

      assert text_output =~ ":error"
    end
  end

  describe "smart cells" do
    test "notifies subscribers when a smart cell starts and passes source diff as delta" do
      smart_cell = %{Notebook.Cell.new(:smart) | kind: "text", source: "content"}
      notebook = %{Notebook.new() | sections: [%{Notebook.Section.new() | cells: [smart_cell]}]}
      session = start_session(notebook: notebook)

      runtime = connected_noop_runtime()
      Session.set_runtime(session.pid, runtime)

      send(
        session.pid,
        {:runtime_smart_cell_definitions, [%{kind: "text", name: "Text", requirement: nil}]}
      )

      Session.subscribe(session.id)

      send(
        session.pid,
        {:runtime_smart_cell_started, smart_cell.id,
         %{source: "content!", js_view: %{}, editor: nil}}
      )

      delta = Delta.new() |> Delta.retain(7) |> Delta.insert("!")
      cell_id = smart_cell.id

      assert_receive {:operation, {:smart_cell_started, _, ^cell_id, ^delta, nil, %{}, nil}}
    end

    test "sends an event to the smart cell server when the editor source changes" do
      smart_cell = %{Notebook.Cell.new(:smart) | kind: "text", source: ""}
      notebook = %{Notebook.new() | sections: [%{Notebook.Section.new() | cells: [smart_cell]}]}
      session = start_session(notebook: notebook)

      runtime = connected_noop_runtime()
      Session.set_runtime(session.pid, runtime)

      send(
        session.pid,
        {:runtime_smart_cell_definitions, [%{kind: "text", name: "Text", requirement: nil}]}
      )

      server_pid = self()

      send(
        session.pid,
        {:runtime_smart_cell_started, smart_cell.id,
         %{
           source: "content",
           js_view: %{ref: smart_cell.id, pid: server_pid, assets: %{}},
           editor: %{language: nil, placement: :bottom, source: "content"}
         }}
      )

      Session.register_client(session.pid, self(), Livebook.Users.User.new())

      delta = Delta.new() |> Delta.retain(7) |> Delta.insert("!")
      Session.apply_cell_delta(session.pid, smart_cell.id, :secondary, delta, 1)

      assert_receive {:editor_source, "content!"}
    end

    test "normalizes line endings in smart cells having an editor" do
      # Prior to Livebook 0.7.0 the editor would use system line endings,
      # hence smart cells having editor may have CRLF in their persisted
      # source, so we want to normalize it upfront

      smart_cell = %{Notebook.Cell.new(:smart) | kind: "text", source: ""}
      notebook = %{Notebook.new() | sections: [%{Notebook.Section.new() | cells: [smart_cell]}]}
      session = start_session(notebook: notebook)

      runtime = connected_noop_runtime()
      Session.set_runtime(session.pid, runtime)

      send(
        session.pid,
        {:runtime_smart_cell_definitions, [%{kind: "text", name: "Text", requirement: nil}]}
      )

      server_pid = self()

      send(
        session.pid,
        {:runtime_smart_cell_started, smart_cell.id,
         %{
           source: "content\r\nmultiline",
           js_view: %{ref: smart_cell.id, pid: server_pid, assets: %{}},
           editor: %{language: nil, placement: :bottom, source: "content\r\nmultiline"}
         }}
      )

      assert %{
               notebook: %{
                 sections: [
                   %{
                     cells: [
                       %{source: "content\nmultiline", editor: %{source: "content\nmultiline"}}
                     ]
                   }
                 ]
               }
             } = Session.get_data(session.pid)
    end

    test "pings the smart cell before evaluation to await all incoming messages" do
      smart_cell = %{Notebook.Cell.new(:smart) | kind: "text", source: "1"}
      notebook = %{Notebook.new() | sections: [%{Notebook.Section.new() | cells: [smart_cell]}]}
      session = start_session(notebook: notebook)

      runtime = connected_noop_runtime()
      Session.set_runtime(session.pid, runtime)

      send(
        session.pid,
        {:runtime_smart_cell_definitions, [%{kind: "text", name: "Text", requirement: nil}]}
      )

      Session.subscribe(session.id)

      send(
        session.pid,
        {:runtime_smart_cell_started, smart_cell.id,
         %{source: "1", js_view: %{pid: self(), ref: "ref"}, editor: nil}}
      )

      Session.queue_cell_evaluation(session.pid, smart_cell.id)

      send(session.pid, {:runtime_evaluation_response, "setup", {:ok, ""}, @eval_meta})

      session_pid = session.pid
      assert_receive {:ping, ^session_pid, metadata, %{ref: "ref"}}

      # Update the source before replying to ping
      send(
        session.pid,
        {:runtime_smart_cell_update, smart_cell.id, %{}, "2", %{reevaluate: false}}
      )

      send(session_pid, {:pong, metadata, %{ref: "ref"}})

      # Sends new digest to clients
      cell_id = smart_cell.id
      new_digest = :erlang.md5("2")
      assert_receive {:hydrate_cell_source_digest, ^cell_id, :primary, ^new_digest}
    end
  end

  describe "parent_locators_for_cell/2" do
    test "given cell in main flow returns previous Code cells" do
      cell1 = %{Cell.new(:code) | id: "c1"}
      cell2 = %{Cell.new(:markdown) | id: "c2"}
      section1 = %{Section.new() | id: "s1", cells: [cell1, cell2]}

      cell3 = %{Cell.new(:code) | id: "c3"}
      section2 = %{Section.new() | id: "s2", cells: [cell3]}

      notebook = %{Notebook.new() | sections: [section1, section2]}
      data = Data.new(notebook: notebook)

      data =
        data_after_operations!(data, [
          {:set_runtime, self(), connected_noop_runtime()},
          {:queue_cells_evaluation, self(), ["c1"]},
          {:add_cell_evaluation_response, self(), "setup", {:ok, nil}, @eval_meta},
          {:add_cell_evaluation_response, self(), "c1", {:ok, nil}, @eval_meta}
        ])

      assert [{:main_flow, "c1"}, {:main_flow, "setup"}] =
               Session.parent_locators_for_cell(data, cell3)
    end

    test "given cell in branching section returns Code cells from both sections" do
      section1 = %{Section.new() | id: "s1"}

      cell1 = %{Cell.new(:code) | id: "c1"}
      cell2 = %{Cell.new(:markdown) | id: "c2"}
      cell3 = %{Cell.new(:code) | id: "c3"}

      section2 = %{
        Section.new()
        | id: "s2",
          parent_id: "s1",
          cells: [cell1, cell2, cell3]
      }

      notebook = %{Notebook.new() | sections: [section1, section2]}
      data = Data.new(notebook: notebook)

      data =
        data_after_operations!(data, [
          {:set_runtime, self(), connected_noop_runtime()},
          {:queue_cells_evaluation, self(), ["c1"]},
          {:add_cell_evaluation_response, self(), "setup", {:ok, nil}, @eval_meta},
          {:add_cell_evaluation_response, self(), "c1", {:ok, nil}, @eval_meta}
        ])

      assert [{"s2", "c1"}, {:main_flow, "setup"}] = Session.parent_locators_for_cell(data, cell3)
    end

    test "given cell in main flow returns an empty list if there is no previous cell" do
      %{setup_section: %{cells: [setup_cell]}} = notebook = Notebook.new()
      data = Data.new(notebook: notebook)

      assert [] = Session.parent_locators_for_cell(data, setup_cell)
    end

    test "ignores fresh and aborted cells" do
      cell1 = %{Cell.new(:code) | id: "c1"}
      cell2 = %{Cell.new(:code) | id: "c2"}
      section1 = %{Section.new() | id: "s1", cells: [cell1, cell2]}

      cell3 = %{Cell.new(:code) | id: "c3"}
      section2 = %{Section.new() | id: "s2", cells: [cell3]}

      notebook = %{Notebook.new() | sections: [section1, section2]}
      data = Data.new(notebook: notebook)

      assert [] = Session.parent_locators_for_cell(data, cell3)

      data =
        data_after_operations!(data, [
          {:set_runtime, self(), connected_noop_runtime()},
          {:queue_cells_evaluation, self(), ["c1"]},
          {:add_cell_evaluation_response, self(), "setup", {:ok, nil}, @eval_meta},
          {:add_cell_evaluation_response, self(), "c1", {:ok, nil}, @eval_meta}
        ])

      assert [{:main_flow, "c1"}, {:main_flow, "setup"}] =
               Session.parent_locators_for_cell(data, cell3)

      data =
        data_after_operations!(data, [
          {:reflect_main_evaluation_failure, self()}
        ])

      assert [] = Session.parent_locators_for_cell(data, cell3)
    end
  end

  test "session has the creation timestamp", %{session: session} do
    assert %DateTime{} = session.created_at
    assert DateTime.compare(session.created_at, DateTime.utc_now()) in [:lt, :eq]
  end

  @tag :tmp_dir
  test "session without a file is persisted to autosave path", %{tmp_dir: tmp_dir} do
    session = start_session(autosave_path: tmp_dir)

    notebook_glob = Path.join(tmp_dir, "**/*.livemd")

    Session.subscribe(session.id)

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

  test "successfully fetches assets for client-specific outputs", %{session: session} do
    Session.subscribe(session.id)

    {_section_id, cell_id} = insert_section_and_cell(session.pid)

    runtime = connected_noop_runtime()
    Session.set_runtime(session.pid, runtime)

    archive_path = Path.expand("../support/assets.tar.gz", __DIR__)
    hash = "test-" <> Livebook.Utils.random_id()
    assets_info = %{archive_path: archive_path, hash: hash, js_path: "main.js"}
    js_output = {:js, %{js_view: %{assets: assets_info}}}
    frame_output = {:frame, [js_output], %{ref: "1", type: :replace}}

    user = Livebook.Users.User.new()
    {_, client_id} = Session.register_client(session.pid, self(), user)

    # Send client-specific output
    send(session.pid, {:runtime_evaluation_output_to, client_id, cell_id, frame_output})

    assert_receive {:operation, {:add_cell_evaluation_output, _, ^cell_id, ^frame_output}}

    # The assets should be available
    assert :ok = Session.fetch_assets(session.pid, hash)
  end

  describe "apps" do
    test "deploying an app under the same slug terminates the old one", %{session: session} do
      Session.subscribe(session.id)

      slug = Livebook.Utils.random_short_id()
      app_settings = %{Livebook.Notebook.AppSettings.new() | slug: slug}
      Session.set_app_settings(session.pid, app_settings)

      Session.deploy_app(session.pid)
      assert_receive {:operation, {:add_app, _, app1_session_id, _app1_session_pid}}
      assert_receive {:operation, {:set_app_registered, _, ^app1_session_id, true}}

      Session.app_subscribe(app1_session_id)

      Session.deploy_app(session.pid)
      assert_receive {:operation, {:add_app, _, app2_session_id, app2_session_pid}}
      assert_receive {:operation, {:set_app_registered, _, ^app1_session_id, false}}
      assert_receive {:operation, {:set_app_registered, _, ^app2_session_id, true}}

      assert_receive {:app_terminated, ^app1_session_id}

      assert {:ok, %{id: ^app2_session_id}} = Livebook.Apps.fetch_session_by_slug(slug)

      Session.app_unregistered(app2_session_pid)
    end

    test "recovers on failure", %{test: test} do
      code =
        quote do
          # This test uses the Embedded runtime, so we can target the
          # process by name, this way make the scenario predictable
          # and avoid long sleeps
          Process.register(self(), unquote(test))
        end
        |> Macro.to_string()

      cell = %{Notebook.Cell.new(:code) | source: code}
      section = %{Notebook.Section.new() | cells: [cell]}
      notebook = %{Notebook.new() | sections: [section]}

      session = start_session(notebook: notebook)

      Session.subscribe(session.id)

      slug = Livebook.Utils.random_short_id()
      app_settings = %{Livebook.Notebook.AppSettings.new() | slug: slug}
      Session.set_app_settings(session.pid, app_settings)

      Session.deploy_app(session.pid)

      assert_receive {:operation, {:add_app, _, app_session_id, app_session_pid}}
      assert_receive {:operation, {:set_app_status, _, ^app_session_id, :running}}

      Process.exit(Process.whereis(test), :shutdown)

      assert_receive {:operation, {:set_app_status, _, ^app_session_id, :error}}
      assert_receive {:operation, {:set_app_status, _, ^app_session_id, :booting}}
      assert_receive {:operation, {:set_app_status, _, ^app_session_id, :running}}

      Session.app_unregistered(app_session_pid)
    end
  end

  defp start_session(opts \\ []) do
    opts = Keyword.merge([id: Utils.random_id()], opts)
    pid = start_supervised!({Session, opts}, id: opts[:id])
    Session.get_by_pid(pid)
  end

  defp insert_section_and_cell(session_pid) do
    Session.insert_section(session_pid, 0)
    assert_receive {:operation, {:insert_section, _, 0, section_id}}
    Session.insert_cell(session_pid, section_id, 0, :code)
    assert_receive {:operation, {:insert_cell, _, ^section_id, 0, :code, cell_id, _attrs}}

    {section_id, cell_id}
  end

  defp connected_noop_runtime() do
    {:ok, runtime} = Livebook.Runtime.NoopRuntime.new() |> Livebook.Runtime.connect()
    runtime
  end

  defp wait_for_session_update(session_pid) do
    # This call is synchronous, so it gives the session time
    # for handling the previously sent change messages.
    Session.get_data(session_pid)
    :ok
  end
end
