defmodule Livebook.SessionTest do
  use ExUnit.Case, async: true

  alias Livebook.{Session, Delta, Runtime, Utils, Notebook}

  # Note: queueing evaluation in most of the tests below
  # requires the runtime to synchronously start first,
  # so we use a longer timeout just to make sure the tests
  # pass reliably
  @evaluation_wait_timeout 2_000

  setup do
    session_id = start_session()
    %{session_id: session_id}
  end

  describe "insert_section/2" do
    test "sends an insert opreation to subscribers", %{session_id: session_id} do
      Phoenix.PubSub.subscribe(Livebook.PubSub, "sessions:#{session_id}")
      pid = self()

      Session.insert_section(session_id, 0)
      assert_receive {:operation, {:insert_section, ^pid, 0, _id}}
    end
  end

  describe "insert_cell/4" do
    test "sends an insert opreation to subscribers", %{session_id: session_id} do
      Phoenix.PubSub.subscribe(Livebook.PubSub, "sessions:#{session_id}")
      pid = self()

      Session.insert_section(session_id, 0)
      assert_receive {:operation, {:insert_section, ^pid, 0, section_id}}

      Session.insert_cell(session_id, section_id, 0, :elixir)
      assert_receive {:operation, {:insert_cell, ^pid, ^section_id, 0, :elixir, _id}}
    end
  end

  describe "delete_section/3" do
    test "sends a delete opreation to subscribers", %{session_id: session_id} do
      Phoenix.PubSub.subscribe(Livebook.PubSub, "sessions:#{session_id}")
      pid = self()

      {section_id, _cell_id} = insert_section_and_cell(session_id)

      Session.delete_section(session_id, section_id, false)
      assert_receive {:operation, {:delete_section, ^pid, ^section_id, false}}
    end
  end

  describe "delete_cell/2" do
    test "sends a delete opreation to subscribers", %{session_id: session_id} do
      Phoenix.PubSub.subscribe(Livebook.PubSub, "sessions:#{session_id}")
      pid = self()

      {_section_id, cell_id} = insert_section_and_cell(session_id)

      Session.delete_cell(session_id, cell_id)
      assert_receive {:operation, {:delete_cell, ^pid, ^cell_id}}
    end
  end

  describe "queue_cell_evaluation/2" do
    test "sends a queue evaluation operation to subscribers", %{session_id: session_id} do
      Phoenix.PubSub.subscribe(Livebook.PubSub, "sessions:#{session_id}")
      pid = self()

      {_section_id, cell_id} = insert_section_and_cell(session_id)

      Session.queue_cell_evaluation(session_id, cell_id)

      assert_receive {:operation, {:queue_cell_evaluation, ^pid, ^cell_id}},
                     @evaluation_wait_timeout
    end

    test "triggers evaluation and sends update operation once it finishes",
         %{session_id: session_id} do
      Phoenix.PubSub.subscribe(Livebook.PubSub, "sessions:#{session_id}")

      {_section_id, cell_id} = insert_section_and_cell(session_id)

      Session.queue_cell_evaluation(session_id, cell_id)

      assert_receive {:operation,
                      {:add_cell_evaluation_response, _, ^cell_id, _,
                       %{evaluation_time_ms: _time_ms}}},
                     @evaluation_wait_timeout
    end
  end

  describe "cancel_cell_evaluation/2" do
    test "sends a cancel evaluation operation to subscribers", %{session_id: session_id} do
      Phoenix.PubSub.subscribe(Livebook.PubSub, "sessions:#{session_id}")
      pid = self()

      {_section_id, cell_id} = insert_section_and_cell(session_id)
      Session.queue_cell_evaluation(session_id, cell_id)

      Session.cancel_cell_evaluation(session_id, cell_id)

      assert_receive {:operation, {:cancel_cell_evaluation, ^pid, ^cell_id}},
                     @evaluation_wait_timeout
    end
  end

  describe "set_notebook_name/2" do
    test "sends a notebook name update operation to subscribers", %{session_id: session_id} do
      Phoenix.PubSub.subscribe(Livebook.PubSub, "sessions:#{session_id}")
      pid = self()

      Session.set_notebook_name(session_id, "Cat's guide to life")
      assert_receive {:operation, {:set_notebook_name, ^pid, "Cat's guide to life"}}
    end
  end

  describe "set_section_name/3" do
    test "sends a section name update operation to subscribers", %{session_id: session_id} do
      Phoenix.PubSub.subscribe(Livebook.PubSub, "sessions:#{session_id}")
      pid = self()

      {section_id, _cell_id} = insert_section_and_cell(session_id)

      Session.set_section_name(session_id, section_id, "Chapter 1")
      assert_receive {:operation, {:set_section_name, ^pid, ^section_id, "Chapter 1"}}
    end
  end

  describe "apply_cell_delta/4" do
    test "sends a cell delta operation to subscribers", %{session_id: session_id} do
      Phoenix.PubSub.subscribe(Livebook.PubSub, "sessions:#{session_id}")
      pid = self()

      {_section_id, cell_id} = insert_section_and_cell(session_id)

      delta = Delta.new() |> Delta.insert("cats")
      revision = 1

      Session.apply_cell_delta(session_id, cell_id, delta, revision)
      assert_receive {:operation, {:apply_cell_delta, ^pid, ^cell_id, ^delta, ^revision}}
    end
  end

  describe "report_cell_revision/3" do
    test "sends a revision report operation to subscribers", %{session_id: session_id} do
      Phoenix.PubSub.subscribe(Livebook.PubSub, "sessions:#{session_id}")
      pid = self()

      {_section_id, cell_id} = insert_section_and_cell(session_id)
      revision = 1

      Session.report_cell_revision(session_id, cell_id, revision)
      assert_receive {:operation, {:report_cell_revision, ^pid, ^cell_id, ^revision}}
    end
  end

  describe "set_cell_attributes/3" do
    test "sends an attributes update operation to subscribers", %{session_id: session_id} do
      Phoenix.PubSub.subscribe(Livebook.PubSub, "sessions:#{session_id}")
      pid = self()

      {_section_id, cell_id} = insert_section_and_cell(session_id)
      attrs = %{metadata: %{"disable_formatting" => true}}

      Session.set_cell_attributes(session_id, cell_id, attrs)
      assert_receive {:operation, {:set_cell_attributes, ^pid, ^cell_id, ^attrs}}
    end
  end

  describe "connect_runtime/2" do
    test "sends a runtime update operation to subscribers", %{session_id: session_id} do
      Phoenix.PubSub.subscribe(Livebook.PubSub, "sessions:#{session_id}")
      pid = self()

      {:ok, runtime} = Livebook.Runtime.Embedded.init()
      Session.connect_runtime(session_id, runtime)

      assert_receive {:operation, {:set_runtime, ^pid, ^runtime}}
    end
  end

  describe "disconnect_runtime/1" do
    test "sends a runtime update operation to subscribers", %{session_id: session_id} do
      Phoenix.PubSub.subscribe(Livebook.PubSub, "sessions:#{session_id}")
      pid = self()

      {:ok, runtime} = Livebook.Runtime.Embedded.init()
      Session.connect_runtime(session_id, runtime)

      Session.disconnect_runtime(session_id)

      assert_receive {:operation, {:set_runtime, ^pid, nil}}
    end
  end

  describe "set_path/1" do
    @tag :tmp_dir
    test "sends a path update operation to subscribers",
         %{session_id: session_id, tmp_dir: tmp_dir} do
      Phoenix.PubSub.subscribe(Livebook.PubSub, "sessions:#{session_id}")
      pid = self()

      path = Path.join(tmp_dir, "notebook.livemd")
      Session.set_path(session_id, path)

      assert_receive {:operation, {:set_path, ^pid, ^path}}
    end

    @tag :tmp_dir
    test "broadcasts an error if the path is already in use",
         %{session_id: session_id, tmp_dir: tmp_dir} do
      path = Path.join(tmp_dir, "notebook.livemd")
      start_session(path: path)

      Phoenix.PubSub.subscribe(Livebook.PubSub, "sessions:#{session_id}")

      Session.set_path(session_id, path)

      assert_receive {:error, "failed to set new path because it is already in use"}
    end

    @tag :tmp_dir
    test "moves images to the new directory", %{session_id: session_id, tmp_dir: tmp_dir} do
      %{images_dir: images_dir} = Session.get_summary(session_id)
      File.mkdir_p!(images_dir)
      images_dir |> Path.join("test.jpg") |> File.touch!()

      path = Path.join(tmp_dir, "notebook.livemd")
      Session.set_path(session_id, path)

      # Wait for the session to deal with the files
      Process.sleep(50)

      assert File.exists?(Path.join([tmp_dir, "images", "test.jpg"]))
      refute File.exists?(images_dir)
    end

    @tag :tmp_dir
    test "does not remove images from the previous dir if not temporary",
         %{session_id: session_id, tmp_dir: tmp_dir} do
      path = Path.join(tmp_dir, "notebook.livemd")
      Session.set_path(session_id, path)

      %{images_dir: images_dir} = Session.get_summary(session_id)
      File.mkdir_p!(images_dir)
      images_dir |> Path.join("test.jpg") |> File.touch!()

      Session.set_path(session_id, nil)

      # Wait for the session to deal with the files
      Process.sleep(50)

      assert File.exists?(Path.join(images_dir, "test.jpg"))

      %{images_dir: new_images_dir} = Session.get_summary(session_id)
      assert File.exists?(Path.join(new_images_dir, "test.jpg"))
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

      Phoenix.PubSub.subscribe(Livebook.PubSub, "sessions:#{session_id}")

      refute File.exists?(path)

      Session.save(session_id)

      assert_receive {:operation, {:mark_as_not_dirty, _}}
      assert File.exists?(path)
      assert File.read!(path) =~ "My notebook"
    end

    @tag :tmp_dir
    test "creates nonexistent directories", %{session_id: session_id, tmp_dir: tmp_dir} do
      path = Path.join(tmp_dir, "nonexistent/dir/notebook.livemd")
      Session.set_path(session_id, path)
      # Perform a change, so the notebook is dirty
      Session.set_notebook_name(session_id, "My notebook")

      Phoenix.PubSub.subscribe(Livebook.PubSub, "sessions:#{session_id}")

      refute File.exists?(path)

      Session.save(session_id)

      assert_receive {:operation, {:mark_as_not_dirty, _}}
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

      Phoenix.PubSub.subscribe(Livebook.PubSub, "sessions:#{session_id}")

      refute File.exists?(path)

      Process.flag(:trap_exit, true)
      Session.close(session_id)

      assert_receive :session_closed
      assert File.exists?(path)
      assert File.read!(path) =~ "My notebook"
    end

    test "clears session temporary directory", %{session_id: session_id} do
      %{images_dir: images_dir} = Session.get_summary(session_id)
      File.mkdir_p!(images_dir)

      assert File.exists?(images_dir)

      Process.flag(:trap_exit, true)
      Session.close(session_id)

      # Wait for the session to deal with the files
      Process.sleep(50)

      refute File.exists?(images_dir)
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

    @tag :tmp_dir
    test "copies images when :copy_images_from option is specified", %{tmp_dir: tmp_dir} do
      tmp_dir |> Path.join("image.jpg") |> File.touch!()

      session_id = start_session(copy_images_from: tmp_dir)

      %{images_dir: images_dir} = Session.get_summary(session_id)
      assert File.exists?(Path.join(images_dir, "image.jpg"))
    end

    test "saves images when :images option is specified" do
      images = %{"image.jpg" => "binary content"}

      session_id = start_session(images: images)

      %{images_dir: images_dir} = Session.get_summary(session_id)
      assert Path.join(images_dir, "image.jpg") |> File.read!() == "binary content"
    end
  end

  # For most tests we use the lightweight embedded runtime,
  # so that they are cheap to run. Here go several integration
  # tests that actually start a Elixir standalone runtime (default in production)
  # to verify session integrates well with it properly.

  test "starts a standalone runtime upon first evaluation if there was none set explicitly" do
    session_id = Utils.random_id()
    {:ok, _} = Session.start_link(id: session_id)

    Phoenix.PubSub.subscribe(Livebook.PubSub, "sessions:#{session_id}")

    {_section_id, cell_id} = insert_section_and_cell(session_id)

    Session.queue_cell_evaluation(session_id, cell_id)
    # Give it a bit more time as this involves starting a system process.
    assert_receive {:operation,
                    {:add_cell_evaluation_response, _, ^cell_id, _,
                     %{evaluation_time_ms: _time_ms}}},
                   @evaluation_wait_timeout
  end

  test "if the runtime node goes down, notifies the subscribers" do
    session_id = Utils.random_id()
    {:ok, _} = Session.start_link(id: session_id)
    {:ok, runtime} = Runtime.ElixirStandalone.init()

    Phoenix.PubSub.subscribe(Livebook.PubSub, "sessions:#{session_id}")

    # Wait for the runtime to be set
    Session.connect_runtime(session_id, runtime)
    assert_receive {:operation, {:set_runtime, _, ^runtime}}

    # Terminate the other node, the session should detect that
    Node.spawn(runtime.node, System, :halt, [])

    assert_receive {:operation, {:set_runtime, _, nil}}
    assert_receive {:info, "runtime node terminated unexpectedly"}
  end

  test "on user change sends an update operation subscribers", %{session_id: session_id} do
    user = Livebook.Users.User.new()
    Session.register_client(session_id, self(), user)

    Phoenix.PubSub.subscribe(Livebook.PubSub, "sessions:#{session_id}")

    updated_user = %{user | name: "Jake Peralta"}
    Livebook.Users.broadcast_change(updated_user)

    assert_receive {:operation, {:update_user, _pid, ^updated_user}}
  end

  # Integration tests concerning input communication
  # between runtime and session
  describe "user input" do
    test "replies to runtime input request" do
      input_cell = %{Notebook.Cell.new(:input) | name: "name", value: "Jake Peralta"}

      elixir_cell = %{
        Notebook.Cell.new(:elixir)
        | source: """
          IO.gets("name: ")
          """
      }

      notebook = %{
        Notebook.new()
        | sections: [
            %{Notebook.Section.new() | cells: [input_cell, elixir_cell]}
          ]
      }

      session_id = start_session(notebook: notebook)

      cell_id = elixir_cell.id

      Phoenix.PubSub.subscribe(Livebook.PubSub, "sessions:#{session_id}")
      Session.queue_cell_evaluation(session_id, cell_id)

      assert_receive {:operation,
                      {:add_cell_evaluation_response, _, ^cell_id, {:text, text_output},
                       %{evaluation_time_ms: _time_ms}}},
                     @evaluation_wait_timeout

      assert text_output =~ "Jake Peralta"
    end

    test "replies with error when no matching input is found" do
      elixir_cell = %{
        Notebook.Cell.new(:elixir)
        | source: """
          IO.gets("name: ")
          """
      }

      notebook = %{
        Notebook.new()
        | sections: [
            %{Notebook.Section.new() | cells: [elixir_cell]}
          ]
      }

      session_id = start_session(notebook: notebook)

      cell_id = elixir_cell.id

      Phoenix.PubSub.subscribe(Livebook.PubSub, "sessions:#{session_id}")
      Session.queue_cell_evaluation(session_id, cell_id)

      assert_receive {:operation,
                      {:add_cell_evaluation_response, _, ^cell_id, {:text, text_output},
                       %{evaluation_time_ms: _time_ms}}},
                     @evaluation_wait_timeout

      assert text_output =~ "no matching Livebook input found"
    end

    test "replies with error when the matching input is invalid" do
      input_cell = %{Notebook.Cell.new(:input) | type: :url, name: "url", value: "invalid"}

      elixir_cell = %{
        Notebook.Cell.new(:elixir)
        | source: """
          IO.gets("name: ")
          """
      }

      notebook = %{
        Notebook.new()
        | sections: [
            %{Notebook.Section.new() | cells: [input_cell, elixir_cell]}
          ]
      }

      session_id = start_session(notebook: notebook)

      cell_id = elixir_cell.id

      Phoenix.PubSub.subscribe(Livebook.PubSub, "sessions:#{session_id}")
      Session.queue_cell_evaluation(session_id, cell_id)

      assert_receive {:operation,
                      {:add_cell_evaluation_response, _, ^cell_id, {:text, text_output},
                       %{evaluation_time_ms: _time_ms}}},
                     @evaluation_wait_timeout

      assert text_output =~ "no matching Livebook input found"
    end
  end

  defp start_session(opts \\ []) do
    session_id = Utils.random_id()
    {:ok, _} = Session.start_link(Keyword.merge(opts, id: session_id))
    session_id
  end

  defp insert_section_and_cell(session_id) do
    Session.insert_section(session_id, 0)
    assert_receive {:operation, {:insert_section, _, 0, section_id}}
    Session.insert_cell(session_id, section_id, 0, :elixir)
    assert_receive {:operation, {:insert_cell, _, ^section_id, 0, :elixir, cell_id}}

    {section_id, cell_id}
  end
end
