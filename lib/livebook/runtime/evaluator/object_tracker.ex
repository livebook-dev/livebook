defmodule Livebook.Runtime.Evaluator.ObjectTracker do
  @moduledoc false

  # This module is an abstraction for tracking objects,
  # references to them and garbage collection.
  #
  # Every object is identified by an arbitrary unique term.
  # Processes can reference those objects by adding a pair
  # of `{pid, scope}`, scope is an optional additinal term
  # distinguishing the reference.
  #
  # Each reference can be released either manually by calling
  # `remove_reference/2` or automatically when the pointing
  # process terminates.
  #
  # When all references for the given object are removed,
  # all messages scheduled with `monitor/3` are sent.

  use GenServer

  @type state :: %{
          objects: %{
            object() => %{
              references: list(object_reference()),
              monitors: list(monitor())
            }
          }
        }

  @typedoc """
  Arbitrary term identifying an object.
  """
  @type object :: term()

  @typedoc """
  Reference to an object with an optional scope.
  """
  @type object_reference :: {process :: pid(), scope :: term()}

  @typedoc """
  Scheduled message to be sent when an object is released.
  """
  @type monitor :: {Process.dest(), payload :: term()}

  @doc """
  Starts a new object tracker.
  """
  @spec start_link(keyword()) :: GenServer.on_start()
  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts)
  end

  @doc """
  Adds a reference to the given object.
  """
  @spec add_reference(pid(), object(), object_reference()) :: :ok
  def add_reference(object_tracker, object, reference) do
    GenServer.cast(object_tracker, {:add_reference, object, reference})
  end

  @doc """
  Removes the given reference from all objects it is attached to.
  """
  @spec remove_reference(pid(), object_reference()) :: :ok
  def remove_reference(object_tracker, reference) do
    GenServer.cast(object_tracker, {:remove_reference, reference})
  end

  @doc """
  Schedules `payload` to be send to `destination` when the object
  is released.
  """
  @spec monitor(pid(), object(), Process.dest(), term()) :: :ok | {:error, :bad_object}
  def monitor(object_tracker, object, destination, payload) do
    GenServer.call(object_tracker, {:monitor, object, destination, payload})
  end

  @impl true
  def init(_opts) do
    {:ok, %{objects: %{}}}
  end

  @impl true
  def handle_cast({:add_reference, object, reference}, state) do
    {parent, _scope} = reference
    Process.monitor(parent)

    state =
      if state.objects[object] do
        update_in(state.objects[object].references, fn references ->
          if reference in references, do: references, else: [reference | references]
        end)
      else
        put_in(state.objects[object], %{references: [reference], monitors: []})
      end

    {:noreply, state}
  end

  def handle_cast({:remove_reference, reference}, state) do
    state = update_references(state, fn references -> List.delete(references, reference) end)

    {:noreply, garbage_collect(state)}
  end

  @impl true
  def handle_call({:monitor, object, destination, payload}, _from, state) do
    monitor = {destination, payload}

    if state.objects[object] do
      state =
        update_in(state.objects[object].monitors, fn monitors ->
          if monitor in monitors, do: monitors, else: [monitor | monitors]
        end)

      {:reply, :ok, garbage_collect(state)}
    else
      {:reply, {:error, :bad_object}, state}
    end
  end

  @impl true
  def handle_info({:DOWN, _ref, :process, pid, _reason}, state) do
    state =
      update_references(state, fn references ->
        Enum.reject(references, &match?({^pid, _}, &1))
      end)

    {:noreply, garbage_collect(state)}
  end

  # Updates references for every object with the given function
  defp update_references(state, fun) do
    update_in(state.objects, fn objects ->
      for {object, %{references: references} = info} <- objects, into: %{} do
        {object, %{info | references: fun.(references)}}
      end
    end)
  end

  defp garbage_collect(state) do
    {to_release, objects} = Enum.split_with(state.objects, &match?({_, %{references: []}}, &1))

    for {_, %{monitors: monitors}} <- to_release, {dest, payload} <- monitors do
      send(dest, payload)
    end

    %{state | objects: Map.new(objects)}
  end
end
