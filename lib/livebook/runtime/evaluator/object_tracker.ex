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

  require Logger

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

  This function synchronously awaits all monitoring acknowledgements
  in case the object is released.
  """
  @spec remove_reference_sync(pid(), object_reference()) :: :ok
  def remove_reference_sync(object_tracker, reference) do
    GenServer.call(object_tracker, {:remove_reference_sync, reference}, :infinity)
  end

  @doc """
  Schedules `payload` to be send to `destination` when the object
  is released.
  """
  @spec monitor(pid(), object(), Process.dest(), term(), boolean()) :: :ok | {:error, :bad_object}
  def monitor(object_tracker, object, destination, payload, ack?) do
    GenServer.call(object_tracker, {:monitor, object, destination, payload, ack?})
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

  @impl true
  def handle_call({:remove_reference_sync, reference}, _from, state) do
    state = update_references(state, fn references -> List.delete(references, reference) end)

    {:reply, :ok, garbage_collect(state, true)}
  end

  def handle_call({:monitor, object, destination, payload, ack?}, _from, state) do
    monitor = {destination, payload, ack?}

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

  def handle_info({:monitor_ack, _}, state) do
    {:noreply, state}
  end

  # Updates references for every object with the given function
  defp update_references(state, fun) do
    update_in(state.objects, fn objects ->
      for {object, %{references: references} = info} <- objects, into: %{} do
        {object, %{info | references: fun.(references)}}
      end
    end)
  end

  defp garbage_collect(state, sync \\ false) do
    {to_release, objects} = Enum.split_with(state.objects, &match?({_, %{references: []}}, &1))

    monitors = for {_, %{monitors: monitors}} <- to_release, monitor <- monitors, do: monitor

    for {dest, payload, _ack? = false} <- monitors, do: send(dest, payload)

    if sync do
      ack_refs =
        for {dest, payload, _ack? = true} <- monitors do
          ack_ref = Process.monitor(dest)
          send(dest, {payload, self(), {:monitor_ack, ack_ref}})
          ack_ref
        end

      for ack_ref <- ack_refs do
        receive do
          {:monitor_ack, ^ack_ref} ->
            Process.demonitor(ack_ref, [:flush])

          {:DOWN, ^ack_ref, :process, _pid, _reason} ->
            :ok
        after
          8_000 ->
            Logger.warning(
              "expected a monitoring acknowledgement, but none was received within 8 seconds"
            )
        end
      end
    else
      for {dest, payload, true} <- monitors do
        send(dest, {payload, self(), {:monitor_ack, :ignored}})
      end
    end

    %{state | objects: Map.new(objects)}
  end
end
