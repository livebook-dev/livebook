defmodule Livebook.Hub.SettingsTest do
  use ExUnit.Case

  alias Livebook.Hub.{Machine, Settings}

  @machine_id Livebook.Utils.random_id()

  test "fetch_machines/0 returns a list of persisted machines" do
    clean_machines()

    Settings.save_machine(machine())

    assert [
             %{
               color: "#FF00FF",
               id: @machine_id,
               name: "Foo - bar",
               token: "foo",
               hub: "fly"
             }
           ] == Settings.fetch_machines()

    Livebook.Storage.current().delete(:hub, @machine_id)
    assert [] == Settings.fetch_machines()
  end

  test "machine_by_id!/1 returns one persisted machine" do
    clean_machines()

    assert_raise Livebook.Hub.Settings.NotFoundError,
                 ~s/could not find a machine matching "#{@machine_id}"/,
                 fn ->
                   Settings.machine_by_id!(@machine_id)
                 end

    Settings.save_machine(machine())

    assert machine() == Settings.machine_by_id!(@machine_id)
  end

  test "machine_exists?/1" do
    clean_machines()
    refute Settings.machine_exists?(machine())

    Settings.save_machine(machine())
    assert Settings.machine_exists?(machine())
  end

  defp clean_machines do
    for %{id: machine_id} <- Livebook.Hub.Settings.fetch_machines() do
      Livebook.Storage.current().delete(:hub, machine_id)
    end
  end

  defp machine do
    %Machine{
      id: @machine_id,
      hub: "fly",
      name: "Foo - bar",
      token: "foo",
      color: "#FF00FF"
    }
  end
end
