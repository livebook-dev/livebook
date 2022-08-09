defmodule Livebook.Hub.SettingsTest do
  use ExUnit.Case

  alias Livebook.Hub.{Machine, Settings}

  @machine_id Livebook.Utils.random_id()

  test "fetch_machines/0 returns a list of persisted machines" do
    for %{id: machine_id} <- Livebook.Hub.Settings.fetch_machines() do
      Livebook.Storage.current().delete(:hub, machine_id)
    end

    machine = %Machine{id: @machine_id, name: "Foo - bar", token: "foo", color: "#FF00FF"}
    Settings.save_machine(machine)

    assert [
             %{
               color: "#FF00FF",
               id: @machine_id,
               name: "Foo - bar",
               token: "foo"
             }
           ] == Settings.fetch_machines()

    Livebook.Storage.current().delete(:hub, @machine_id)
    assert [] == Settings.fetch_machines()
  end
end
