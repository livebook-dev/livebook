defmodule LivebookWeb.PolicyHook do
  import Phoenix.Component

  def on_mount(:private, _params, _session, socket) do
    {:cont, socket |> assign(:policy, %{read: true, execute: true, edit: true})}
  end

  def on_mount(:shared, _params, _session, socket) do
    {:cont, socket |> assign(:policy, %{read: true, execute: false, comment: false, edit: false})}
  end
end
