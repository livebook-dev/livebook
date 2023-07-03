defmodule LivebookWeb.SessionIdentity do
  @moduledoc false
  # This module implements the ZTA contract specific to Livebook cookies

  import Plug.Conn

  def authenticate(_, conn, _) do
    if id = get_session(conn, :current_user_id) do
      {conn, %{id: id}}
    else
      user_id = Livebook.Utils.random_id()
      {put_session(conn, :current_user_id, user_id), %{id: user_id}}
    end
  end

  def child_spec(_opts) do
    %{id: __MODULE__, start: {Function, :identity, [:ignore]}}
  end
end
