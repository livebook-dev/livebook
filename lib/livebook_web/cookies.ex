defmodule LivebookWeb.Cookies do
  def authenticate(_conn, user_data) do
    user_data
  end

  def child_spec(_opts) do
    %{id: __MODULE__, start: {Function, :identity, [:ignore]}}
  end
end
