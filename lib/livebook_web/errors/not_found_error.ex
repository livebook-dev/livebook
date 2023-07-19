defmodule LivebookWeb.NotFoundError do
  @moduledoc false
  defexception [:message, plug_status: 404]
end
