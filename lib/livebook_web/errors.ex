defmodule LivebookWeb.NotFoundError do
  defexception [:message, plug_status: 404]
end

defmodule LivebookWeb.BadRequestError do
  defexception [:message, plug_status: 400]
end
