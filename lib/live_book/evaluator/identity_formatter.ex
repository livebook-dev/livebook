defmodule LiveBook.Evaluator.IdentityFormatter do
  @moduledoc false

  # The default formatter leaving the response unchanged.

  @behaviour LiveBook.Evaluator.Formatter

  @impl true
  def format(evaluation_response), do: evaluation_response
end
