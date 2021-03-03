defmodule Livebook.Evaluator.IdentityFormatter do
  @moduledoc false

  # The default formatter leaving the response unchanged.

  @behaviour Livebook.Evaluator.Formatter

  @impl true
  def format(evaluation_response), do: evaluation_response
end
