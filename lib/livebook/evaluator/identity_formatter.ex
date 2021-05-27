defmodule Livebook.Evaluator.IdentityFormatter do
  @moduledoc false

  # The default formatter leaving the output unchanged.

  @behaviour Livebook.Evaluator.Formatter

  @impl true
  def format_output(output), do: output

  @impl true
  def format_response(evaluation_response), do: evaluation_response
end
