defmodule Livebook.Copilot.DummyBackend do
  # :model determines generation options, prompt format and post processing
  # def completion(%{model: "echo"}, pre, suf) do
  #   return "\nEcho!\n#{pre}<fill-me>#{suf}"
  # end
  def completion(_, pre, suf) do
    Process.sleep(500)
    "\nEcho!\n#{pre}<fill-me>#{suf}"
  end

  def model_loaded?(_), do: true
  def load_model!(_), do: true
end
