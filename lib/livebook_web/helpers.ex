defmodule LivebookWeb.Helpers do
  use Phoenix.Component

  use LivebookWeb, :verified_routes

  alias Livebook.Notebook
  alias Livebook.Notebook.Cell

  @doc """
  Determines user platform based on the given *User-Agent* header.
  """
  @spec platform_from_user_agent(String.t()) :: :linux | :mac | :windows | :other
  def platform_from_user_agent(user_agent) when is_binary(user_agent) do
    cond do
      linux?(user_agent) -> :linux
      mac?(user_agent) -> :mac
      windows?(user_agent) -> :windows
      true -> :other
    end
  end

  defp linux?(user_agent), do: String.match?(user_agent, ~r/Linux/)
  defp mac?(user_agent), do: String.match?(user_agent, ~r/Mac OS X/)
  defp windows?(user_agent), do: String.match?(user_agent, ~r/Windows/)

  @doc """
  Returns path to specific process dialog within LiveDashboard.
  """
  def live_dashboard_process_path(pid) do
    pid_str = Phoenix.LiveDashboard.PageBuilder.encode_pid(pid)
    ~p"/dashboard/#{node()}/processes?info=#{pid_str}"
  end

  @doc """
  Converts human-readable strings to strings which can be used
  as HTML element IDs (compatible with HTML5)

  At the same time duplicate IDs are enumerated to avoid duplicates
  """
  @spec names_to_html_ids(list(String.t())) :: list(String.t())
  def names_to_html_ids(names) do
    names
    |> Enum.map(&name_to_html_id/1)
    |> Enum.map_reduce(%{}, fn html_id, counts ->
      counts = Map.update(counts, html_id, 1, &(&1 + 1))

      case counts[html_id] do
        1 -> {html_id, counts}
        count -> {"#{html_id}-#{count}", counts}
      end
    end)
    |> elem(0)
  end

  defp name_to_html_id(name) do
    name
    |> String.trim()
    |> String.downcase()
    |> String.replace(~r/\s+/u, "-")
  end

  defdelegate ansi_string_to_html(string), to: LivebookWeb.Helpers.ANSI
  defdelegate ansi_string_to_html_lines(string), to: LivebookWeb.Helpers.ANSI

  @doc """
  Returns the text in singular or plural depending on the quantity

  ## Examples

      iex> LivebookWeb.Helpers.pluralize(1, "notebook is not persisted", "notebooks are not persisted")
      "1 notebook is not persisted"

      iex> LivebookWeb.Helpers.pluralize(3, "notebook is not persisted", "notebooks are not persisted")
      "3 notebooks are not persisted"

  """
  @spec pluralize(non_neg_integer(), String.t(), String.t()) :: String.t()
  def pluralize(1, singular, _plural), do: "1 #{singular}"
  def pluralize(count, _singular, plural), do: "#{count} #{plural}"

  @doc """
  Formats the given UTC datetime relatively to present.
  """
  @spec format_datetime_relatively(DateTime.t()) :: String.t()
  def format_datetime_relatively(date) do
    date |> DateTime.to_naive() |> Livebook.Utils.Time.time_ago_in_words()
  end

  @doc """
  TODO
  """
  @spec canvas_outputs(Notebook.t()) :: list({Cell.id(), map()})
  def canvas_outputs(notebook) do
    for {cell, _section} <- Notebook.cells_with_section(notebook),
        Cell.evaluable?(cell),
        cell.output_location != nil,
        into: %{} do
      {cell.id, cell.output_location}
    end
  end
end
