defmodule LivebookWeb.Helpers do
  use Phoenix.Component

  use LivebookWeb, :verified_routes

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

  # TODO
  def input_views_for_output(output, data, changed_input_ids) do
    input_ids = for attrs <- Cell.find_inputs_in_output(output), do: attrs.id

    data.input_infos
    |> Map.take(input_ids)
    |> Map.new(fn {input_id, %{value: value}} ->
      {input_id, %{value: value, changed: MapSet.member?(changed_input_ids, input_id)}}
    end)
  end

  # TODO
  def visible_outputs(notebook) do
    for section <- Enum.reverse(notebook.sections),
        cell <- Enum.reverse(section.cells),
        Cell.evaluable?(cell),
        output <- filter_outputs(cell.outputs, notebook.app_settings.output_type),
        do: {cell.id, output}
  end

  defp filter_outputs(outputs, :all), do: outputs
  defp filter_outputs(outputs, :rich), do: rich_outputs(outputs)

  defp rich_outputs(outputs) do
    for output <- outputs, output = filter_output(output), do: output
  end

  defp filter_output({idx, output})
       when elem(output, 0) in [:plain_text, :markdown, :image, :js, :control, :input],
       do: {idx, output}

  defp filter_output({idx, {:tabs, outputs, metadata}}) do
    outputs_with_labels =
      for {output, label} <- Enum.zip(outputs, metadata.labels),
          output = filter_output(output),
          do: {output, label}

    {outputs, labels} = Enum.unzip(outputs_with_labels)

    {idx, {:tabs, outputs, %{metadata | labels: labels}}}
  end

  defp filter_output({idx, {:grid, outputs, metadata}}) do
    outputs = rich_outputs(outputs)

    if outputs != [] do
      {idx, {:grid, outputs, metadata}}
    end
  end

  defp filter_output({idx, {:frame, outputs, metadata}}) do
    outputs = rich_outputs(outputs)
    {idx, {:frame, outputs, metadata}}
  end

  defp filter_output({idx, {:error, _message, {:interrupt, _, _}} = output}),
    do: {idx, output}

  defp filter_output(_output), do: nil
end
