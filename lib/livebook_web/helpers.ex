defmodule LivebookWeb.Helpers do
  use Phoenix.Component

  use LivebookWeb, :verified_routes

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
  Returns a list of human readable messages for all upload and upload
  entry errors.
  """
  @spec upload_error_messages(Phoenix.LiveView.UploadConfig.t()) :: list(String.t())
  def upload_error_messages(upload) do
    errors = upload_errors(upload) ++ Enum.flat_map(upload.entries, &upload_errors(upload, &1))
    Enum.map(errors, &upload_error_to_string/1)
  end

  @doc """
  Converts an upload or entry error to string.
  """
  @spec upload_error_to_string(term()) :: String.t()
  def upload_error_to_string(:too_large), do: "Too large"
  def upload_error_to_string(:too_many_files), do: "You have selected too many files"
  def upload_error_to_string(:not_accepted), do: "You have selected an unacceptable file type"

  def upload_error_to_string({:writer_failure, message}) when is_binary(message) do
    Livebook.Utils.upcase_first(message)
  end
end
